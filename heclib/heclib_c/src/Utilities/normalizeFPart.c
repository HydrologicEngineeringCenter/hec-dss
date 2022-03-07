#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <hecdssInternal.h>

#ifdef _MSC_VER
#define strtok_r strtok_s
#endif

typedef struct TagValue_s {
	char  tag;
	char* value;
} TagValue;

int validateTagTime(const char* value, int includeSeconds) {
	int yr, mon, day, hr, min, sec;
	int validLength = includeSeconds ? 15 : 13;
	if (strlen(value) != validLength) return false;
	for (int i = 0; i < validLength; ++i) {
		switch (i) {
		case 8:
			if (value[i] != '-') return false;
			break;
		default:
			if (!isdigit(value[i])) return false;
		}
	}
	yr  = value[ 3] - '0' + 10 * (value[ 2] - '0') + 100 * (value[1] - '0') + 1000 * (value[0] - '0');
	mon = value[ 5] - '0' + 10 * (value[ 4] - '0');
	day = value[ 7] - '0' + 10 * (value[ 6] - '0');
	hr  = value[10] - '0' + 10 * (value[ 9] - '0');
	min = value[12] - '0' + 10 * (value[11] - '0');
	if (includeSeconds) {
		sec = value[14] - '0' + 10 * (value[13] - '0');
	}
	switch (mon) {
	case 1:
	case 3:
	case 5:
	case 7:
	case 8:
	case 10:
	case 12:
		if (day > 31) return false;
		break;
	case 4:
	case 6:
	case 9:
	case 11:
		if (day > 30) return false;
		break;
	case 2:
		if (yr % 4 == 0 && (yr % 100 != 0 || yr % 400 == 0)) {
			if (day > 29) return false;
		}
		else {
			if (day > 28) return false;
		}
		break;
	default:
		return false;
	}
	if (hr > 24 || (hr == 24 && min > 0)) return false;
	if (min > 59) return false;
	if (includeSeconds) {
		if (sec > 59) return false;
	}
	return true;
}
int validateFPartTag(const char tag, const char* value) {
	switch (tag) {
	case 'C' :
		// Collection number
		if (strlen(value) != 6) return false;
		for (int i = 0; i < 6; ++i) {
			if (!isalnum(value[i])) return false;
		}
		return true;
	case 'T':
		// Time of forecast (YYYYMMDD-hhmm)
		if (!validateTagTime(value, false)) return false;
		return true;
	case 'N':
		// Run Name
		return true;
	case 'V':
		// Version time (YYYYMMDD-hhmmss)
		if (!validateTagTime(value, true)) return false;
		return true;
	case 'R':
		// Run ID
		if (strlen(value) % 2) return false;
		for (int i = 0; i < strlen(value); i += 2) {
			if (value[i] == '-') {
				if (value[i + 1] != '-') return false;
			}
			else {
				if (!isalpha(value[i])) return false;
				if (!isdigit(value[i + 1])) return false; // allow for trials to come back, otherwise should always be '0'
			}
		}
		return true;
	default :
		// ERROR Invalid tag
		return false;
	}
}

// < 10 : indicates normal F-part with possible ':' and/or '|' characters not intended for tagging
// = 10 : indicates nothing about the intent of the F-part
// > 10 : indicates a failed attempt at a tagged F-part
#define SUCCESS                  0
#define NO_TAG_SEPARATOR         1 // no ':' found
#define INVALID_TAG_FORMAT       2 // ':' found but not in a valid tag format
#define MULTIPLE_UNTAGGED_PARTS  3 // '|' followed by untagged text found more than once
#define EMPTY_F_PART             4 // NULL F-Part
#define MEMORY_ERROR            10 // error allocating memory
#define INVALID_TAG_CHARACTER   11 // tag character not in list of valid tag characters
#define REPEATED_TAG_CHARACTER  12 // valid tag character found more than once
#define INVALID_C_TAG_VALUE     13 // tag value doesn't match expected format for Collection
#define INVALID_T_TAG_VALUE     14 // tag value doesn't match expected format for Time of Forecast
#define INVALID_V_TAG_VALUE     15 // tag value doesn't match expected format for Version Time
#define INVALID_R_TAG_VALUE     16 // tag value doesn't match expected format for Run ID
#define UNEXPECTED_ERROR       100 // Something unexpected went wrong!
/**
 * Takes a DSS F Part string and
 * <ul>
 * <li>Ensures only expected tags (C:, T:, V:, N:,and R:) are included</li>
 * <li>Verifies the data for any included tags are correct</li>
 * <li>Assembles the resulting F Part with the included tags in canonical order</li>
 * </ul>
 * @param normalized The normalized string. NULL on error. Otherwise an allocated string even if identical to input string.
 *                   You MUST free it yourself.
 * @param fPart      The F Part string to normalize.
 *
 * @return
 * <dl>
 * <dt>  0 </dt><dd> success                                                      </dd>
 * <dt>  1 </dt><dd> no ':' found                                                 </dd>
 * <dt>  2 </dt><dd> ':' found but not in a valid tag format                      </dd>
 * <dt>  3 </dt><dd> '|' followed by untagged text found more than once           </dd>
 * <dt>  4 </dt><dd> NULL F-part                                                  </dd>
 * <dt> 10 </dt><dd> error allocating memory                                      </dd>
 * <dt> 11 </dt><dd> tag character not in list of valid tag characters            </dd>
 * <dt> 12 </dt><dd> valid tag character found more than once                     </dd>
 * <dt> 13 </dt><dd> tag value doesn't match expected format for Collection       </dd>
 * <dt> 14 </dt><dd> tag value doesn't match expected format for Time of Forecast </dd>
 * <dt> 15 </dt><dd> tag value doesn't match expected format for Version Time     </dd>
 * <dt> 16 </dt><dd> tag value doesn't match expected format for Run ID           </dd>
 * </dl>
 */
int normalizeFPart(char** normalized, const char* fPart) {
	const char* TAG_DELIMITER = "|";
	const char* TAG_SEPARATOR = ":";
	const char* TAG_CHARACTERS = "CTNVR"; // determines canonical (normalized) order of tags
	int status = SUCCESS;

	assert(strlen(TAG_DELIMITER) == 1);
	assert(strlen(TAG_SEPARATOR) == 1);
	*normalized = NULL;
	if (fPart) {
		TagValue* tagValues = NULL;
		int       tagCount = 0;
		char*     untaggedPortion = NULL;
		char*     buf = NULL;
		do {
			//------------------//
			// parse the F Part //
			//------------------//
			if (!strstr(fPart, TAG_SEPARATOR)) {
				status = NO_TAG_SEPARATOR;
				break;
			}
			int len = strlen(fPart);
			buf = strdup(fPart);
			if (buf == NULL) {
				status = MEMORY_ERROR;
				break;
			}
			char* saveptr = NULL;
			char* cp1 = strtok_r(buf, TAG_DELIMITER, &saveptr);
			while (cp1) {
				char* cp2 = strstr(cp1, TAG_SEPARATOR);
				if (cp2) {
					if (cp2 - cp1 != 1) {
						status = INVALID_TAG_FORMAT;
						break;
					}
					char c = toupper(*cp1);
					if (!strchr(TAG_CHARACTERS, c)) {
						status = INVALID_TAG_CHARACTER;
						break;
					}
					for (int i = 0; i < tagCount; ++i) {
						if (tagValues[i].tag == c) {
							status = REPEATED_TAG_CHARACTER;
							break;
						}
					}
					if (status != SUCCESS) break;
					void* vp = realloc(tagValues, ++tagCount * sizeof(TagValue));
					if (vp == NULL) {
						status = MEMORY_ERROR;
						--tagCount;
						break;
					}
					tagValues = (TagValue*)vp;
					tagValues[tagCount - 1].tag = c;
					tagValues[tagCount - 1].value = strdup(cp2 + 1);
					if (!tagValues[tagCount - 1].value) {
						status = MEMORY_ERROR;
						break;
					}
					if (!validateFPartTag(tagValues[tagCount - 1].tag, tagValues[tagCount - 1].value)) {
						switch (c) {
						case 'C': return INVALID_C_TAG_VALUE;
						case 'T': return INVALID_T_TAG_VALUE;
						case 'V': return INVALID_V_TAG_VALUE;
						case 'R': return INVALID_R_TAG_VALUE;
						default: return UNEXPECTED_ERROR;
						}
						break;
					}
				}
				else {
					if (untaggedPortion) {
						status = MULTIPLE_UNTAGGED_PARTS;
						break;
					}
					untaggedPortion = strdup(cp1);
					if (untaggedPortion == NULL) {
						status = MEMORY_ERROR;
						break;
					}
				}
				cp1 = strtok_r(NULL, TAG_DELIMITER, &saveptr);
			}
			if (fPart[strlen(fPart) - 1] == TAG_DELIMITER[0]) {
				// this will be hidden by strtok
				if (untaggedPortion) {
					status = MULTIPLE_UNTAGGED_PARTS;
				}
			}
			if (status != SUCCESS) break;
			//-----------------------------//
			// build the normalized F Part //
			//-----------------------------//
			if (tagCount > 0 || untaggedPortion != NULL) {
				for (int i = 0; i < strlen(TAG_CHARACTERS) && status == SUCCESS; ++i) {
					for (int j = 0; j < tagCount && status == SUCCESS; ++j) {
						if (tagValues[j].tag == TAG_CHARACTERS[i]) {
							size_t currentLen = *normalized == NULL ? 0 : strlen(*normalized);
							size_t extraLen = strlen(tagValues[j].value) + 4;
							cp1 = (char*)realloc(*normalized, currentLen + extraLen);
							if (cp1 == NULL) {
								status = MEMORY_ERROR;
								free(*normalized);
								*normalized = NULL;
								break;
							}
							*normalized = cp1;
							memset(*normalized + currentLen, 0, extraLen);
							(*normalized)[currentLen] = tagValues[j].tag;
							(*normalized)[currentLen + 1] = TAG_SEPARATOR[0];
							strcat(*normalized, tagValues[j].value);
							(*normalized)[strlen(*normalized)] = TAG_DELIMITER[0];
						}
					}
				}
				if (untaggedPortion) {
					size_t currentLen = strlen(*normalized);
					size_t extraLen = strlen(untaggedPortion) + 1;
					cp1 = (char*)realloc(*normalized, currentLen + extraLen);
					if (cp1 == NULL) {
						status = MEMORY_ERROR;
						free(*normalized);
						*normalized = NULL;
						break;
					}
					*normalized = cp1;
					memset(*normalized + currentLen, 0, extraLen);
					strcat(*normalized, untaggedPortion);
				}
			}
		} while (false);
		//-----------------------------//
		// clean up memory allocations //
		//-----------------------------//
		if (buf) free(buf);
		if (untaggedPortion) free(untaggedPortion);
		if (tagValues) {
			for (int i = 0; i < tagCount; ++i) {
				if (tagValues[i].value != NULL) {
					free(tagValues[i].value);
				}
			}
			free(tagValues);
		}
	}
	else {
		status = EMPTY_F_PART;
	}
	return status;
}