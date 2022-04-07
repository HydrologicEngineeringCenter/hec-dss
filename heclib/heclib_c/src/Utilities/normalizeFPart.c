#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>
#include <normalizeFPart.h>
#include <heclib.h>

#ifdef _MSC_VER
#define strtok_r strtok_s
#define strdup _strdup
#endif

typedef struct TagValue_s {
	char  tag;
	char* value;
} TagValue;

#define JAN  1
#define FEB  2
#define MAR  3
#define APR  4
#define MAY  5
#define JUN  6
#define JUL  7
#define AUG  8
#define SEP  9
#define OCT 10
#define NOV 11
#define DEC 12
#define IS_LEAPYEAR(yr) (yr % 4 == 0 && (yr % 100 != 0 || yr % 400 == 0))

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
	case JAN:
	case MAR:
	case MAY:
	case JUL:
	case AUG:
	case OCT:
	case DEC:
		if (day > 31) return false;
		break;
	case APR:
	case JUN:
	case SEP:
	case NOV:
		if (day > 30) return false;
		break;
	case FEB:
		if (IS_LEAPYEAR(yr)) {
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

#undef JAN
#undef FEB
#undef MAR
#undef APR
#undef MAY
#undef JUN
#undef JUL
#undef AUG
#undef SEP
#undef OCT
#undef NOV
#undef DEC
#undef IS_LEAPYEAR

int validateFPartTag(const char tag, const char* value) {
	switch (tag) {
	case 'C' :
		// Collection (6 alphanumeric chars)
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
		// Run Name (no constraints)
		return true;
	case 'V':
		// Version time (YYYYMMDD-hhmmss)
		if (!validateTagTime(value, true)) return false;
		return true;
	case 'R':
		// Run ID (multiples of '--' or alphanum char followed by digit char)
		if (strlen(value) % 2) return false;
		for (int i = 0; i < strlen(value); i += 2) {
			if (value[i] == '-') {
				if (value[i + 1] != '-') return false;
			}
			else {
				if (!isalnum(value[i])) return false;
				if (!isdigit(value[i + 1])) return false; // allow for trials to come back, otherwise should always be '0'
			}
		}
		return true;
	default :
		// ERROR Invalid tag
		return false;
	}
}

int canUseOriginalFPart(const int errorCode) {
	return errorCode < normalizeFPartStatus.MEMORY_ERROR;
}

const char* normalizeFPartErrorMessage(const int errorCode) {
	// unable to use struct members in switch/case construct
	/*  0 */ if (errorCode == normalizeFPartStatus.SUCCESS)                 return "Success";
	/*  1 */ if (errorCode == normalizeFPartStatus.NO_TAG_SEPARATOR)        return "No ':' character found";
	/*  2 */ if (errorCode == normalizeFPartStatus.INVALID_TAG_FORMAT)      return "':' character found but not in a valid tag format";
	/*  3 */ if (errorCode == normalizeFPartStatus.MULTIPLE_UNTAGGED_PARTS) return "'|' character followed by untagged text found more than once";
	/*  4 */ if (errorCode == normalizeFPartStatus.EMPTY_F_PART)            return "Empty F-part";
	/* 10 */ if (errorCode == normalizeFPartStatus.MEMORY_ERROR)            return "Error allocating memory";
	/* 11 */ if (errorCode == normalizeFPartStatus.INVALID_TAG_CHARACTER)   return "Tag character not in list of valid tag characters";
	/* 12 */ if (errorCode == normalizeFPartStatus.REPEATED_TAG_CHARACTER)  return "Valid tag character found more than once";
	/* 13 */ if (errorCode == normalizeFPartStatus.INVALID_C_TAG_VALUE)     return "Tag value doesn't match expected format for Collection";
	/* 14 */ if (errorCode == normalizeFPartStatus.INVALID_T_TAG_VALUE)     return "Tag value doesn't match expected format or invalid date/time for Time of Forecast";
	/* 15 */ if (errorCode == normalizeFPartStatus.INVALID_V_TAG_VALUE)     return "Tag value doesn't match expected format or invalid date/time for Version Time";
	/* 16 */ if (errorCode == normalizeFPartStatus.INVALID_R_TAG_VALUE)     return "Tag value doesn't match expected format for Run ID";
	return "Unexpected error";
}

int normalizeFPart(char** normalized, const char* fPart) {
	const char* TAG_DELIMITER = "|";
	const char* TAG_SEPARATOR = ":";
	const char* TAG_CHARACTERS = "CNTRV"; // determines canonical (normalized) order of tags
	int status = normalizeFPartStatus.SUCCESS;

	assert(strlen(TAG_DELIMITER) == 1);
	assert(strlen(TAG_SEPARATOR) == 1);
	*normalized = NULL;
	if (fPart && strlen(fPart) > 0) {
		TagValue* tagValues = NULL;
		int       tagCount = 0;
		char*     untaggedPortion = NULL;
		char*     buf = NULL;
		do {
			//------------------//
			// parse the F Part //
			//------------------//
			if (!strstr(fPart, TAG_SEPARATOR)) {
				status = normalizeFPartStatus.NO_TAG_SEPARATOR;
				break;
			}
			size_t len = strlen(fPart);
			buf = strdup(fPart);
			if (buf == NULL) {
				status = normalizeFPartStatus.MEMORY_ERROR;
				break;
			}
			char* saveptr = NULL;
			char* cp1 = strtok_r(buf, TAG_DELIMITER, &saveptr);
			while (cp1) {
				char* cp2 = strstr(cp1, TAG_SEPARATOR);
				if (cp2) {
					if (cp2 - cp1 != 1) {
						status = normalizeFPartStatus.INVALID_TAG_FORMAT;
						break;
					}
					char c = toupper(*cp1);
					if (!strchr(TAG_CHARACTERS, c)) {
						status = normalizeFPartStatus.INVALID_TAG_CHARACTER;
						break;
					}
					for (int i = 0; i < tagCount; ++i) {
						if (tagValues[i].tag == c) {
							status = normalizeFPartStatus.REPEATED_TAG_CHARACTER;
							break;
						}
					}
					if (status != normalizeFPartStatus.SUCCESS) break;
					void* vp = realloc(tagValues, ++tagCount * sizeof(TagValue));
					if (vp == NULL) {
						status = normalizeFPartStatus.MEMORY_ERROR;
						--tagCount;
						break;
					}
					tagValues = (TagValue*)vp;
					tagValues[tagCount - 1].tag = c;
					tagValues[tagCount - 1].value = strdup(cp2 + 1);
					if (!tagValues[tagCount - 1].value) {
						status = normalizeFPartStatus.MEMORY_ERROR;
						break;
					}
					if (!validateFPartTag(tagValues[tagCount - 1].tag, tagValues[tagCount - 1].value)) {
						switch (c) {
						case 'C': return normalizeFPartStatus.INVALID_C_TAG_VALUE;
						case 'T': return normalizeFPartStatus.INVALID_T_TAG_VALUE;
						case 'V': return normalizeFPartStatus.INVALID_V_TAG_VALUE;
						case 'R': return normalizeFPartStatus.INVALID_R_TAG_VALUE;
						default: return normalizeFPartStatus.UNEXPECTED_ERROR;
						}
					}
				}
				else {
					if (untaggedPortion) {
						status = normalizeFPartStatus.MULTIPLE_UNTAGGED_PARTS;
						break;
					}
					untaggedPortion = strdup(cp1);
					if (untaggedPortion == NULL) {
						status = normalizeFPartStatus.MEMORY_ERROR;
						break;
					}
				}
				cp1 = strtok_r(NULL, TAG_DELIMITER, &saveptr);
			}
			if (fPart[strlen(fPart) - 1] == TAG_DELIMITER[0]) {
				// this will be hidden by strtok
				if (untaggedPortion) {
					status = normalizeFPartStatus.MULTIPLE_UNTAGGED_PARTS;
				}
			}
			if (status != normalizeFPartStatus.SUCCESS) break;
			//-----------------------------//
			// build the normalized F Part //
			//-----------------------------//
			if (tagCount > 0 || untaggedPortion != NULL) {
				for (int i = 0; i < strlen(TAG_CHARACTERS) && status == normalizeFPartStatus.SUCCESS; ++i) {
					for (int j = 0; j < tagCount && status == normalizeFPartStatus.SUCCESS; ++j) {
						if (tagValues[j].tag == TAG_CHARACTERS[i]) {
							size_t currentLen = *normalized == NULL ? 0 : strlen(*normalized);
							size_t extraLen = strlen(tagValues[j].value) + 4;
							cp1 = (char*)realloc(*normalized, currentLen + extraLen);
							if (cp1 == NULL) {
								status = normalizeFPartStatus.MEMORY_ERROR;
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
						status = normalizeFPartStatus.MEMORY_ERROR;
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
				free(tagValues[i].value);
			}
			free(tagValues);
		}
	}
	else {
		status = normalizeFPartStatus.EMPTY_F_PART;
	}
	return status;
}

int normalizeFPartInPathname(char** normalized, const char* pathname) {
	*normalized = NULL;
	char pathnameParts[6][MAX_PART_SIZE];
	for (int i = 0; i < 6; ++i) {
		zpathnameGetPart(pathname, i + 1, pathnameParts[i], MAX_PART_SIZE);
	}
	char* normalizedFPart = NULL;
	int status = normalizeFPart(&normalizedFPart, pathnameParts[5]);
	if (status == normalizeFPartStatus.SUCCESS) {
		size_t len = 0;
		for (int i = 0; i < 5; ++i) {
			len += strlen(pathnameParts[i]);
		}
		len += strlen(normalizedFPart) + 8; // 7 slashes + null
		char* buf = (char*)malloc(len);
		if (buf == NULL) {
			return normalizeFPartStatus.MEMORY_ERROR;
		}
		*normalized = buf;
		sprintf(
			*normalized,
			"/%s/%s/%s/%s/%s/%s/",
			pathnameParts[0],
			pathnameParts[1],
			pathnameParts[2],
			pathnameParts[3],
			pathnameParts[4],
			normalizedFPart);
		free(normalizedFPart);
		return normalizeFPartStatus.SUCCESS;
	}
	else {
		if (canUseOriginalFPart(status)) {
			*normalized = strdup(pathname);
			if (*normalized == NULL) {
				return normalizeFPartStatus.MEMORY_ERROR;
			}
			return normalizeFPartStatus.SUCCESS;
		}
		return status;
	}
}
