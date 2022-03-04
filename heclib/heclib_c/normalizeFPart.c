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
			if (isdigit(value[i])) return false;
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
	case 'N':
		// Run Name
		return true;
	default :
		// ERROR Invalid tag
		return false;
	}
}

/**
 * Takes a DSS F Part string and 
 * <ul>
 * <li>Ensures only expected tags (C:, T:, V:, R:,and N:) are included</li>
 * <li>Verifies the data for any included tags are correct</li>
 * <li>Assembles the resulting F Part with the included tags in canonical order</li>
 * </ul>
 * @param fPart The F Part string to normalize.
 * @return The normalized string. NULL indicates an error. Otherwise an allocated string even if identical to input string.
 *         You MUST free it yourself.
 */
char* normalizeFPart(const char* fPart) {
	const char const* TAG_DELIMITER = "|";
	const char const* TAG_SEPARATOR = ":";
	const char TAG_CHARACTERS[] = {"CTVRN"};

	char* normalized = NULL;

	if (fPart) {
		TagValue* tagValues = NULL;
		int       tagCount = 0;
		char*     untaggedPortion = NULL;
		char*     buf = NULL;
		int       error = false;
		do {
			int len = strlen(fPart);
			buf = strdup(fPart);
			if (buf == NULL) {
				// ERROR memory error
				break;
			}
			char* saveptr;
			char* cp1 = strtok_r(buf, TAG_DELIMITER, saveptr);
			while (cp1) {
				char* cp2 = strstr(cp1, TAG_SEPARATOR);
				if (cp2) {
					if (cp2 - cp1 != 1) {
						// ERROR Invalid tag
						error = true;
						break;
					}
					char c = toupper(*cp1);
					for (int i = 0; i < tagCount; ++i) {
						if (tagValues[i].tag == c) {
							// ERROR tag used multiple times
							error = true;
							break;
						}
					}
					if (error) break;
					void* vp = realloc(tagValues, ++tagCount * sizeof(TagValue));
					if (vp == NULL) {
						// ERROR memory error
						--tagCount;
						error = true;
						break;
					}
					tagValues = (TagValue*)vp;
					tagValues[tagCount - 1].tag = c;
					tagValues[tagCount - 1].value = strdup(cp2 + 1);
					if (!tagValues[tagCount - 1].value) {
						// ERROR memory error
						error = true;
						break;
					}
					error = !validateFPartTag(tagValues[tagCount - 1].tag, tagValues[tagCount-1].value);
					if (error) {
						// ERROR invalid tag data
						break;
					}
				}
				else {
					if (untaggedPortion) {
						// ERROR multiple untagged portions
						error = true;
						break;
					}
					untaggedPortion = strdup(cp1);
					if (untaggedPortion == NULL) {
						// ERROR memory error
						error = true;
						break;
					}
				}
				cp1 = strtok_r(NULL, TAG_DELIMITER, saveptr);
			}
			if (error) break;
			if (tagCount > 0 || untaggedPortion != NULL) {
				for (int i = 0; i < sizeof(TAG_CHARACTERS) / sizeof(TAG_CHARACTERS[0]) && !error; ++i) {
					for (int j = 0; j < tagCount && !error; ++j) {
						int currentLen = strlen(normalized);
						int extraLen = strlen(tagValues[j].value) + 4;
						cp1 = (char*)realloc(normalized, currentLen + extraLen);
						if (cp1 == NULL) {
							// ERROR memory error
							free(normalized);
							normalized = NULL;
							error = true;
							break;
						}
						normalized = cp1;
						memset(normalized + currentLen, 0, extraLen);
						normalized[currentLen] = tagValues[j].tag;
						normalized[currentLen+1] = TAG_SEPARATOR;
						strcat(normalized, tagValues[j].value);
						normalized[strlen(normalized)] = TAG_DELIMITER;
					}
				}
				if (untaggedPortion) {
					int currentLen = strlen(normalized);
					int extraLen = strlen(untaggedPortion) + 1;
					cp1 = (char*)realloc(normalized, currentLen + extraLen);
					if (cp1 == NULL) {
						// ERROR memory error
						free(normalized);
						normalized = NULL;
						error = true;
						break;
					}
					normalized = cp1;
					memset(normalized + currentLen, 0, extraLen);
					strcat(normalized, untaggedPortion);
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
	return normalized;
}