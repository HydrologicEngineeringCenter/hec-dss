#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <zlib.h>
#include <verticalDatum.h>
#include <hecdssInternal.h>
#include <heclib.h>

//
// The 64 valid characters used in base64 encoding (excluding pad character '='), in index order
//
static const unsigned char* base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
//
// The index of valid characters. Ex: 'A' = ASCII 65; pos 65 in this table is 0 which is the index pos of 'A' in
// the valid characters. Positions for non-valid characters are all 255.
//
static const unsigned char base64bytes[] = {255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                                            255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                                            255,255,255,255,255,255,255,255,255,255,255, 62,255,255,255, 63,
                                             52, 53, 54, 55, 56, 57, 58, 59, 60, 61,255,255,255,255,255,255,
                                            255,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
                                             15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,255,255,255,255,255,
                                            255, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
                                             41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,255,255,255,255,255,
                                            255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                                            255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                                            255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                                            255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                                            255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                                            255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                                            255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                                            255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255};

// these unit aliases are from the CWMS database
const char* footUnitAliases[]  = {"FT","FEET","FOOT"};
const char* meterUnitAliases[] = {"M","METER","METERS","METRE","METRES"};
const int footUnitAliasCount = sizeof(footUnitAliases) / sizeof(footUnitAliases[0]);
const int meterUnitAliasCount = sizeof(meterUnitAliases) / sizeof(meterUnitAliases[0]);

#if !defined(__APPLE__) && !defined(__sun__)
const char* strcasestr(const char* haystack, const char* needle) {
    int   haystackLen = strlen(haystack);
    int   needleLen = strlen(needle);
    for (int haystackPos = 0; haystackPos < haystackLen - needleLen; ++haystackPos) {
        if (!strncasecmp(haystack + haystackPos, needle, needleLen)) {
            return haystack + haystackPos;
        }
    }
    return NULL;
}
#endif
/**
 * malloc with zero-initialization
 * @param size The number of bytes to allocate
 * @return     The allocated memory or NULL if unable
 */
void* mallocAndInit(size_t size) {
    void* buf = malloc(size);
    if (buf != NULL) {
        memset(buf, 0, size);
    }
    return buf;
}
/**
 * calloc with zero-initialization
 * @param num The number of items to allocate memory for
 * @size      The size of each item
 * @return    The allocated memory or NULL if unable
 */
void* callocAndInit(size_t num, size_t size) {
    return mallocAndInit(num*  size);
}
/**
 * Tests whether a value is essentially the UNDEFINED_VERTICAL_DATUM_VALUE
 * @param value The value to test
 * @return      Whether the value nearly equals UNDEFINED_VERTICAL_DATUM_VALUE
 */
int isUndefinedVertDatumValue(double value) {
    if (value == UNDEFINED_VERTICAL_DATUM_VALUE) {
        return TRUE;
    }
    double ratio = value / UNDEFINED_VERTICAL_DATUM_VALUE;
    return ratio > 0.95 && ratio < 1.05;
}
/**
 * Tests whether two doubles are essentially equal (within a specified tolerance)
 * @param d1      One of the two values
 * @param d2      One of the two values
 * @param epsilon The tolerance to use
 * @return        Whether the values are within the tolerance
 */
int areEqual(double d1, double d2, double epsilon) {
    double diff = d1 - d2;
    return (diff < epsilon) && (-diff < epsilon);
}
/**
 * Returns the string representation of a double value
 * @param d   The value
 * @param buf A buffer to receive the string representation
 * @return    A pointer to the buffer holding the string representation
 */
char* doubleToChar(double d, char* buf) {
    if (buf != NULL) {
        sprintf(buf, "%f", d);
    }
    return buf;
}
//
// Fortran wrapper for unitIsFeet
//
int unitisfeet_(const char* unit, size_t lenUnit) {
    char* cUnit = (char*)mallocAndInit(lenUnit + 1);
    F2C(unit, cUnit, lenUnit, lenUnit + 1);
    int isFeet = unitIsFeet(cUnit);
    free(cUnit);
    return isFeet ? 1 : 0;
}
//
// See verticalDatum.h for documentation
//
int unitIsFeet(const char* unit) {
    for (int i = 0; i < footUnitAliasCount; ++i) {
        if (!strcasecmp(unit, footUnitAliases[i])) {
            return TRUE;
        }
    }
    return FALSE;
}
//
// Fortran wrapper for unitIsMeters
//
int unitismeters_(const char* unit, size_t lenUnit) {
    char* cUnit = (char*)mallocAndInit(lenUnit + 1);
    F2C(unit, cUnit, lenUnit, lenUnit + 1);
    int isMeters = unitIsMeters(cUnit);
    free(cUnit);
    return isMeters ? 1 : 0;
}
//
// See verticalDatum.h for documentation
//
int unitIsMeters(const char* unit) {
    for (int i = 0; i < meterUnitAliasCount; ++i) {
        if (!strcasecmp(unit, meterUnitAliases[i])) {
            return TRUE;
        }
    }
    return FALSE;
}
//
// Fortran interface to getOffset
//
void getoffset_(
    double*     offset,
    const char* offsetUnit,
    const char* dataUnit,
    size_t      lenOffsetUnit,
    size_t      lenDataUnit) {

    char* cOffsetUnit = (char*)mallocAndInit(lenOffsetUnit + 1);
    char* cDataUnit = (char*)mallocAndInit(lenDataUnit + 1);
    F2C(offsetUnit, cOffsetUnit, lenOffsetUnit, lenOffsetUnit + 1);
    F2C(dataUnit, cDataUnit, lenDataUnit, lenDataUnit + 1);
    *offset = getOffset(*offset, cOffsetUnit, cDataUnit);
    free(cOffsetUnit);
    free(cDataUnit);
}
//
// See verticalDatum.h for documentation
//
double getOffset(double offset, const char* offsetUnit, const char* _dataUnit) {
    int dataInFeet     = 0;
    int dataInMeters   = 0;
    int offsetInFeet   = 0;
    int offsetInMeters = 0;

    if (offset == UNDEFINED_VERTICAL_DATUM_VALUE) {
        return offset;
    }
    // blank trim the data unit (shouldn't have to do this)
    char* dataUnit = (char*)mallocAndInit(strlen(_dataUnit)+1);
    strcpy(dataUnit, _dataUnit);
    for (int i = strlen(dataUnit)-1; dataUnit[i] == 32; --i) {
        dataUnit[i] = '\0';
    }
    if (offsetUnit == NULL || dataUnit == NULL) return UNDEFINED_VERTICAL_DATUM_VALUE;
    if (!strcasecmp(offsetUnit, dataUnit)) {
        free(dataUnit);
        return offset;
    }
    dataInFeet = unitIsFeet(dataUnit);
    if (!dataInFeet) {
        dataInMeters = unitIsMeters(dataUnit);
    }
    free(dataUnit);
    offsetInFeet = unitIsFeet(offsetUnit);
    if (!offsetInFeet) {
        offsetInMeters = unitIsMeters(offsetUnit);
    }
    if (dataInFeet) {
        if (offsetInFeet)   return offset;
        if (offsetInMeters) return offset / METERS_PER_FOOT;
        return UNDEFINED_VERTICAL_DATUM_VALUE;
    }
    else if (dataInMeters) {
        if (offsetInFeet)   return offset*  METERS_PER_FOOT;
        if (offsetInMeters) return offset;
        return UNDEFINED_VERTICAL_DATUM_VALUE;
    }
    else {
        return UNDEFINED_VERTICAL_DATUM_VALUE;
    }
}
//
// See verticalDatum.h for documentation
//
char* extractFromDelimitedString(
        char**      delimitedString,
        const char* parameter,
        const char* separator,
        int         matchCase,
        int         removeFromString,
        char        delimiter) {

    int len = strlen(parameter) + 1;
    if (separator) {
        len += strlen(separator);
    }
    char* param = mallocAndInit(len);
    strcpy(param, parameter);
    if (separator) {
        strcat(param, separator);
    }
    char* value = NULL;
    char* paramStart = matchCase ? (char*)strstr(*delimitedString, param) : (char*)strcasestr(*delimitedString, param);
    if (paramStart) {
        char* valueStart = paramStart + strlen(param);
        char* valueEnd;
        for (valueEnd = valueStart + 1; *valueEnd && (*valueEnd != delimiter); ++valueEnd);
        len = valueEnd - valueStart;
        value = (char*)mallocAndInit(len + 1);
        memcpy(value, valueStart, len);
        value[len] = '\0';
        if (removeFromString) {
            if (*valueEnd) {
                ++valueEnd;
                memmove(paramStart, valueEnd, strlen(valueEnd)+1);
            }
            else {
                *paramStart = '\0';
            }
        }
    }
    free(param);
    return value;
}
//
// See verticalDatum.h for documentation
//
int insertIntoDelimitedString(
        char**     delimitedString,
        int        delimitedStringSize,
        const char* parameter,
        const char* value,
        const char* separator,
        int         overwriteExisting,
        char        delimiter) {

    if (!delimitedString) {
        return -2;
    }

    char* existing = extractFromDelimitedString(
        delimitedString,
        parameter,
        separator,
        TRUE,
        FALSE,
        delimiter);
    if (existing && !overwriteExisting) {
        free(existing);
        return 0;
    }
    if (existing && overwriteExisting) {
        free(extractFromDelimitedString(delimitedString, parameter, separator, TRUE, TRUE, delimiter));
    }
	int len = strlen(*delimitedString);
    if (len > 0 && (*delimitedString)[len-1] != delimiter) {
		(*delimitedString)[len++] = delimiter;
    }
	for (const char* cp = parameter; *cp && len < delimitedStringSize; ++cp) {
		(*delimitedString)[len++] = *cp;
	}
    if (separator) {
		for (const char* cp = separator; *cp && len < delimitedStringSize; ++cp) {
			(*delimitedString)[len++] = *cp;
		}
    }
    if (value) {
		for (const char* cp = value; *cp && len < delimitedStringSize; ++cp) {
			(*delimitedString)[len++] = *cp;
		}
    }
	if (len > delimitedStringSize -2) {
        if (existing) {
            free(existing);
        }
        return -1;
	}
    (*delimitedString)[len++] = delimiter;
    (*delimitedString)[len] = '\0';
    if (existing) {
        free(existing);
    }
    return 0;
}
//
// Fortran interface for stringToUserHeader
//
void stringtouserheader_(
    const char* str,
    int*        userHeader,
    int*        userHeaderCapacity,
    int*        userHeaderNumber,
    size_t      lenStr) {

    char* cStr = (char*)malloc(lenStr + 1);
    F2C(str, cStr, lenStr, lenStr + 1);
    int* _userHeader = stringToUserHeader(cStr, userHeaderNumber);
    memcpy(userHeader, _userHeader, MIN(*userHeaderCapacity, *userHeaderNumber) * sizeof(int));
    free(cStr);
    free(_userHeader);
}
//
// See verticalDatum.h for documentation
//
int* stringToUserHeader(const char* str, int* userHeaderNumber) {
    int  numBytes = strlen(str);
	int  numInts = numBytes == 0 ? 0 : (numBytes-1) / 4 + 1;
    int* userHeader = NULL;
    if (numInts > 0) {
        userHeader = (int*)callocAndInit(numInts, 4);
		memset((char*)userHeader, 0, numInts*  4);
		memcpy((char*)userHeader, str, numBytes);
		if (bigEndian()) {
			// big endian
			uint32_t* _4bytes = (uint32_t*)userHeader;
			for (int i = 0; i < numInts; ++i) {
				BYTESWAP(*_4bytes++);
			}
		}
    }
    *userHeaderNumber = numInts;
    return userHeader;
}
//
// Fortran interface for stringToUserHeader
//
void userheadertostring_(
    char*      str,
    const int* userHeader,
    const int* userHeaderNumber,
    size_t     lenStr) {

    char* cStr = userHeaderToString(userHeader, *userHeaderNumber);
    C2F(cStr, str, lenStr);
    free(cStr);
}
//
// See verticalDatum.h for documentation
//
char* userHeaderToString(const int* userHeader, int userHeaderNumber) {
    char* str = NULL;
    if (userHeader != NULL && userHeaderNumber > 0) {
		int* buf = (int*)callocAndInit(userHeaderNumber, 4);
		memcpy(buf, userHeader, 4*  (size_t)userHeaderNumber);
		if (bigEndian()) {
			// big endian
			uint32_t* _4bytes = (uint32_t*)buf;
			for (int i = 0; i < userHeaderNumber; ++i) {
				BYTESWAP(*_4bytes++);
			}
		}
        char* start = (char*)buf;
        char* cp;
        int   len = userHeaderNumber*  4;
        for (cp = start; (cp - start) < len; ++cp) {
            if (!*cp) {
                break;
            }
        }
        while ((cp - start) > 1 == ' ' && *(cp-1)) --cp;
        len = cp - start;
		if (len > 0) {
			str = mallocAndInit((size_t)userHeaderNumber*4+1);
			memcpy(str, start, len);
			str[len] = '\0';
		}
        free(buf);
    }
    if (str == NULL) {
        str = mallocAndInit(1);
        str[0] = '\0';
    }
    return str;
}
//
// See verticalDatum.h for documentation
//
int b64EncodedLen(int toEncodeLen) {
    if (toEncodeLen == 0) return 0;
    if (toEncodeLen < 0) return -1;
    return ((toEncodeLen - 1) / 3 + 1) * 4;
}
//
// See verticalDatum.h for documentation
//
int b64DecodedLen(int toDecodeLen) {
    if (toDecodeLen < 0) return -1;
    if (toDecodeLen % 4) return -2;
    return toDecodeLen / 4*  3; // maximum length, may be 1 ro 2 less
}
//
// byte operations for b64Encode
//
#define i0_0 (i[0] & 0xfc) >> 2 // part of input byte 0 in output byte 0 - top 6 bits shifted down 2
#define i0_1 (i[0] & 0x03) << 4 // part of input byte 0 in output byte 1 - bottom 2 bits shifted up 4
#define i1_1 (i[1] & 0xf0) >> 4 // part of input byte 1 in output byte 1 - top 4 bits shifted down 4
#define i1_2 (i[1] & 0x0f) << 2 // part of input byte 1 in output byte 2 - bottom 4 bits shifted up 2
#define i2_2 (i[2] & 0xc0) >> 6 // part of input byte 2 in output byte 2 - top 2 bits shifted down 6
#define i2_3 (i[2] & 0x3f)      // part of input byte 2 in output byte 3 - bottom 6 bits
//
// See verticalDatum.h for documentation
//
int b64Encode(char** encoded, const char* toEncode, int toEncodeLen) {
    int len = b64EncodedLen(toEncodeLen);
    if (len < 0) return len;
    *encoded = (char*)mallocAndInit(len+1);
    const char* i = toEncode;
    char* o = *encoded;
    int remainingLen = toEncodeLen;
    while (remainingLen > 0) {
        switch (remainingLen) {
            case 1 :
                o[0] = base64chars[i0_0];
                o[1] = base64chars[i0_1];
                o[2] = '=';
                o[3] = '=';
                o += 4;
                remainingLen = 0;
                break;
            case 2 :
                o[0] = base64chars[i0_0];
                o[1] = base64chars[i0_1 | i1_1];
                o[2] = base64chars[i1_2];
                o[3] = '=';
                o += 4;
                remainingLen = 0;
                break;
            default :
                o[0] = base64chars[i0_0];
                o[1] = base64chars[i0_1 | i1_1];
                o[2] = base64chars[i1_2 | i2_2];
                o[3] = base64chars[i2_3];
                i += 3;
                o += 4;
                remainingLen -= 3;
        }
    }
    *o = '\0';
    return 0;
}
#undef i0_0
#undef i0_1
#undef i1_1
#undef i1_2
#undef i2_2
#undef i2_3
//
// byte operations for b64Decode
//
#define i0_0 (i[0] & 0x3f) << 2 // part of input byte 0 in output byte 0 - bits 0-5 shifted up 2
#define i1_0 (i[1] & 0x30) >> 4 // part of input byte 1 in output byte 0 - bits 4-5 shifed down 4
#define i1_1 (i[1] & 0x0f) << 4 // part of input byte 1 in output byte 1 - bits 0-3 shifted up 4
#define i2_1 (i[2] & 0x3c) >> 2 // part of input byte 2 in output byte 1 - bits 2-5 shifted down 2
#define i2_2 (i[2] & 0x03) << 6 // part of input byte 2 in output byte 2 - bits 0-1 shifted up 6
#define i3_2 (i[3] & 0x3f)      // part of input byte 3 in output byte 2 - bits 0-5
//
// See verticalDatum.h for documentation
//
int b64Decode(char** decoded, int* decodedLen, const char* toDecode) {
    int len = b64DecodedLen(strlen(toDecode));
    if (len < 4) {
        len = -1;
    }
    else if (*(toDecode + len - 2) == '=') {
        len -= 2;
    }
    else if (*(toDecode + len - 1) == '=') {
        --len;

    }
    if (len < 0) {
        return len;
    }
    *decoded = (char*)mallocAndInit(strlen(toDecode));
    const char* c = toDecode;
    char i[4];
    char* o = *decoded;
    while (*c) {
        i[0] = base64bytes[c[0]];
        i[1] = base64bytes[c[1]];
        i[2] = base64bytes[c[2]];
        i[3] = base64bytes[c[3]];
        if (i[0] == 255 || i[1] == 255) {
            free(*decoded);
            *decodedLen = 0;
            return -3;
        }
        o[0] = i0_0 | i1_0;
        o[1] = i1_1;
        if (c[2] == '=') {
            c += 2;
            break;
        }
        o[1] |= i2_1;
        o[2] = i2_2;
        if (c[3] == '=') {
            c += 3;
            break;
        }
        o[2] |= i3_2;

        c += 4;
        o += 3;
    }
    *decodedLen = (c - toDecode);
    return 0;
}
#undef i0_0
#undef i1_0
#undef i1_1
#undef i2_1
#undef i2_2
#undef i3_2
//
// See verticalDatum.h for documentation
//
char* findTextBetween(textBoundaryInfo* tbi, const char* buf, const char* after, const char* before) {
    if (tbi == NULL) {
        return NULL_POINTER_ERROR;
    }
    memset(tbi, 0, sizeof(textBoundaryInfo));
    tbi->firstWithBoundary = strstr(buf, after);
    if (!tbi->firstWithBoundary) {
        return BEGINNING_BOUNDARY_NOT_FOUND;
    }
    tbi->first = tbi->firstWithBoundary + strlen(after);
    tbi->offset = tbi->first - buf;
    tbi->last = strstr(tbi->first, before);
    if (!tbi->last) {
        return ENDING_BOUNDARY_NOT_FOUND;
    }
    tbi->lastWithBoundary = tbi->last + strlen(before);
    tbi->len = tbi->last - tbi->first;
    tbi->lenWithBoundaries = tbi->lastWithBoundary - tbi->firstWithBoundary;
    for (tbi->firstNonBlank = tbi->first;
         tbi->firstNonBlank < tbi->last && isspace(*tbi->firstNonBlank);
         tbi->firstNonBlank++);

    if (tbi->firstNonBlank == tbi->last) {
        tbi->lastNonBlank = tbi->last;
        tbi->lenNonBlank = 0;
    }
    else {

        for (tbi->lastNonBlank = tbi->last;
             tbi->lastNonBlank > tbi->first && isspace(*(tbi->lastNonBlank - 1));
             tbi->lastNonBlank--);
        tbi->lenNonBlank = tbi->lastNonBlank - tbi->firstNonBlank;
    }
    tbi->offsetNonBlank = tbi->firstNonBlank - buf;
    return NULL;
}
//
// Fortran interface for decodeAndGunzip
//
void decodeandgunzip_(
    char*       results,
    char*       errMsg,
    const char* inputBuf,
    size_t      lenResults,
    size_t      lenErrMsg,
    size_t      lenInputBuf) {

    char* cInputBuf = (char*)malloc(lenInputBuf + 1);
    F2C(inputBuf, cInputBuf, lenInputBuf, lenInputBuf + 1);
    char* cResults = NULL;
    char* cErrMsg = decodeAndGunzip(&cResults, cInputBuf);
    C2F(cResults, results, lenResults);
    C2F(cErrMsg, errMsg, lenErrMsg);
    free(cInputBuf);
}
//
// See verticalDatum.h for documentation
//
char* decodeAndGunzip(char** results, const char* inputBuf) {
    char     textBuf[4096];
    int      decodedLen;
    char*    decodedBuf;
    char*    outputBuf;
    int      rc;
    z_stream zstr;
    //--------------------------------//
    // first, Base64 decode the input //
    //--------------------------------//
    decodedLen = b64DecodedLen(strlen(inputBuf));
    if (b64Decode(&decodedBuf, &decodedLen, inputBuf)) {
        return BASE64_DECODING_ERROR;
    }
    //---------------------------------------------------//
    // next, setup the structure for the gzip decompress //
    //---------------------------------------------------//
    zstr.zalloc    = Z_NULL;
    zstr.zfree     = Z_NULL;
    zstr.opaque    = Z_NULL;
    zstr.avail_in  = decodedLen + 1;
    zstr.next_in   = decodedBuf;
    zstr.avail_out = sizeof(textBuf);
    zstr.next_out  = textBuf;
    rc = inflateInit2(&zstr, 16+MAX_WBITS);
    if (rc != Z_OK) {
        free(decodedBuf);
        return ERROR_ON_INFLATEINIT2;
    }
    //---------------------------------//
    // now perform the gzip decompress //
    //---------------------------------//
    rc = inflate(&zstr, Z_FINISH);
    if (rc != Z_OK && rc != Z_STREAM_END) {
        free(decodedBuf);
        return ERROR_ON_INFLATE;
    }
    rc = inflateEnd(&zstr);
    if (rc != Z_OK) {
        free(decodedBuf);
        return ERROR_ON_INFLATEEND;
    }
    //--------------------//
    // return the results //
    //--------------------//
    textBuf[zstr.total_out] = '\0';
    *results = (char*)mallocAndInit(strlen(textBuf)+1);
    strcpy(*results, textBuf);
    free(decodedBuf);
    return NULL;
}
//
// Fortran interface for gzipAndEncode
//
void gzipandencode_(
    char*       results,
    char*       errMsg,
    const char* inputBuf,
    size_t      lenResults,
    size_t      lenErrMsg,
    size_t      lenInputBuf) {

    char* cInputBuf = (char*)malloc(lenInputBuf + 1);
    F2C(inputBuf, cInputBuf, lenInputBuf, lenInputBuf + 1);
    char* cResults = NULL;
    char* cErrMsg = gzipAndEncode(&cResults, cInputBuf);
    C2F(cResults, results, lenResults);
    C2F(cErrMsg, errMsg, lenErrMsg);
    free(cInputBuf);
}
//
// See verticalDatum.h for documentation
//
char* gzipAndEncode(char** results, const char* inputBuf) {
    char*    compressedBuf;
    char*    encodedBuf;
    int      rc;
    int      inputLen = strlen(inputBuf)+1;
    int      outputLen;
    z_stream zstr;

    //-----------------------------------//
    // setup structure for gzip compress //
    //-----------------------------------//
    compressedBuf = (char*)mallocAndInit(inputLen);
    zstr.zalloc    = Z_NULL;
    zstr.zfree     = Z_NULL;
    zstr.opaque    = Z_NULL;
    zstr.avail_in  = inputLen;
    zstr.next_in   = (char*)inputBuf;
    zstr.avail_out = inputLen;
    zstr.next_out  = compressedBuf;
    //-------------------------//
    // gzip compress the input //
    //-------------------------//
    rc = deflateInit2(&zstr, Z_DEFAULT_COMPRESSION, Z_DEFLATED, 16+MAX_WBITS, 8, Z_DEFAULT_STRATEGY);
    if (rc != Z_OK) {
        return ERROR_ON_DEFLATEINIT2;
    }
    rc = deflate(&zstr, Z_FINISH);
    if (rc != Z_OK && rc != Z_STREAM_END) {
        free(compressedBuf);
        return ERROR_ON_DEFLATE;
    }
    rc = deflateEnd(&zstr);
    if (rc != Z_OK) {
        free(compressedBuf);
        return ERROR_ON_DEFLATEEND;
    }
    //----------------------------------//
    // Base64 encode the compressed buf //
    //----------------------------------//
    if (b64Encode(&encodedBuf, compressedBuf, zstr.total_out)) {
        free(compressedBuf);
        return BASE64_ENCODING_ERROR;
    }
    *results = encodedBuf;
    free(compressedBuf);
    return NULL;
}
//
// See verticalDatum.h for documentation
//
char* expandEmptyXmlTags(char** outputBuf, const char* inputBuf) {
    const char* in;
    char* out;
    int   tagBufLen = 32;
    char* tagBuf = (char*)mallocAndInit(tagBufLen);
    int   xmlBufLen = strlen(inputBuf) * 3;
    char* xmlBuf = (char*)mallocAndInit(xmlBufLen);
    int   inTag = FALSE;
    int   tagPos;
    char* tagChar;
    in  = inputBuf;
    out = xmlBuf;
    while (*in) {
        switch (*in) {
            case '<' :
                if (inTag) {
                    free(tagBuf);
                    free(xmlBuf);
                    return XML_IS_NOT_WELL_FORMED;
                }
                inTag = TRUE;
                tagPos = 0;
                memset(tagBuf, 0, tagBufLen);
                *out++ = *in++;
                break;
            case '/' :
                switch (*(in+1)) {
                    case '\0':
                        free(tagBuf);
                        free(xmlBuf);
                        return XML_IS_NOT_WELL_FORMED;
                    case '>' :
                        *out++ = '>';
                        *out++ = '<';
                        *out++ = '/';
                        tagChar = tagBuf;
                        while (*tagChar) {
                            *out++ = *tagChar++;
                        }
                        ++in;
                        break;
                    default :
                        *out++ = *in++;
                }
                break;
            case '>' :
                inTag = FALSE;
                *out++ = *in++;
                break;
            default :
                if (isspace(*in)) {
                    inTag = FALSE;
                }
                if (inTag) {
                    if (tagPos >= tagBufLen) {
                        tagBufLen *= 2;
                        char* cp = (char*)realloc(tagBuf, tagBufLen);
                        if (cp == NULL) {
                            free(tagBuf);
                            free(xmlBuf);
                            return MEMORY_ALLOCATION_ERROR;
                        }
                        tagBuf = cp;
                    }
                    tagBuf[tagPos++] = *in;
                }
                *out++ = *in++;
        }
    }
    free(tagBuf);
    *outputBuf = xmlBuf;
    return NULL;
}
//
// See verticalDatum.h for documentation
//
char* validateXmlStructure(const char* xml) {
    int     size = 20;
    int     count = 0;
    char**  tagNames = (char**)mallocAndInit(size*  sizeof(char*));
    char*   buf = (char*)mallocAndInit(strlen(xml)+1);
    char*   cp1;
    char*   cp2;
    size_t  len;
    int     first = TRUE;
    char*   saveptr = NULL;

    strcpy(buf, xml);
    while (TRUE) {
        if (first) {
            cp1 = strtok_r(buf, "<", &saveptr);
            first = FALSE;
        }
        else {
            cp1 = strtok_r(NULL, "<", &saveptr);
        }
        if (!cp1) {
            break;
        }
        for (cp2 = cp1; *cp2; ++cp2) {
            if (isspace(*cp2) || *cp2 == '>') {
                break;
            }
        }
        if (!*cp2) {
            for (int i = 0; i < count; ++i) {
                free(tagNames[i]);
            }
            free(tagNames);
            free(buf);
            return INVALID_XML_STRUCTURE;
        }
        if (*cp1 == '/') {
            //-----------------//
            // closing bracket //
            //-----------------//
            ++cp1;
            len = cp2 - cp1;
            if (strncmp(cp1, tagNames[count-1], len)) {
                free(buf);
                return INVALID_XML_STRUCTURE;
            }
            free(tagNames[--count]);
            if (count == 0) {
                break;
            }
        }
        else {
            //-----------------//
            // opening bracket //
            //-----------------//
            if (count++ == size) {
                size *= 2;
                char** cpp = (char**)realloc(tagNames, size*  sizeof(char*));
                if (cpp == NULL) {
                    free(tagNames);
                    free(buf);
                    return MEMORY_ALLOCATION_ERROR;
                }
                tagNames = cpp;
            }
            len = cp2 - cp1;
            tagNames[count-1] = (char*)mallocAndInit(len+1);
            strncpy(tagNames[count-1], cp1, len);
            tagNames[count-1][len] = '\0';
        }
    }
    for (int i = 0; i < count; ++i) {
        free(tagNames[i]);
    }
    free(tagNames);
    free(buf);
    return NULL;
}
//
// See verticalDatum.h for documentation
//
void initializeVerticalDatumInfo(verticalDatumInfo* vdi) {
    if (vdi != NULL) {
        memset(vdi, 0, sizeof(*vdi));
        vdi->offsetToNgvd29 = UNDEFINED_VERTICAL_DATUM_VALUE;
        vdi->offsetToNavd88 = UNDEFINED_VERTICAL_DATUM_VALUE;
    }
}
//
// Fortran interface to stringToVerticalDatumInfo
//
void stringtoverticaldatuminfo_(
    const char* inputStr,
    char*       errorMessage,
    char*       nativeDatum,
    char*       unit,
    double*     offsetNgvd29,
    int32_t*    offsetNgvd29IsEstimate,
    double*     offsetNavd88,
    int32_t*    offsetNavd88IsEstimate,
    size_t      lenInputStr,
    size_t      lenErrorMessage,
    size_t      lenNativeDatum,
    size_t      lenUnit) {
    char* cInputStr = (char*)mallocAndInit(lenInputStr + 1);
    F2C(inputStr, cInputStr, lenInputStr, lenInputStr + 1);
    char* cErrMsg;
    verticalDatumInfo vdi;

    cErrMsg = stringToVerticalDatumInfo(&vdi, cInputStr);
    if (cErrMsg) {
        C2F(cErrMsg, errorMessage, lenErrorMessage);
        C2F(CVERTICAL_DATUM_UNSET, nativeDatum, lenNativeDatum);
        C2F(" ", unit, lenUnit);
        *offsetNgvd29 = UNDEFINED_VERTICAL_DATUM_VALUE;
        *offsetNgvd29IsEstimate = -1;
        *offsetNavd88 = UNDEFINED_VERTICAL_DATUM_VALUE;
        *offsetNavd88IsEstimate = -1;
    }
    else {
        C2F(" ", errorMessage, lenErrorMessage);
        C2F(vdi.nativeDatum, nativeDatum, lenNativeDatum);
        C2F(vdi.unit, unit, lenUnit);
        *offsetNgvd29 = vdi.offsetToNgvd29;
        *offsetNgvd29IsEstimate = vdi.offsetToNgvd29IsEstimate;
        *offsetNavd88 = vdi.offsetToNavd88;
        *offsetNavd88IsEstimate = vdi.offsetToNavd88IsEstimate;
    }
    free(cInputStr);
}
//
// See verticalDatum.h for documentation
//
char* stringToVerticalDatumInfo(verticalDatumInfo* vdi, const char* inputStr) {
    char* errmsg = NULL;
    char*  xml1;
    char*  xml;
    char   offsetBuf[2][128];
    int    freeXml1;
    double dtmp;
    textBoundaryInfo tbi;

    initializeVerticalDatumInfo(vdi);

    memset(offsetBuf, 0, sizeof(offsetBuf)); // null terminators will exist after strncpy()

    int ngvd29OffsetProcessed = 0;
    int navd88OffsetProcessed = 0;

    if (inputStr == NULL || *inputStr == '\0') {
        vdi = NULL;
        return INPUT_STRING_IS_NULL;
    }
    if (strchr(inputStr, '<')) {
        xml1 = (char*)inputStr;
        freeXml1 = FALSE;
    }
    else {
        errmsg = decodeAndGunzip(&xml1, inputStr);
        if (errmsg != NULL) {
            return errmsg;
        }
        freeXml1 = TRUE;
    }
    errmsg = expandEmptyXmlTags(&xml, xml1);
    if (freeXml1) {
        free(xml1);
    }
    if (errmsg) {
        return errmsg;
    }
    errmsg = validateXmlStructure(xml);
    if (errmsg) {
        return errmsg;
    }
    errmsg = findTextBetween(&tbi, xml, "<vertical-datum-info", "</vertical-datum-info>");
    if (errmsg != NULL || tbi.lenNonBlank == 0) {
        free(xml);
        return XML_IS_NOT_A_VALID_VERTICAL_DATUM_INFO_INSTANCE;
    }
    //---------------------//
    // get the offset unit //
    //---------------------//
    errmsg = findTextBetween(&tbi, tbi.first, "unit=\"", "\"");
    if (errmsg != NULL || tbi.lenNonBlank == 0) {
        free(xml);
        return NO_OFFSET_UNIT_IN_XML;
    }
    strncpy(vdi->unit, tbi.firstNonBlank, MIN(tbi.lenNonBlank, sizeof(vdi->unit)-1));
    if (strcmp(vdi->unit, "ft") && strcmp(vdi->unit, "m")) {
        free(xml);
        return INVALID_OFFSET_UNIT_IN_XML;
    }
    //----------------------//
    // get the native datum //
    //----------------------//
    errmsg = findTextBetween(&tbi, xml, "<native-datum>", "</native-datum>");
    if (errmsg != NULL || tbi.lenNonBlank == 0) {
        free(xml);
        return NO_NATIVE_DATUM_IN_XML;
    }
    strncpy(vdi->nativeDatum, tbi.firstNonBlank, MIN(tbi.lenNonBlank, sizeof(vdi->nativeDatum)-1));
    if (!strcmp(vdi->nativeDatum, CVERTICAL_DATUM_NAVD88)) {
        vdi->offsetToNavd88 = 0.f;
        vdi->offsetToNavd88IsEstimate = FALSE;
    }
    else if (!strcmp(vdi->nativeDatum, CVERTICAL_DATUM_NGVD29)) {
        vdi->offsetToNgvd29 = 0.f;
        vdi->offsetToNgvd29IsEstimate = FALSE;
    }
    else {
        //---------------------------------------//
        // get the local datum name if it exists //
        //---------------------------------------//
        errmsg = findTextBetween(&tbi, xml, "<local-datum-name>", "</local-datum-name>");
        if (errmsg == NULL && tbi.lenNonBlank > 0) {
            size_t len = MIN(tbi.lenNonBlank, sizeof(vdi->nativeDatum)-1);
            strncpy(vdi->nativeDatum, tbi.firstNonBlank, len);
            vdi->nativeDatum[len] = '\0'; // because we're overwriting a previous value
        }
    }
    //----------------------------------------//
    // get the sub-xml blocks for the offsets //
    //----------------------------------------//
    errmsg = findTextBetween(&tbi, xml, "<offset", "</offset>");
    if (errmsg == NULL && tbi.lenNonBlank != 0) {
        strncpy(offsetBuf[0], tbi.firstWithBoundary, tbi.lenWithBoundaries);
        errmsg = findTextBetween(&tbi, tbi.lastWithBoundary, "<offset", "</offset>");
        if (errmsg == NULL && tbi.lenNonBlank != 0) {
            strncpy(offsetBuf[1], tbi.firstWithBoundary, tbi.lenWithBoundaries);
        }
    }
    //---------------------------//
    // process the offset blocks //
    //---------------------------//
    for (int i = 0; i < 2; ++i) {
        if (offsetBuf[i][0] == '\0') {
            if (i == 0) {
                free(xml);
                return NULL; //MISSING_OFFSET_BLOCK_IN_XML;
            }
            break; // only 1 offset block
        }
        errmsg = findTextBetween(&tbi, offsetBuf[i], "<to-datum>", "</to-datum>");
        if (errmsg != NULL || tbi.lenNonBlank == 0) {
            free(xml);
            return INVALID_OFFSET_BLOCK_IN_XML;
        }
        if (!strncmp(tbi.firstNonBlank, CVERTICAL_DATUM_NGVD29, 7)) {
            if (ngvd29OffsetProcessed) {
                free(xml);
                return MULTIPLE_NGVD_29_OFFSET_BLOCKS_IN_XML;
            }
            ngvd29OffsetProcessed = 1;
            errmsg = findTextBetween(&tbi, offsetBuf[i], "<value>", "</value>");
            if (errmsg != NULL || tbi.lenNonBlank == 0) {
            free(xml);
            return NO_NGVD_29_OFFSET_VALUE_IN_XML;
            }
            errno = 0;
            dtmp = strtod(tbi.firstNonBlank, &tbi.lastNonBlank);
            if (dtmp != 0.f || errno == 0) {
                vdi->offsetToNgvd29 = dtmp;
            }
            else {
                free(xml);
                return INVALID_NGVD_29_OFFSET_VALUE_IN_XML;
            }
            errmsg = findTextBetween(&tbi, offsetBuf[i], "estimate=\"", "\"");
            if (errmsg != NULL || tbi.lenNonBlank == 0) {
                free(xml);
                return INVALID_NGVD_29_OFFSET_BLOCK_IN_XML;
            }
            if (!strncmp(tbi.firstNonBlank, "true", tbi.lenNonBlank)) {
                vdi->offsetToNgvd29IsEstimate = TRUE;
            }
            else if (!strncmp(tbi.firstNonBlank, "false", tbi.lenNonBlank)) {
                vdi->offsetToNgvd29IsEstimate = FALSE;
            }
            else {
                free(xml);
                return INVALID_NGVD_29_OFFSET_BLOCK_IN_XML;
            }
        }
        else if (!strncmp(tbi.firstNonBlank, CVERTICAL_DATUM_NAVD88, 7)) {
            if (navd88OffsetProcessed) {
                free(xml);
                return MULTIPLE_NAVD_88_OFFSET_BLOCKS_IN_XML;
            }
            navd88OffsetProcessed = 1;
            errmsg = findTextBetween(&tbi, offsetBuf[i], "<value>", "</value>");
            if (errmsg != NULL || tbi.lenNonBlank == 0) {
                free(xml);
                return NO_NAVD_88_OFFSET_VALUE_IN_XML;
            }
            errno = 0;
            dtmp = strtod(tbi.firstNonBlank, &tbi.lastNonBlank);;
            if (dtmp != 0.f || errno == 0) {
                vdi->offsetToNavd88 = dtmp;
            }
            else {
                free(xml);
                return INVALID_NAVD_88_OFFSET_VALUE_IN_XML;

            }
            errmsg = findTextBetween(&tbi, offsetBuf[i], "estimate=\"", "\"");
            if (errmsg != NULL || tbi.lenNonBlank == 0) {
				free(xml);
                return INVALID_NAVD_88_OFFSET_BLOCK_IN_XML;
            }
            if (!strncmp(tbi.firstNonBlank, "true", tbi.lenNonBlank)) {
                vdi->offsetToNavd88IsEstimate = TRUE;
            }
            else if (!strncmp(tbi.firstNonBlank, "false", tbi.lenNonBlank)) {
                vdi->offsetToNavd88IsEstimate = FALSE;
            }
            else {
                free(xml);
                return INVALID_NAVD_88_OFFSET_BLOCK_IN_XML;
            }
        }
        else {
            free(xml);
            return INVALID_DATUM_IN_SPECIFIED_IN_XML;
        }
    }
    free(xml);
    return NULL;
}
//
// Fortran interface for verticalDatumInfoToString
//
void verticaldatuminfotostring_(
    char*          outputStr,
    char*          errorMessage,
    const char*    nativeDatum,
    const char*    unit,
    const double*  offsetNgvd29,
    const int32_t* offsetNgvd29IsEstimate,
    const double*  offsetNavd88,
    const int32_t* offsetNavd88IsEstimate,
    const int32_t* generateCompressed,
    size_t         lenErrorMessage,
    size_t         lenOutputStr,
    size_t         lenNativeDatum,
    size_t         lenUnit) {

    char* cErrMsg;
    char* cResults;
    verticalDatumInfo vdi;
    F2C(nativeDatum, vdi.nativeDatum, lenNativeDatum, sizeof(vdi.nativeDatum));
    F2C(unit, vdi.unit, lenUnit, sizeof(vdi.unit));
    vdi.offsetToNgvd29 = *offsetNgvd29;
    vdi.offsetToNgvd29IsEstimate = *offsetNgvd29IsEstimate;
    vdi.offsetToNavd88 = *offsetNavd88;
    vdi.offsetToNavd88IsEstimate = *offsetNavd88IsEstimate;
    cErrMsg = verticalDatumInfoToString(&cResults, &vdi, *generateCompressed);
    C2F(cErrMsg, errorMessage, lenErrorMessage);
    C2F(cResults, outputStr, lenOutputStr);
    free(cResults);
}
//
// See verticalDatum.h for documentation
//
char* verticalDatumInfoToString(char** results, const verticalDatumInfo* vdi, int generateCompressed) {
    char  xml[4096];
    char* cp = xml;
    char* errmsg;

    sprintf(cp, "<vertical-datum-info unit=\"%s\">\n", vdi->unit);
    while (*cp)++cp;
    if (!strcmp(vdi->nativeDatum, CVERTICAL_DATUM_NGVD29) ||
        !strcmp(vdi->nativeDatum, CVERTICAL_DATUM_NAVD88) ||
        !strcmp(vdi->nativeDatum, CVERTICAL_DATUM_OTHER) ||
        !strcmp(vdi->nativeDatum, CVERTICAL_DATUM_LOCAL)) {

        sprintf(cp, "  <native-datum>%s</native-datum>\n", vdi->nativeDatum);
    }
    else {
        sprintf(cp, "  <native-datum>OTHER</native-datum>\n  <local-datum-name>%s</local-datum-name>\n", vdi->nativeDatum);
    }
    while (*cp)++cp;
    if (vdi->offsetToNgvd29 != UNDEFINED_VERTICAL_DATUM_VALUE) {
        sprintf(
            cp,
            "  <offset estimate=\"%s\">\n    <to-datum>NGVD-29</to-datum>\n    <value>%lf</value>\n  </offset>\n",
            vdi->offsetToNgvd29IsEstimate ? "true" : "false",
            vdi->offsetToNgvd29);
        while (*cp)++cp;
    }
    if (vdi->offsetToNavd88 != UNDEFINED_VERTICAL_DATUM_VALUE) {
        sprintf(
            cp,
            "  <offset estimate=\"%s\">\n    <to-datum>NAVD-88</to-datum>\n    <value>%lf</value>\n  </offset>\n",
            vdi->offsetToNavd88IsEstimate ? "true" : "false",
            vdi->offsetToNavd88);
        while (*cp)++cp;
    }
    sprintf(cp, "</vertical-datum-info>");
    if (generateCompressed) {
        errmsg = gzipAndEncode(results, xml);
        if (errmsg != NULL) {
            return errmsg;
        }
    }
    else {
		*results = (char*)mallocAndInit(strlen(xml)+1);
        strcpy(*results, xml);
    }
    return NULL;
}
//
// See verticalDatum.h for documentation
//
verticalDatumInfo* extractVerticalDatumInfoFromUserHeader(const int* userHeader, int userHeaderSize) {
    verticalDatumInfo* vdi = NULL;
    char* cp = userHeaderToString(userHeader, userHeaderSize);
    if (cp) {
        char* vdiStr = extractFromDelimitedString(
            &cp,
            VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
            ":",
            FALSE,
            FALSE,
            ';');
        if (vdiStr) {
            vdi = (verticalDatumInfo*)malloc(sizeof(verticalDatumInfo));
            char* errmsg = stringToVerticalDatumInfo(vdi, vdiStr);
            if (errmsg != NULL) {
                if (vdi) {
                    free(vdi);
                }
                vdi = NULL;
            }
            free(vdiStr);
        }
        free(cp);
    }
    return vdi;
}
/**
 * Fortran callable routine to return any vertical datum info in a DSS record user header
 *
 * @param nativeDatum              Receives the native datum
 * @param unit                     Receives the VDI unit
 * @param offsetToNavd88           Receives the offset from the native datum to NAVD-88
 * @param offsetToNavd88IsEstimate Receives whether the offsetToNavd88 value is estmated (0/1)
 * @param offsetToNgvd29           Receives the offset from the native datum to NGVD-29
 * @param offsetToNgvd29IsEstimate Receives whether the offsetToNgvd29 value is estmated (0/1)
 * @param userHeader               The user header integer array to retrieve the VDI from
 * @param userHeaderSize           The number of integers in the user header
 * @param lenNativeDatum           The hidden parameter passed from Fortran for the size of the nativeDatum character parameter
 * @param lenUnit                  The hidden parameter passed from Fortran for the size of the unit character parameter
 */
void extractverticaldatuminfofromuserheader_(
        char*      nativeDatum,
        char*      unit,
        double*    offsetToNavd88,
        int*       offsetToNavd88IsEstimate,
        double*    offsetToNgvd29,
        int*       offsetToNgvd29IsEstimate,
        const int* userHeader,
        const int* userHeaderSize,
        size_t     lenNativeDatum,
        size_t     lenUnit) {
    C2F(" ", nativeDatum, lenNativeDatum);
    C2F(" ", unit,        lenUnit);
    *offsetToNgvd29           = UNDEFINED_VERTICAL_DATUM_VALUE;
    *offsetToNgvd29IsEstimate = TRUE;
    *offsetToNavd88           = UNDEFINED_VERTICAL_DATUM_VALUE;
    *offsetToNavd88IsEstimate = TRUE;
    verticalDatumInfo* tmpVdi = extractVerticalDatumInfoFromUserHeader(userHeader, *userHeaderSize);
    if (tmpVdi) {
        C2F(tmpVdi->nativeDatum, nativeDatum, lenNativeDatum);
        C2F(tmpVdi->unit,        unit,        lenUnit);
        *offsetToNgvd29           = tmpVdi->offsetToNgvd29;
        *offsetToNgvd29IsEstimate = tmpVdi->offsetToNgvd29IsEstimate;
        *offsetToNavd88           = tmpVdi->offsetToNavd88;
        *offsetToNavd88IsEstimate = tmpVdi->offsetToNavd88IsEstimate;
        free(tmpVdi);
    }
}
/**
 * Fortran callable routine to return any vertical datum info in a DSS 7 location record for a pathname
 *
 * @param nativeDatum              Receives the native datum
 * @param unit                     Receives the VDI unit
 * @param offsetToNavd88           Receives the offset from the native datum to NAVD-88
 * @param offsetToNavd88IsEstimate Receives whether the offsetToNavd88 value is estmated (0/1)
 * @param offsetToNgvd29           Receives the offset from the native datum to NGVD-29
 * @param offsetToNgvd29IsEstimate Receives whether the offsetToNgvd29 value is estmated (0/1)
 * @param fileTable                The file table (ifltab) of the open DSS 7 file
 * @pathname                       The pathname to retrieve the location record for
 * @param lenNativeDatum           The hidden parameter passed from Fortran for the size of the nativeDatum character parameter
 * @param lenUnit                  The hidden parameter passed from Fortran for the size of the unit character parameter
 * @param lenPathname              The hidden parameter passed from Fortran for the size of the pathname character parameter
 */
void getlocationverticaldatuminfo_(
        char*      nativeDatum,
        char*      unit,
        double*    offsetToNavd88,
        int*       offsetToNavd88IsEstimate,
        double*    offsetToNgvd29,
        int*       offsetToNgvd29IsEstimate,
        long long* fileTable,
        char*      pathname,
        size_t     lenNativeDatum,
        size_t     lenUnit,
        size_t     lenPathname) {

    char* cPathname = (char*)mallocAndInit(lenPathname+1);
    verticalDatumInfo vdi;
    memset(&vdi, 0, sizeof(vdi));

    F2C(pathname, cPathname, lenPathname, lenPathname+1);
    zStructLocation* ls = zstructLocationNew(cPathname);
    free(cPathname);
    if (ls) {
        zlocationRetrieve(fileTable, ls);
        if (ls->supplemental) {
            char* compressedVdi = extractFromDelimitedString(
                &ls->supplemental,
                VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                ":",
                TRUE,
                FALSE,
                ';');
            if (compressedVdi) {
                stringToVerticalDatumInfo(&vdi, compressedVdi);
                strcpy(nativeDatum, vdi.nativeDatum);
                free(compressedVdi);
            }
        }
        zstructFree(ls);
    }
    if (strlen(vdi.nativeDatum) > 0) {
        C2F(vdi.nativeDatum, nativeDatum, lenNativeDatum);
        C2F(vdi.unit,        unit,        lenUnit);
        *offsetToNgvd29           = vdi.offsetToNgvd29;
        *offsetToNgvd29IsEstimate = vdi.offsetToNgvd29IsEstimate;
        *offsetToNavd88           = vdi.offsetToNavd88;
        *offsetToNavd88IsEstimate = vdi.offsetToNavd88IsEstimate;
    }
    else {
        C2F(" ", nativeDatum, lenNativeDatum);
        C2F(" ", unit,        lenUnit);
        *offsetToNgvd29           = UNDEFINED_VERTICAL_DATUM_VALUE;
        *offsetToNgvd29IsEstimate = TRUE;
        *offsetToNavd88           = UNDEFINED_VERTICAL_DATUM_VALUE;
        *offsetToNavd88IsEstimate = TRUE;
    }
}
//
// See verticalDatum.h for documentation
//
int	getCurrentVerticalDatum(
        char* cverticalDatum,
        int   cverticalDatumSize,
        int*  userHeader,
        int*  userHeaderSize,
        char* unit) {

    if (cverticalDatum == NULL || cverticalDatumSize < CVERTICAL_DATUM_SIZE) {
        return -1;
    }
    memset(cverticalDatum, 0, cverticalDatumSize);
    //----------------------------//
    // first get the global value //
    //----------------------------//
    int iverticalDatum;
    zquery("VDTM", cverticalDatum, cverticalDatumSize, &iverticalDatum);
    //-----------------------------//
    // next, check the user header //
    //-----------------------------//
    if (userHeader != NULL && *userHeaderSize > 0) {
        char* userHeaderString = userHeaderToString(userHeader, *userHeaderSize);
        if (userHeaderString) {
            char* verticalDatum = extractFromDelimitedString(
                &userHeaderString,
                VERTICAL_DATUM_USER_HEADER_PARAM,
                ":",
                TRUE,
                TRUE, // this causes vertical datum to be removed from userHeaderString
                ';');
            if (verticalDatum) {
                if (!strcasecmp(verticalDatum, CVERTICAL_DATUM_NAVD88)) {
                    iverticalDatum = IVERTICAL_DATUM_NAVD88;
                    strcpy(cverticalDatum, CVERTICAL_DATUM_NAVD88);
                }
                else if (!strcasecmp(verticalDatum, CVERTICAL_DATUM_NGVD29)) {
                    iverticalDatum = IVERTICAL_DATUM_NGVD29;
                    strcpy(cverticalDatum, CVERTICAL_DATUM_NGVD29);
                }
                else if (!strcasecmp(verticalDatum, CVERTICAL_DATUM_UNSET)) {
                    iverticalDatum = IVERTICAL_DATUM_UNSET;
                    strcpy(cverticalDatum, CVERTICAL_DATUM_UNSET);
                }
                else if (!strcasecmp(verticalDatum, CVERTICAL_DATUM_OTHER)) {
                    iverticalDatum = IVERTICAL_DATUM_OTHER;
                    strcpy(cverticalDatum, CVERTICAL_DATUM_OTHER);
                }
                else {
                    iverticalDatum = IVERTICAL_DATUM_OTHER;
                    strcpy(cverticalDatum, verticalDatum);
                }
                //---------------------------------------//
                // remove vertical datum from userHeader //
                //---------------------------------------//
                int  newHeaderSize;
                int* newUserHeader = stringToUserHeader(userHeaderString, &newHeaderSize);
                if (newHeaderSize > *userHeaderSize) {
                    free(newUserHeader);
                    free(verticalDatum);
                    free(userHeaderString);
                    return -1; // shouldn't ever happen
                }
                memset(userHeader, 0, *userHeaderSize * sizeof(int));
                memcpy(userHeader, newUserHeader, newHeaderSize * sizeof(int));
                *userHeaderSize = newHeaderSize;
                free(newUserHeader);
                free(verticalDatum);
            }
            free(userHeaderString);
        }
    }
    //---------------------------------------//
    // finally, check the unit specification //
    //---------------------------------------//
    if (unit != NULL) {
        if (strchr(unit, '|')) {
            char*  saveptr;
            char* value;
            char* unitValue = NULL;
            char* verticalDatum = NULL;
            char* unitSpec = mallocAndCopy(unit);
            char* key = strtok_r(unitSpec, "|=", &saveptr);
            while (key) {
                value = strtok_r(NULL, "|=", &saveptr);
                if (!strcasecmp(key, "U")) {
                    unitValue = value;
                }
                else if (!strcasecmp(key, "V")) {
                    verticalDatum = value;
                }
                key = strtok_r(NULL, "|=", &saveptr);
            }
            if (unitValue) {
                //----------------------------------------------//
                // convert the unit spec to a simple unit value //
                //----------------------------------------------//
                sprintf(unit, "%s", unitValue); // safe becuase new string is always shorter than old string
            }
            if (verticalDatum) {
                if (!strcasecmp(verticalDatum, CVERTICAL_DATUM_NAVD88)) {
                    iverticalDatum = IVERTICAL_DATUM_NAVD88;
                    strcpy(cverticalDatum, CVERTICAL_DATUM_NAVD88);
                }
                else if (!strcasecmp(verticalDatum, CVERTICAL_DATUM_NGVD29)) {
                    iverticalDatum = IVERTICAL_DATUM_NGVD29;
                    strcpy(cverticalDatum, CVERTICAL_DATUM_NGVD29);
                }
                else if (!strcasecmp(verticalDatum, CVERTICAL_DATUM_UNSET)) {
                    iverticalDatum = IVERTICAL_DATUM_UNSET;
                    strcpy(cverticalDatum, CVERTICAL_DATUM_UNSET);
                }
                else if (!strcasecmp(verticalDatum, CVERTICAL_DATUM_OTHER)) {
                    iverticalDatum = IVERTICAL_DATUM_OTHER;
                    strcpy(cverticalDatum, CVERTICAL_DATUM_OTHER);
                }
                else {
                    iverticalDatum = IVERTICAL_DATUM_OTHER;
                    strcpy(cverticalDatum, verticalDatum);
                }
            }
            free(unitSpec);
        }
    }
    return iverticalDatum;
}
//
// Fortran interface for normalizeVdiInUserHeader
//
void normalizevdiinuserheader_(
    int*   userHeader,
    int*   userHeaderNumber,
    int*   userHeaderSize,
    char*  errorMesage,
    size_t lenErrorMessage) {

    char* cErrMsg = normalizeVdiInUserHeader(userHeader, userHeaderNumber, *userHeaderSize);
    if (cErrMsg) {
        C2F(cErrMsg, errorMesage, lenErrorMessage);
    }
}
//
// See verticalDatum.h for documentation
//
char* normalizeVdiInUserHeader(int* userHeader, int* userHeaderNumber, int userHeaderSize) {
    char* headerString = userHeaderToString(userHeader, *userHeaderNumber);
    if (!headerString) return NULL;
    int headerStringSize = strlen(headerString);
    char* vdiStr = extractFromDelimitedString(
        &headerString,
        VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
        ":",
        TRUE,
        FALSE,
        ';');
    if (!vdiStr) {
        free(headerString);
        return NULL;
    }
    verticalDatumInfo vdi;
    char* errmsg = stringToVerticalDatumInfo(&vdi, vdiStr);
    free(vdiStr);
    if (errmsg) {
        free(headerString);
        return errmsg;
    }
    errmsg = verticalDatumInfoToString(&vdiStr, &vdi, FALSE);
    if (errmsg) {
        free(headerString);
        return errmsg;
    }
    errmsg = verticalDatumInfoToString(&vdiStr, &vdi, TRUE);
    if (errmsg) {
        free(headerString);
        return errmsg;
    }
    int status = insertIntoDelimitedString(
        &headerString,
        headerStringSize,
        VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
        vdiStr,
        ":",
        TRUE,
        ';');
    if (status == -1) {
        headerStringSize *= 2;
        char* cp = (char*)realloc(headerString, headerStringSize);
        if (!cp) {
            status = 1;
        }
        else {
            headerString = cp;
            status = insertIntoDelimitedString(
                &headerString,
                headerStringSize,
                VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                vdiStr,
                ":",
                TRUE,
                ';');
        }
    }
    free(vdiStr);
    if (status) {
        free(headerString);
        return MEMORY_ALLOCATION_ERROR;
    }
    char* workbuf = strdup(headerString);
    headerString[0] = '\0';
    char* saveptr = NULL;
    char* cp = strtok_r(workbuf, ";", &saveptr);
    while (cp) {
        int offset = strspn(cp, " ");
        if (offset < strlen(cp)) {
            strcat(headerString, &cp[offset]);
            strcat(headerString, ";");
        }
        cp = strtok_r(NULL, ";", &saveptr);
    }
    free(workbuf);
    int newHeaderNumber;
    int* newHeader = stringToUserHeader(headerString, &newHeaderNumber);
    if (newHeaderNumber > userHeaderSize) {
        free(headerString);
        free(newHeader);
        return INSUFFICIENT_SPACE_ERROR;
    }
    *userHeaderNumber = newHeaderNumber;
    for (int i = 0; i < *userHeaderNumber; ++i) {
        userHeader[i] = i < newHeaderNumber ? newHeader[i] : 0;
    }
    free(headerString);
    free(newHeader);
    return NULL;
}
//
// Fortran wrapper for processStorageVdis
//
void processstoragevdis_(
    double*        offsetToUse,
    char*          errorMessage,
    const char*    fileVdiStr,
    const char*    dataVdiStr,
    const char*    currentDatum,
    const int32_t* fileContainsData,
    const char*    dataUnit,
    size_t         lenErrorMessage,
    size_t         lenFileVdiStr,
    size_t         lenDataVdiStr,
    size_t         lenCurrentDatum,
    size_t         lenDataUnit) {

    char* cFileVdiStr = (char*)malloc(lenFileVdiStr + 1);
    char* cDataVdiStr = (char*)malloc(lenDataVdiStr + 1);
    char* cCurrentDatum = (char*)malloc(lenCurrentDatum + 1);
    char* cDataUnit = (char*)malloc(lenDataUnit + 1);
    F2C(fileVdiStr, cFileVdiStr, lenFileVdiStr, lenFileVdiStr + 1);
    F2C(dataVdiStr, cDataVdiStr, lenDataVdiStr, lenDataVdiStr + 1);
    F2C(currentDatum, cCurrentDatum, lenCurrentDatum, lenCurrentDatum + 1);
    F2C(dataUnit, cDataUnit, lenDataUnit, lenDataUnit + 1);

    verticalDatumInfo fileVdi;
    stringToVerticalDatumInfo(&fileVdi, cFileVdiStr);
    verticalDatumInfo dataVdi;
    stringToVerticalDatumInfo(&dataVdi, cDataVdiStr);
    char* cp = NULL;
    verticalDatumInfoToString(&cp, &dataVdi, FALSE);
    free(cp);
    char* cErrorMessage = processStorageVdis(offsetToUse, &fileVdi, &dataVdi, cCurrentDatum, *fileContainsData, cDataUnit);
    if (cErrorMessage) {
        C2F(cErrorMessage, errorMessage, lenErrorMessage);
        free(cErrorMessage);
    }
    else {
        C2F("", errorMessage, lenErrorMessage);
    }
    if (cFileVdiStr)
        free(cFileVdiStr);
    if (cDataVdiStr)
        free(cDataVdiStr);
    if (cCurrentDatum)
        free(cCurrentDatum);
    if (cDataUnit)
        free(cDataUnit);

}
//
// See verticalDatum.h for documentation
//
char* processStorageVdis(
    double*                  offsetToUse,
    const verticalDatumInfo* _fileVdi,
    const verticalDatumInfo* _dataVdi,
    const char*              _currentDatum,
    int                      fileContainsData,
    const char*              dataUnit) {

    int vdiOverwrite = FALSE;
    char charVal[8];
    zquery("VDOW", charVal, sizeof(charVal), &vdiOverwrite);
    *offsetToUse = UNDEFINED_VERTICAL_DATUM_VALUE;
    verticalDatumInfo fileVdi;
    initializeVerticalDatumInfo(&fileVdi);
    if (_fileVdi) {
        strcpy(fileVdi.nativeDatum, _fileVdi->nativeDatum);
        strcpy(fileVdi.unit, _fileVdi->unit);
        fileVdi.offsetToNavd88 = _fileVdi->offsetToNavd88;
        fileVdi.offsetToNavd88IsEstimate = _fileVdi->offsetToNavd88IsEstimate;
        fileVdi.offsetToNgvd29 = _fileVdi->offsetToNgvd29;
        fileVdi.offsetToNgvd29IsEstimate = _fileVdi->offsetToNgvd29IsEstimate;
    }
    verticalDatumInfo dataVdi;
    initializeVerticalDatumInfo(&dataVdi);
    if (_dataVdi) {
        strcpy(dataVdi.nativeDatum, _dataVdi->nativeDatum);
        strcpy(dataVdi.unit, _dataVdi->unit);
        dataVdi.offsetToNavd88 = _dataVdi->offsetToNavd88;
        dataVdi.offsetToNavd88IsEstimate = _dataVdi->offsetToNavd88IsEstimate;
        dataVdi.offsetToNgvd29 = _dataVdi->offsetToNgvd29;
        dataVdi.offsetToNgvd29IsEstimate = _dataVdi->offsetToNgvd29IsEstimate;
    }
    char* fileNativeDatum = !strcmp(fileVdi.nativeDatum, "") ? CVERTICAL_DATUM_UNSET : fileVdi.nativeDatum;
    char* dataNativeDatum = !strcmp(dataVdi.nativeDatum, "") ? CVERTICAL_DATUM_UNSET : dataVdi.nativeDatum;
    char* currentDatum = !strcmp((char*)_currentDatum, "") ? CVERTICAL_DATUM_UNSET : (char*)_currentDatum;
    char  errorMessage[1024];
    //---------------------------------------------------------------------//
    // test whether data native datum is compatible with file native datum //
    //---------------------------------------------------------------------//
    int compatibleNativeDatum = TRUE;
    if (fileContainsData) {
        compatibleNativeDatum = (
            !strcmp(dataNativeDatum, CVERTICAL_DATUM_UNSET)
            || !strcmp(fileNativeDatum, dataNativeDatum));
    }
    else {
        compatibleNativeDatum = (
            !strcmp(fileNativeDatum, CVERTICAL_DATUM_UNSET)
            || !strcmp(dataNativeDatum, CVERTICAL_DATUM_UNSET)
            || !strcmp(fileNativeDatum, dataNativeDatum));
    }
    if (!compatibleNativeDatum) {
        if (!vdiOverwrite) {
            sprintf(
                errorMessage,
                " VERTICAL DATUM ERROR\n"
                " Data native datum of %s conflicts with file native datum of %s.\n"
                " No data stored.",
                dataNativeDatum,
                fileNativeDatum);
            return strdup(errorMessage);
        }
    }
    //-------------------------------------//
    // test whether we have eqivalent VDIs //
    //-------------------------------------//
    if ((!strcmp(dataNativeDatum, fileNativeDatum)
        && strcmp(dataNativeDatum, CVERTICAL_DATUM_UNSET))
        && !vdiOverwrite) {
        //---------------//
        // compare units //
        //---------------//
        if (!unitIsFeet(dataVdi.unit) && !unitIsMeters(dataVdi.unit)) {
            sprintf(
                errorMessage,
                " VERTICAL DATUM ERROR\n"
                " Data VDI unit of %s is not recognized as feet or meters.\n"
                " No data stored.",
                dataVdi.unit);
            return strdup(errorMessage);
        }
        if (!unitIsFeet(fileVdi.unit) && !unitIsMeters(fileVdi.unit)) {
            sprintf(
                errorMessage,
                " VERTICAL DATUM ERROR\n"
                " File VDI unit of %s is not recognized as feet or meters.\n"
                " No data stored.",
                fileVdi.unit);
            return strdup(errorMessage);
        }
        //-------------------------//
        // compare NAVD-88 offsets //
        //-------------------------//
        char buf[80];
        if (isUndefinedVertDatumValue(dataVdi.offsetToNavd88) != isUndefinedVertDatumValue(fileVdi.offsetToNavd88)) {
            sprintf(
                errorMessage,
                " VERTICAL DATUM ERROR\n"
                " Data VDI offset to NAVD-88 of %s%s%s conflicts with file VDI offset of %s%s%s.\n"
                " No data stored.",
                isUndefinedVertDatumValue(dataVdi.offsetToNavd88) ? "UNDEFINED" : doubleToChar(dataVdi.offsetToNavd88, buf),
                isUndefinedVertDatumValue(dataVdi.offsetToNavd88) ? "" : " ",
                isUndefinedVertDatumValue(dataVdi.offsetToNavd88) ? "" : dataVdi.unit,
                isUndefinedVertDatumValue(fileVdi.offsetToNavd88) ? "UNDEFINED" : doubleToChar(fileVdi.offsetToNavd88, buf),
                isUndefinedVertDatumValue(fileVdi.offsetToNavd88) ? "" : " ",
                isUndefinedVertDatumValue(fileVdi.offsetToNavd88) ? "" : fileVdi.unit);
            return strdup(errorMessage);
        }
        if (!isUndefinedVertDatumValue(dataVdi.offsetToNavd88)) {
            if (!areEqual(getOffset(dataVdi.offsetToNavd88, dataVdi.unit, fileVdi.unit), fileVdi.offsetToNavd88, FLT_EPSILON)) {
                sprintf(
                    errorMessage,
                    " VERTICAL DATUM ERROR\n"
                    " Data VDI offset to NAVD-88 of %f %s conflicts with file VDI offset of %f %s.\n"
                    " No data stored.",
                    dataVdi.offsetToNavd88,
                    dataVdi.unit,
                    fileVdi.offsetToNavd88,
                    fileVdi.unit);
                return strdup(errorMessage);
            }
            if (dataVdi.offsetToNavd88IsEstimate != fileVdi.offsetToNavd88IsEstimate) {
                sprintf(
                    errorMessage,
                    " VERTICAL DATUM ERROR\n"
                    " Data VDI offset to NAVD-88 is estimated of %s conflicts with file VDI offset is estimated of %s.\n"
                    " No data stored.",
                    dataVdi.offsetToNavd88IsEstimate ? "TRUE" : "FALSE",
                    fileVdi.offsetToNavd88IsEstimate ? "TRUE" : "FALSE");
                return strdup(errorMessage);
            }
        }
        //-------------------------//
        // compare NGVD-29 offsets //
        //-------------------------//
        if (isUndefinedVertDatumValue(dataVdi.offsetToNgvd29) != isUndefinedVertDatumValue(fileVdi.offsetToNgvd29)) {
            sprintf(
                errorMessage,
                " VERTICAL DATUM ERROR\n"
                " Data VDI offset to NGVD-29 of %s%s%s conflicts with file VDI offset of %s%s%s.\n"
                " No data stored.",
                isUndefinedVertDatumValue(dataVdi.offsetToNgvd29) ? "UNDEFINED" : doubleToChar(dataVdi.offsetToNgvd29, buf),
                isUndefinedVertDatumValue(dataVdi.offsetToNgvd29) ? "" : " ",
                isUndefinedVertDatumValue(dataVdi.offsetToNgvd29) ? "" : dataVdi.unit,
                isUndefinedVertDatumValue(fileVdi.offsetToNgvd29) ? "UNDEFINED" : doubleToChar(fileVdi.offsetToNgvd29, buf),
                isUndefinedVertDatumValue(fileVdi.offsetToNgvd29) ? "" : " ",
                isUndefinedVertDatumValue(fileVdi.offsetToNgvd29) ? "" : fileVdi.unit);
            return strdup(errorMessage);
        }
        if (!isUndefinedVertDatumValue(dataVdi.offsetToNgvd29)) {
            if (!areEqual(getOffset(dataVdi.offsetToNgvd29, dataVdi.unit, fileVdi.unit), fileVdi.offsetToNgvd29, FLT_EPSILON)) {
                sprintf(
                    errorMessage,
                    " VERTICAL DATUM ERROR\n"
                    " Data VDI offset to NGVD-29 of %f %s conflicts with file VDI offset of %f %s.\n"
                    " No data stored.",
                    dataVdi.offsetToNgvd29,
                    dataVdi.unit,
                    fileVdi.offsetToNgvd29,
                    fileVdi.unit);
                return strdup(errorMessage);
            }
            if (dataVdi.offsetToNgvd29IsEstimate != fileVdi.offsetToNgvd29IsEstimate) {
                sprintf(
                    errorMessage,
                    " VERTICAL DATUM ERROR\n"
                    " Data VDI offset to NGVD-29 is estimated of %s conflicts with file VDI offset is estimated of %s.\n"
                    " No data stored.",
                    dataVdi.offsetToNgvd29IsEstimate ? "TRUE" : "FALSE",
                    fileVdi.offsetToNgvd29IsEstimate ? "TRUE" : "FALSE");
                return strdup(errorMessage);
            }
        }
    }
    //------------------------------------------------------------//
    // test whether current datum is compatible with native datum //
    //------------------------------------------------------------//
    int compatibleCurrentDatum = FALSE;
    if (!strcmp(currentDatum, CVERTICAL_DATUM_UNSET)               // current datum == UNSET
        || !strcmp(currentDatum, dataNativeDatum)                  // || current datum == data native datum
        || (!strcmp(dataNativeDatum, CVERTICAL_DATUM_UNSET)        // || data native datum == UNSET
            && (!strcmp(currentDatum, fileNativeDatum)             //    && current datum == file native datum
                || !strcmp(fileNativeDatum, CVERTICAL_DATUM_UNSET) //       || file native datum == UNSET
                || vdiOverwrite))                                  //       || vdiOverwrite
        || !strcmp(currentDatum, CVERTICAL_DATUM_NAVD88)           // || current datum == NAVD-88
        || !strcmp(currentDatum, CVERTICAL_DATUM_NGVD29)) {        // || current datum == NGVD-29
        compatibleCurrentDatum = TRUE;
    }
    if (!compatibleCurrentDatum) {
        sprintf(
            errorMessage,
            " VERTICAL DATUM ERROR\n"
            " Current datum of %s conflicts with %s native datum of %s.\n"
            " No data stored.",
            currentDatum,
            strcmp(dataNativeDatum, CVERTICAL_DATUM_UNSET) ? "data" : "file",
            strcmp(dataNativeDatum, CVERTICAL_DATUM_UNSET) ? dataNativeDatum : fileNativeDatum);
        return strdup(errorMessage);
    }
    //--------------------------------------------//
    // test whether we have a valid offset to use //
    //--------------------------------------------//
    verticalDatumInfo* targetVdi = NULL;
    if (!strcmp(currentDatum, dataNativeDatum) && !strcmp(fileNativeDatum, CVERTICAL_DATUM_UNSET)) {
        *offsetToUse = 0;
    }
    else {
        if (strcmp(dataNativeDatum, CVERTICAL_DATUM_UNSET) || vdiOverwrite) {
            targetVdi = &dataVdi;
        }
        else {
            targetVdi = &fileVdi;
        }
        if (!strcmp(currentDatum, CVERTICAL_DATUM_UNSET)         // current datum == UNSET
            || !strcmp(currentDatum, targetVdi->nativeDatum)) {  // || current datum == native datum
            *offsetToUse = 0;
        }
        else if (!strcmp(currentDatum, CVERTICAL_DATUM_NAVD88)) {
            *offsetToUse = getOffset(targetVdi->offsetToNavd88, targetVdi->unit, dataUnit);
        }
        else if (!strcmp(currentDatum, CVERTICAL_DATUM_NGVD29)) {
            *offsetToUse = getOffset(targetVdi->offsetToNgvd29, targetVdi->unit, dataUnit);
        }
    }
    if (*offsetToUse == UNDEFINED_VERTICAL_DATUM_VALUE) {
        sprintf(
            errorMessage,
            " VERTICAL DATUM ERROR\n"
            " No offset information. Cannot convert data from %s to %s\n"
            " No data stored.",
            currentDatum,
            targetVdi->nativeDatum);
        return strdup(errorMessage);
    }
    //-------------------------------------//
    // test whether the data unit is valid //
    //-------------------------------------//
    if ((strcmp(fileNativeDatum, CVERTICAL_DATUM_UNSET) || strcmp(dataNativeDatum, CVERTICAL_DATUM_UNSET))
        && (!unitIsFeet(dataUnit) && !unitIsMeters(dataUnit))) {
        sprintf(
            errorMessage,
            " VERTICAL DATUM ERROR\n"
            " Data unit of %s is not recognized as feet or meters.\n"
            " No data stored.",
            dataUnit);
        return strdup(errorMessage);
    }
    return NULL;
}

int* copyVdiFromLocationStructToUserHeader(
    zStructLocation* locStruct,
    int* userHeader,
    int* userHeaderNumber,
    int* status) {

    if (locStruct->supplemental) {
        *status = 0;
        char* compressed = extractFromDelimitedString(&locStruct->supplemental, VERTICAL_DATUM_INFO_USER_HEADER_PARAM, ":", TRUE, FALSE, ';');
        if (compressed) {
            do {
                char* headerBuf = userHeaderToString(userHeader, *userHeaderNumber);
                int len;
                if (headerBuf == NULL) {
                    len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 3;
                    headerBuf == malloc(len);
                    if (headerBuf == NULL) {
                        free(compressed);
                        status = -1;
                        break;
                    }
                }
                else {
                    len = strlen(headerBuf);
                }
                if (-1 == insertIntoDelimitedString(&headerBuf, len, VERTICAL_DATUM_INFO_USER_HEADER_PARAM, compressed, ":", FALSE, ';')) {
                    // insufficent space
                    len = strlen(headerBuf) + VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 4;
                    char* cp = realloc(headerBuf, len);
                    if (cp == NULL) {
                        free(compressed);
                        free(headerBuf);
                        status = -2;
                        break;
                    }
                    headerBuf = cp;
                    if (insertIntoDelimitedString(&headerBuf, len, VERTICAL_DATUM_INFO_USER_HEADER_PARAM, compressed, ":", FALSE, ';')) {
                        // unexpected error
                        free(compressed);
                        status = -3;
                        return userHeader;
                    }
                }
                free(compressed);
                free(userHeader);
                userHeader = stringToUserHeader(headerBuf, userHeaderNumber);
                free(headerBuf);
            } while (FALSE);
        }
    }
    return userHeader;
}
