#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <zlib.h>
#include <verticalDatum.h>
#include <hecdssInternal.h>
#include <heclib.h>

//
// The 64 valid characters used in base64 encoding (excluding pad character '='), in index order
//
static const unsigned char *base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
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
const char *footUnitAliases[]  = {"FT","FEET","FOOT"};
const char *meterUnitAliases[] = {"M","METER","METERS","METRE","METRES"};
const int footUnitAliasCount = sizeof(footUnitAliases) / sizeof(footUnitAliases[0]);
const int meterUnitAliasCount = sizeof(meterUnitAliases) / sizeof(meterUnitAliases[0]);

void *_malloc(size_t size) {
    void *buf = malloc(size);
    memset(buf, 0, size);
    return buf;
}

void *_calloc(size_t num, size_t size) {
    return _malloc((int)num * (int)size);
}
//
// See verticalDatum.h for documentation
//
int unitIsFeet(const char *unit) {
    for (int i = 0; i < footUnitAliasCount; ++i) {
        if (!strcasecmp(unit, footUnitAliases[i])) {
            return TRUE;
        }
    }
    return FALSE;
}
//
// Fortran wrapper for unitIsFeet
//
int unitisfeet_(char *unit, slen_t lenUnit) {
    char *cUnit = (char *)_malloc(lenUnit+1);
    F2C(unit, cUnit, lenUnit, lenUnit+1);
    int isFeet = unitIsFeet(cUnit);
    free(cUnit);
    return isFeet;
}
//
// See verticalDatum.h for documentation
//
int unitIsMeters(const char *unit) {
    for (int i = 0; i < meterUnitAliasCount; ++i) {
        if (!strcasecmp(unit, meterUnitAliases[i])) {
            return TRUE;
        }
    }
    return FALSE;
}
//
// Fortran wrapper for unitIsMeters
//
int unitismeters_(char *unit, slen_t lenUnit) {
    char *cUnit = (char *)_malloc(lenUnit+1);
    F2C(unit, cUnit, lenUnit, lenUnit+1);
    int isMeters = unitIsMeters(cUnit);
    free(cUnit);
    return isMeters;
}
//
// See verticalDatum.h for documentation
//
double getOffset(double offset, const char *offsetUnit, const char *_dataUnit) {
    int dataInFeet     = 0;
    int dataInMeters   = 0;
    int offsetInFeet   = 0;
    int offsetInMeters = 0;
    // blank trim the data unit (shouldn't have to do this)
    char *dataUnit = (char *)_malloc(strlen(_dataUnit)+1);
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
        if (offsetInFeet)   return offset * METERS_PER_FOOT;
        if (offsetInMeters) return offset;
        return UNDEFINED_VERTICAL_DATUM_VALUE;
    }
    else {
        return UNDEFINED_VERTICAL_DATUM_VALUE;
    }
}
void getoffset_(
        double *offset,
        const char *offsetUnit,
        const char *dataUnit,
        slen_t lenOffsetUnit,
        slen_t lenDataUnit) {

    char *cOffsetUnit = (char *)_malloc(lenOffsetUnit+1);
    char *cDataUnit   = (char *)_malloc(lenDataUnit+1);
    F2C(offsetUnit, cOffsetUnit, lenOffsetUnit, lenOffsetUnit+1);
    F2C(dataUnit, cDataUnit, lenDataUnit, lenDataUnit+1);
    *offset = getOffset(*offset, cOffsetUnit, cDataUnit);
    free(cOffsetUnit);
    free(cDataUnit);
}

#if !defined(__APPLE__) && !defined(__sun__)
    const char *strcasestr(const char *haystack, const char *needle) {
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
//
// See verticalDatum.h for documentation
//
char *extractFromDelimitedString(
        char      **delimitedString,
        const char *parameter,
        const char *separator,
        int         matchCase,
        int         removeFromString,
        char        delimiter) {

    int len = strlen(parameter) + 1;
    if (separator) {
        len += strlen(separator);
    }
    char *param = _malloc(len);
    strcpy(param, parameter);
    if (separator) {
        strcat(param, separator);
    }
    char *value = NULL;
    char *paramStart = matchCase ? (char *)strstr(*delimitedString, param) : (char *)strcasestr(*delimitedString, param);
    if (paramStart) {
        char *valueStart = paramStart + strlen(param);
        char *valueEnd;
        for(valueEnd = valueStart+1; *valueEnd && *valueEnd != delimiter; ++valueEnd);
        len = valueEnd - valueStart;
        value = (char *)_malloc(len + 1);
        strncpy(value, valueStart, len);
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
int insertIntoDelimitedString(
        char     **delimitedString,
        int        delimitedStringSize,
        const char *parameter,
        const char *value,
        const char *separator,
        int         overwriteExisting,
        char        delimiter) {

    if (!delimitedString) {
        return -2;
    }

    char *existing = extractFromDelimitedString(
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
    int toInsertLen = strlen(parameter) + 1; // account for delimiter
    if (separator) {
        toInsertLen += strlen(separator);
    }
    if (value) {
        toInsertLen += strlen(value);
    }
    int availableLen = delimitedStringSize - strlen(*delimitedString);
    if (existing) {
        availableLen += strlen(parameter) + strlen(existing);
        if (separator) {
            availableLen += strlen(separator);
        }
    }
    if (availableLen < toInsertLen) {
        if (existing) {
            free(existing);
        }
        return -1;
    }
    if (existing && overwriteExisting) {
        char *dummy = extractFromDelimitedString(delimitedString, parameter, separator, TRUE, TRUE, delimiter);
        free(dummy);
    }
    if (strlen(*delimitedString) > 0 && (*delimitedString)[strlen(*delimitedString)-1] != delimiter) {
        sprintf(*delimitedString+strlen(*delimitedString), "%c", delimiter);
    }
    strcat(*delimitedString, parameter);
    if (separator) {
        strcat(*delimitedString, separator);
    }
    if (value) {
        strcat(*delimitedString, value);
    }
    if (delimiter) {
        strncat(*delimitedString, &delimiter, 1);
    }
    if (existing) {
        free(existing);
    }
    return 0;
}
//
// See verticalDatum.h for documentation
//
int *stringToUserHeader(const char *str, int *intCount) {
    int  numBytes = strlen(str);
	int numInts = (numBytes-1) / 4 + 1;
    int *userHeader = NULL;
    if (numInts > 0) {
        userHeader = (int *)_calloc(numInts, 4);
        int *buf = (int *)_calloc(numInts, 4);
		strcpy((char *)userHeader, str);
		if (ntohl(0x12345678) == 0x12345678) {
			// big endian
			uint32_t *_4bytes = (uint32_t *)userHeader;
			for (int i = 0; i < numInts; ++i) {
				BYTESWAP(*_4bytes++);
			}
		}
    }
    *intCount = numInts;
    return userHeader;
}
//
// See verticalDatum.h for documentation
//
char *userHeaderToString(const int *userHeader, const int userHeaderSize) {
    char *str = NULL;
    if (userHeader != NULL && userHeaderSize > 0) {
		int *buf = (int *)calloc(userHeaderSize, 4);
		memcpy(buf, userHeader, 4 * userHeaderSize);
		if (ntohl(0x12345678) == 0x12345678) {
			// big endian
			uint32_t *_4bytes = (uint32_t *)buf;
			for (int i = 0; i < userHeaderSize; ++i) {
				BYTESWAP(*_4bytes++);
			}
		}
        char *start = (char *)buf;
        char *cp;
        int   len;
        for (cp = start; *cp && cp - start < userHeaderSize * 4; ++cp);
        while (*(cp-1) == ' ') --cp;
        len = cp - start;
        str = _malloc(len+1);
        strncpy(str, start, len);
        str[len] = '\0';
		free(buf);
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
    return toDecodeLen / 4 * 3; // maximum length, may be 1 ro 2 less
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
int b64Encode(char **encoded, const char *toEncode, int toEncodeLen) {
    int len = b64EncodedLen(toEncodeLen);
    if (len < 0) return len;
    *encoded = (char *)_malloc(len+1);
    const char *i = toEncode;
    char *o = *encoded;
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
int b64Decode(char **decoded, int *decodedLen, const char *toDecode) {
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
    *decoded = (char *)_malloc(strlen(toDecode));
    const char *c = toDecode;
    char i[4];
    char *o = *decoded;
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
char *findTextBetween(textBoundaryInfo *tbi, const char *buf, const char *after, const char *before) {
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
// See verticalDatum.h for documentation
//
char *decodeAndGunzip(char **results, const char *inputBuf) {
    char  textBuf[4096];
    int   decodedLen;
    char *decodedBuf;
    char *outputBuf;
    int   rc;
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
    *results = (char *)_malloc(zstr.total_out+1);
    textBuf[zstr.total_out] = '\0';
    strcpy(*results, textBuf);
    free(decodedBuf);
    return NULL;
}
//
// See verticalDatum.h for documentation
//
char *gzipAndEncode(char **results, const char *inputBuf) {
    char *compressedBuf;
    char *encodedBuf;
    int   rc;
    int   inputLen = strlen(inputBuf)+1;
    int   outputLen;
    z_stream zstr;

    //-----------------------------------//
    // setup structure for gzip compress //
    //-----------------------------------//
    compressedBuf = (char *)_malloc(inputLen);
    zstr.zalloc    = Z_NULL;
    zstr.zfree     = Z_NULL;
    zstr.opaque    = Z_NULL;
    zstr.avail_in  = inputLen;
    zstr.next_in   = (char *)inputBuf;
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
    *results = (char *)_malloc(strlen(encodedBuf)+1);
    strcpy(*results, encodedBuf);
    free(compressedBuf);
    free(encodedBuf);
    return NULL;
}
//
// See verticalDatum.h for documentation
//
char *expandEmptyXmlTags(char **outputBuf, const char *inputBuf) {
    const char *in;
    char *out;
    int   tagBufLen = 32;
    char *tagBuf = (char *)_malloc(tagBufLen);
    int   xmlBufLen = strlen(inputBuf) * 3;
    char *xmlBuf = (char *)_malloc(xmlBufLen);
    int   inTag = FALSE;
    int   tagPos;
    char *tagChar;
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
                        tagBuf = (char *)realloc(tagBuf, tagBufLen);
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
char *validateXmlStructure(const char *xml) {
    int    size = 20;
    int    count = 0;
    char **tagNames = (char **)_malloc(size * sizeof(char *));
    char  *buf = (char *)_malloc(strlen(xml)+1);
    char  *cp1;
    char  *cp2;
    int    len;
    int    first = TRUE;

    strcpy(buf, xml);
    while (TRUE) {
        char *saveptr;
        cp1 = first ? strtok_r(buf, "<", &saveptr) : strtok_r(NULL, "<", &saveptr);
        first = FALSE;
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
                tagNames = (char **)realloc(tagNames, size * sizeof(char *));
            }
            len = cp2 - cp1;
            tagNames[count-1] = (char *)_malloc(len+1);
            strncpy(tagNames[count-1], cp1, len);
            tagNames[count-1][len] = '\0';
        }
    }
    free(tagNames);
    for (int i = 0; i < count; ++i) {
        free(tagNames[i]);
    }
    free(buf);
    return NULL;
}
//
// See verticalDatum.h for documentation
//
char *stringToVerticalDatumInfo(verticalDatumInfo *vdi, const char *inputStr) {
    char  *errmsg = NULL;
    char  *xml1;
    char  *xml;
    char   offsetBuf[2][128];
    int    freeXml1;
    double dtmp;
    textBoundaryInfo tbi;

    vdi->offsetToNgvd29 = UNDEFINED_VERTICAL_DATUM_VALUE;
    vdi->offsetToNgvd29IsEstimate = FALSE;
    vdi->offsetToNavd88 = UNDEFINED_VERTICAL_DATUM_VALUE;
    vdi->offsetToNavd88IsEstimate = FALSE;
    memset(vdi->nativeDatum, 0, sizeof(vdi->nativeDatum)); // null terminators will exist after strncpy()
    memset(vdi->unit, 0, sizeof(vdi->unit));                 // null terminators will exist after strncpy()

    memset(offsetBuf, 0, sizeof(offsetBuf));               // null terminators will exist after strncpy()

    int ngvd29OffsetProcessed = 0;
    int navd88OffsetProcessed = 0;

    if (inputStr == NULL || *inputStr == '\0') {
        vdi = NULL;
        return INPUT_STRING_IS_NULL;
    }
    if (strchr(inputStr, '<')) {
        xml1 = (char *)inputStr;
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
    else if (!strcmp(vdi->nativeDatum, CVERTICAL_DATUM_OTHER)) {
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
    else {
        free(xml);
        return INVALID_NATIVE_DATUM_IN_XML;
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
                return MISSING_OFFSET_BLOCK_IN_XML;
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
// See verticalDatum.h for documentation
//
char *verticalDatumInfoToString(char **results, verticalDatumInfo *vdi, int generateCompressed) {
    char  xml[4096];
    char *cp = xml;
    char *errmsg;

    sprintf(cp, "<vertical-datum-info unit=\"%s\">\n", vdi->unit);
    while (*cp)++cp;
    if (!strcmp(vdi->nativeDatum, "NGVD-29") ||
        !strcmp(vdi->nativeDatum, "NGVD-29") ||
        !strcmp(vdi->nativeDatum, "OTHER")) {

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
        *results = (char *)_malloc(strlen(xml)+1);
        strcpy(*results, xml);
    }
    return NULL;
}
//
// See verticalDatum.h for documentation
//
verticalDatumInfo *extractVerticalDatumInfoFromUserHeader(const int *userHeader, const int userHeaderSize) {
    verticalDatumInfo *vdi = NULL;
    char *cp = userHeaderToString(userHeader, userHeaderSize);
    if (cp) {
        char *vdiStr = extractFromDelimitedString(
            &cp,
            VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
            ":",
            FALSE,
            FALSE,
            ';');
        if (vdiStr) {
            vdi = (verticalDatumInfo *)_malloc(sizeof(verticalDatumInfo));
            char *errmsg = stringToVerticalDatumInfo(vdi, vdiStr);
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
//
// See verticalDatum.h for documentation
//
int	getEffectiveVerticalDatum(
        char  *cverticalDatum,
        int    cverticalDatumSize,
        int  **userHeader,
        int   *userHeaderSize,
        char **unit) {

    memset(cverticalDatum, 0, cverticalDatumSize);
    if (cverticalDatumSize < CVERTICAL_DATUM_SIZE) {
        return -1;
    }
    //----------------------------//
    // first get the global value //
    //----------------------------//
    int iverticalDatum;
    zquery("VDTM", cverticalDatum, cverticalDatumSize, &iverticalDatum);
    //-----------------------------//
    // next, check the user header //
    //-----------------------------//
    if (userHeader != NULL && *userHeader != NULL && *userHeaderSize > 0) {
        char *userHeaderString = userHeaderToString(*userHeader, *userHeaderSize);
        if (userHeaderString) {
            char *verticalDatum = extractFromDelimitedString(
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
                int *dummy = stringToUserHeader(userHeaderString, &newHeaderSize);
                free(dummy);
                sprintf((char *)*userHeader, "%s", userHeaderString); // safe because new string is always shorter than old string
                *userHeaderSize = newHeaderSize;
                free(verticalDatum);
            }
            free(userHeaderString);
        }
    }
    //---------------------------------------//
    // finally, check the unit specification //
    //---------------------------------------//
    if (unit != NULL && *unit != NULL) {
        if (strchr(*unit, '|')) {
            char  *saveptr;
            char *value;
            char *unitValue = NULL;
            char *verticalDatum = NULL;
            char *unitSpec = mallocAndCopy(*unit);
            char *key = strtok_r(unitSpec, "|=", &saveptr);
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
                sprintf(*unit, "%s", unitValue); // safe becuase new string is always shorter than old string
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
// See verticalDatum.h for documentation
//
void stringtoverticaldatuminfo_(
        char    *inputStr,
        char    *errorMessage,
        char    *nativeDatum,
        char    *unit,
        double  *offsetNgvd29,
        int32_t *offsetNgvd29IsEstimate,
        double  *offsetNavd88,
        int32_t *offsetNavd88IsEstimate,
        slen_t   lenInputStr,
        slen_t   lenErrorMessage,
        slen_t   lenNativeDatum,
        slen_t   lenUnit) {

    char *lInputStr = (char *)_malloc(lenInputStr+1);
    F2C(inputStr, lInputStr, lenInputStr, lenInputStr+1);
    char *errmsg;
    verticalDatumInfo vdi;

    errmsg = stringToVerticalDatumInfo(&vdi, lInputStr);
    if (errmsg) {
        C2F(errmsg, errorMessage, lenErrorMessage);
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
    free(lInputStr);
}
//
// See verticalDatum.h for documentation
//
void verticaldatuminfotostring_(
        char    *outputStr,
        char    *errorMessage,
        char    *nativeDatum,
        char    *unit,
        double  *offsetNgvd29,
        int32_t *offsetNgvd29IsEstimate,
        double  *offsetNavd88,
        int32_t *offsetNavd88IsEstimate,
        int32_t *generateCompressed,
        slen_t   lenErrorMessage,
        slen_t   lenOutputStr,
        slen_t   lenNativeDatum,
        slen_t   lenUnit) {

    char *errmsg;
    char *results;
    verticalDatumInfo vdi;
    F2C(nativeDatum, vdi.nativeDatum, lenNativeDatum, sizeof(vdi.nativeDatum));
    F2C(unit, vdi.unit, lenUnit, sizeof(vdi.unit));
    vdi.offsetToNgvd29 = *offsetNgvd29;
    vdi.offsetToNavd88IsEstimate = *offsetNgvd29IsEstimate;
    vdi.offsetToNavd88 = *offsetNavd88;
    vdi.offsetToNavd88IsEstimate = *offsetNavd88IsEstimate;
    errmsg = verticalDatumInfoToString(&results, &vdi, *generateCompressed);
    C2F(errmsg, errorMessage, lenErrorMessage);
    C2F(results, outputStr, lenOutputStr);
    free(results);
}
