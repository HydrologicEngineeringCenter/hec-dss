#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <zlib.h>
#include <verticalDatum.h>
#include <hecdssInternal.h>
 
//
// The 64 valid characters used in base64 encoding (excluding pad character '='), in index order
//
static const unsigned char *base64chars = "ABCDEFGHIGKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
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
 
//
// See verticalDatum.h for documentation
//
int *string_to_user_header(const char *str, int *intCount) {
    int  numBytes = strlen(str);
    int  numInts = numberIntsInBytes(numBytes);
    int *userHeader = (int *)calloc(numInts, 4);
    *intCount = numInts * 4;
    charInt ((void *)str, userHeader, numBytes, *intCount, 0, 1, 0);
    return userHeader;
}
//
// See verticalDatum.h for documentation
//
char *string_from_user_header(const int *userHeader, int userHeaderSize) {
    char *start = (char *)userHeader;
    char *cp;
    char *str;
    int   len;
    for (cp = start; *cp && cp - start < userHeaderSize * 4; ++cp);
    len = cp - start;
    str = malloc(len+1);
    strncpy(str, start, len);
    str[len] = '\0';
    return str;
}

//
// See verticalDatum.h for documentation
//
int b64encoded_len(int to_encode_len) {
    if (to_encode_len == 0) return 0;
    if (to_encode_len < 0) return -1;
    return ((to_encode_len - 1) / 3 + 1) * 4;
}
//
// See verticalDatum.h for documentation
//
int b64decoded_len(int to_decode_len) {
    if (to_decode_len < 0) return -1;
    if (to_decode_len % 4) return -2;
    return to_decode_len / 4 * 3; // maximum length, may be 1 ro 2 less
}
//
// byte operations for b64encode
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
int b64encode(char **encoded, const char *to_encode, int to_encode_len) {
    int len = b64encoded_len(to_encode_len);
    if (len < 0) return len;
    *encoded = (char *)malloc(len+1);
    const char *i = to_encode;
    char *o = *encoded;
    int remaining_len = to_encode_len;
    while (remaining_len > 0) {
        switch (remaining_len) {
            case 1 :
                o[0] = base64chars[i0_0];
                o[1] = base64chars[i0_1];
                o[2] = '=';
                o[3] = '=';
                o += 4;
                remaining_len = 0;
                break;
            case 2 :
                o[0] = base64chars[i0_0];
                o[1] = base64chars[i0_1 | i1_1];
                o[2] = base64chars[i1_2];
                o[3] = '=';
                o += 4;
                remaining_len = 0;
                break;
            default :
                o[0] = base64chars[i0_0];
                o[1] = base64chars[i0_1 | i1_1];
                o[2] = base64chars[i1_2 | i2_2];
                o[3] = base64chars[i2_3];
                i += 3;
                o += 4;
                remaining_len -= 3;
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
// byte operations for b64decode
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
int b64decode(char **decoded, int *decoded_len, const char *to_decode) {
    int len = b64decoded_len(strlen(to_decode));
    if (*(to_decode - 2) == '=') {
        len -= 2;
    }
    else if (*(to_decode - 1) == '=') {
        --len;
 
    }
    if (len < 0) return len;
    *decoded = (char *)malloc(strlen(to_decode));
    const char *c = to_decode;
    char i[4];
    char *o = *decoded;
    while (*c) {
        i[0] = base64bytes[c[0]];
        i[1] = base64bytes[c[1]];
        i[2] = base64bytes[c[2]];
        i[3] = base64bytes[c[3]];
        if (i[0] == 255 || i[1] == 255) {
            free(*decoded);
            *decoded_len = 0;
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
    *decoded_len = (c - to_decode);
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
char *find_text_between(text_boundary_info *tbi, const char *buf, const char *after, const char *before) {
    memset(tbi, 0, sizeof(text_boundary_info));
    tbi->first_with_boundary = strstr(buf, after);
    if (!tbi->first_with_boundary) {
        return BEGINNING_BOUNDARY_NOT_FOUND;
    }
    tbi->first = tbi->first_with_boundary + strlen(after);
    tbi->offset = tbi->first - buf;
    tbi->last = strstr(tbi->first, before);
    if (!tbi->last) {
        return ENDING_BOUNDARY_NOT_FOUND;
    }
    tbi->last_with_boundary = tbi->last + strlen(before);
    tbi->len = tbi->last - tbi->first;
    tbi->len_with_boundaries = tbi->last_with_boundary - tbi->first_with_boundary;
    for (tbi->first_non_blank = tbi->first;
         tbi->first_non_blank < tbi->last && isspace(*tbi->first_non_blank);
         tbi->first_non_blank++);
 
    if (tbi->first_non_blank == tbi->last) {
        tbi->last_non_blank = tbi->last;
        tbi->len_non_blank = 0;
    }
    else {
 
        for (tbi->last_non_blank = tbi->last;
             tbi->last_non_blank > tbi->first && isspace(*(tbi->last_non_blank - 1));
             tbi->last_non_blank--);
        tbi->len_non_blank = tbi->last_non_blank - tbi->first_non_blank;
    }
    tbi->offset_non_blank = tbi->first_non_blank - buf;
    return NULL;
}
//
// See verticalDatum.h for documentation
//
char *decode_and_uncompress(char **results, const char *input_buf) {
    char  text_buf[4096];
    int   decoded_len;
    char *decoded_buf;
    char *output_buf;
    int   rc;
    z_stream zstr;
    //--------------------------------//
    // first, Base64 decode the input //
    //--------------------------------//
    decoded_len = b64decoded_len(strlen(input_buf));
    if (b64decode(&decoded_buf, &decoded_len, input_buf)) {
        return BASE64_DECODING_ERROR;
    }
    //---------------------------------------------------//
    // next, setup the structure for the gzip decompress //
    //---------------------------------------------------//
    zstr.zalloc   = Z_NULL;
    zstr.zfree    = Z_NULL;
    zstr.opaque   = Z_NULL;
    zstr.next_in  = Z_NULL;
    zstr.avail_in = 0;
    rc = inflateInit2(&zstr, 16+MAX_WBITS);
    if (rc != Z_OK) {
        printf("Error code %d on inflateInit2(): %s", rc, zstr.msg);
        free(decoded_buf);
        return ERROR_ON_INFLATEINIT2;
    }
    //---------------------------------//
    // now perform the gzip decompress //
    //---------------------------------//
    zstr.next_in   = decoded_buf;
    zstr.avail_in  = decoded_len;
    zstr.next_out  = &text_buf[0];
    zstr.avail_out = sizeof(text_buf);
    rc = inflate(&zstr, Z_FINISH);
    if (rc != Z_STREAM_END) {
        printf("Error code %d on inflate(): %s", rc, zstr.msg);
        free(decoded_buf);
        return ERROR_ON_INFLATE;
    }
    rc = inflateEnd(&zstr);
    if (rc != Z_OK) {
        printf("Error code %d on inflateEnd(): %s", rc, zstr.msg);
        free(decoded_buf);
        return ERROR_ON_INFLATEEND;
    }
    //--------------------//
    // return the results //
    //--------------------//
    *results = (char *)malloc(zstr.total_out+1);
    text_buf[zstr.total_out] = '\0';
    strcpy(*results, text_buf);
    free(decoded_buf);
    return NULL;
}
//
// See verticalDatum.h for documentation
//
char *compress_and_encode(char **results, const char *input_buf) {
    char *compressed_buf;
    char *encoded_buf;
    int   rc;
    int   input_len = strlen(input_buf);
    int   output_len;
    z_stream zstr;
 
    //-----------------------------------//
    // setup structure for gzip compress //
    //-----------------------------------//
    zstr.zalloc   = Z_NULL;
    zstr.zfree    = Z_NULL;
    zstr.opaque   = Z_NULL;
    zstr.next_in  = Z_NULL;
    zstr.avail_in = 0;
    //-------------------------//
    // gzip compress the input //
    //-------------------------//
    rc = deflateInit2(&zstr, Z_DEFAULT_COMPRESSION, Z_DEFLATED, 16+MAX_WBITS, 8, Z_DEFAULT_STRATEGY);
    if (rc != Z_OK) {
        printf("Error code %d on deflateInit2(): %s", rc, zstr.msg);
        return ERROR_ON_DEFLATEINIT2;
    }
    compressed_buf = (char *)malloc(input_len);
    zstr.next_in   = (char *)input_buf;
    zstr.avail_in  = input_len;
    zstr.next_out  = compressed_buf;
    zstr.avail_out = input_len;
    rc = deflate(&zstr, Z_FINISH);
    if (rc != Z_STREAM_END) {
        printf("Error code %d on deflateInit2(): %s", rc, zstr.msg);
        free(compressed_buf);
        return ERROR_ON_DEFLATE;
    }
    rc = deflateEnd(&zstr);
    if (rc != Z_OK) {
        printf("Error code %d on deflateEnd(): %s", rc, zstr.msg);
        free(compressed_buf);
        return ERROR_ON_DEFLATEEND;
    }
    //----------------------------------//
    // Base64 encode the compressed buf //
    //----------------------------------//
    if (b64encode(&encoded_buf, compressed_buf, zstr.total_out)) {
        free(compressed_buf);
        return BASE64_ENCODING_ERROR;
    }
    *results = (char *)malloc(strlen(encoded_buf)+1);
    strcpy(*results, encoded_buf);
    free(compressed_buf);
    free(encoded_buf);
    return NULL;
}
//
// See verticalDatum.h for documentation
//
char *expand_empty_xml_tags(char **output_buf, const char *input_buf) {
    const char *in;
    char *out;
    int   tag_buf_len = 32;
    char *tag_buf = (char *)malloc(tag_buf_len);
    int   xml_buf_len = strlen(input_buf) * 3;
    char *xml_buf = (char *)malloc(xml_buf_len);
    int   in_tag = FALSE;
    int   tag_pos;
    char *tag_char;
    in  = input_buf;
    out = xml_buf;
    while (*in) {
        switch (*in) {
            case '<' :
                if (in_tag) {
                    free(tag_buf);
                    free(xml_buf);
                    return XML_IS_NOT_WELL_FORMED;
                }
                in_tag = TRUE;
                tag_pos = 0;
                memset(tag_buf, 0, tag_buf_len);
                *out++ = *in++;
                break;
            case '/' :
                switch (*(in+1)) {
                    case '\0':
                        free(tag_buf);
                        free(xml_buf);
                        return XML_IS_NOT_WELL_FORMED;
                    case '>' :
                        *out++ = '>';
                        *out++ = '<';
                        *out++ = '/';
                        tag_char = tag_buf;
                        while (*tag_char) {
                            *out++ = *tag_char++;
                        }
                        ++in;
                        break;
                    default :
                        *out++ = *in++;
                }
                break;
            case '>' :
                in_tag = FALSE;
                *out++ = *in++;
                break;
            default :
                if (isspace(*in)) {
                    in_tag = FALSE;
                }
                if (in_tag) {
                    if (tag_pos >= tag_buf_len) {
                        tag_buf_len *= 2;
                        tag_buf = (char *)realloc(tag_buf, tag_buf_len);
                    }
                    tag_buf[tag_pos++] = *in;
                }
                *out++ = *in++;
        }
    }
    free(tag_buf);
    *output_buf = xml_buf;
    return NULL;
}
//
// See verticalDatum.h for documentation
//
char *validate_xml_structure(const char *xml) {
    int    size = 20;
    int    count = 0;
    char **tag_names = (char **)malloc(size * sizeof(char *));
    char  *buf = (char *)malloc(strlen(xml)+1);
    char  *cp1;
    char  *cp2;
    int    len;
    int    first = TRUE;
 
    strcpy(buf, xml);
    while (TRUE) {
        cp1 = first ? strtok(buf, "<") : strtok(NULL, "<");
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
                free(tag_names[i]);
            }
            free(tag_names);
            return INVALID_XML_STRUCTURE;
        }
        if (*cp1 == '/') {
            //-----------------//
            // closing bracket //
            //-----------------//
            ++cp1;
            len = cp2 - cp1;
            if (strncmp(cp1, tag_names[count-1], len)) {
                return INVALID_XML_STRUCTURE;
            }
            free(tag_names[--count]);
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
                tag_names = (char **)realloc(tag_names, size * sizeof(char *));
            }
            len = cp2 - cp1;
            tag_names[count-1] = (char *)malloc(len+1);
            strncpy(tag_names[count-1], cp1, len);
            tag_names[count-1][len] = '\0';
        }
    }
    for (int i = 0; i < count; ++i) {
        free(tag_names[i]);
    }
    free(tag_names);
    return NULL;
}
//
// See verticalDatum.h for documentation
//
char *vertical_datum_info_from_string(vertical_datum_info *vdi, const char *input_str) {
    char *err_msg = NULL;
    char *xml1;
    char *xml;
    char  offset_buf[2][128];
    text_boundary_info tbi;
    int  free_xml1;
 
    vdi->elevation = UNDEFINED;
    vdi->offset_to_ngvd_29 = UNDEFINED;
    vdi->offset_to_ngvd_29_is_estimate = FALSE;
    vdi->offset_to_navd_88 = UNDEFINED;
    vdi->offset_to_navd_88_is_estimate = FALSE;
    memset(vdi->native_datum, 0, sizeof(vdi->native_datum)); // null terminators will exist after strncpy()
    memset(vdi->unit, 0, sizeof(vdi->unit));                 // null terminators will exist after strncpy()
 
    memset(offset_buf, 0, sizeof(offset_buf));               // null terminators will exist after strncpy()
 
    int ngvd_29_offset_processed = 0;
    int navd_88_offset_processed = 0;
 
    if (input_str == NULL) {
        vdi = NULL;
        return INPUT_STRING_IS_NULL;
    }
    if (strchr(input_str, '<')) {
        xml1 = (char *)input_str; 
        free_xml1 = FALSE; 
    }
    else {
        err_msg = decode_and_uncompress(&xml1, input_str);
        if (err_msg != NULL) {
            return err_msg;
        }
        free_xml1 = TRUE;
    }
    err_msg = expand_empty_xml_tags(&xml, xml1);
    if (free_xml1) {
        free(xml1);
    }
    if (err_msg) {
        return err_msg;
    }
    err_msg = validate_xml_structure(xml);
    if (err_msg) {
        return err_msg;
    }
 
    err_msg = find_text_between(&tbi, xml, "<vertical-datum-info", "</vertical-datum-info>");
    if (err_msg != NULL || tbi.len_non_blank == 0) {
        printf("%s\n", err_msg);
        free(xml);
        return XML_IS_NOT_A_VALID_VERTICAL_DATUM_INFO_INSTANCE;
    }
    //------------------------//
    // get the elevation unit //
    //------------------------//
    err_msg = find_text_between(&tbi, tbi.first, "unit=\"", "\"");
    if (err_msg != NULL || tbi.len_non_blank == 0) {
        printf("%s\n", err_msg);
        free(xml);
        return NO_ELEVATION_UNIT_IN_XML;
    }
    strncpy(vdi->unit, tbi.first_non_blank, MIN(tbi.len_non_blank, sizeof(vdi->unit)-1));
    if (strcmp(vdi->unit, "ft") && strcmp(vdi->unit, "m")) {
        free(xml);
        return INVALID_ELEVATION_UNIT_IN_XML;
    }
    //----------------------//
    // get the native datum //
    //----------------------//
    err_msg = find_text_between(&tbi, xml, "<native-datum>", "</native-datum>");
    if (err_msg != NULL || tbi.len_non_blank == 0) {
        printf("%s\n", err_msg);
        free(xml);
        return NO_NATIVE_DATUM_IN_XML;
    }
    strncpy(vdi->native_datum, tbi.first_non_blank, MIN(tbi.len_non_blank, sizeof(vdi->native_datum)-1));
    if (strcmp(vdi->native_datum, "NGVD-29") &&
        strcmp(vdi->native_datum, "NAVD-88") &&
        strcmp(vdi->native_datum, "OTHER"))
    {
            free(xml);
            return INVALID_NATIVE_DATUM_IN_XML;
    }
    if (!strcmp(vdi->native_datum, "OTHER")) {
        //---------------------------------------//
        // get the local datum name if it exists //
        //---------------------------------------//
        err_msg = find_text_between(&tbi, xml, "<local-datum-name>", "</local-datum-name>");
        if (err_msg == NULL && tbi.len_non_blank > 0) {
            size_t len = MIN(tbi.len_non_blank, sizeof(vdi->native_datum)-1);
            strncpy(vdi->native_datum, tbi.first_non_blank, len);
            vdi->native_datum[len] = '\0'; // because we're overwriting a previous value
        }
    }
    //--------------------------------//
    // get the elevation if it exists //
    //--------------------------------//
    err_msg = find_text_between(&tbi, xml, "<elevation>", "</elevation>");
    if (err_msg == NULL && tbi.len_non_blank > 0) {
        vdi->elevation = strtof(tbi.first_non_blank, &tbi.last_non_blank);
        if (tbi.last_non_blank == tbi.first_non_blank) {
            free(xml);
            return INVALID_ELEVATION_VALUE_IN_XML;
        }
    }
    //----------------------------------------//
    // get the sub-xml blocks for the offsets //
    //----------------------------------------//
    err_msg = find_text_between(&tbi, xml, "<offset", "</offset>");
    if (err_msg == NULL && tbi.len_non_blank != 0) {
        strncpy(offset_buf[0], tbi.first_with_boundary, tbi.len_with_boundaries);
        err_msg = find_text_between(&tbi, tbi.last_with_boundary, "<offset", "</offset>");
        if (err_msg == NULL && tbi.len_non_blank != 0) {
            strncpy(offset_buf[1], tbi.first_with_boundary, tbi.len_with_boundaries);
        }
    }
    //---------------------------//
    // process the offset blocks //
    //---------------------------//
    for (int i = 0; i < 2; ++i) {
        err_msg = find_text_between(&tbi, offset_buf[i], "<to-datum>", "</to-datum>");
        if (err_msg != NULL || tbi.len_non_blank == 0) {
            printf("%s\n", err_msg);
            free(xml);
            return INVALID_OFFSET_BLOCK_IN_XML;
        }
        if (!strncmp(tbi.first_non_blank, "NGVD-29", 7)) {
            if (ngvd_29_offset_processed) {
                free(xml);
                return MULTIPLE_NGVD_29_OFFSET_BLOCKS_IN_XML;
            }
            ngvd_29_offset_processed = 1;
            err_msg = find_text_between(&tbi, offset_buf[i], "<value>", "</value>");
            if (err_msg != NULL || tbi.len_non_blank == 0) {
                printf("%s\n", err_msg);
            free(xml);
            return NO_NGVD_29_OFFSET_VALUE_IN_XML;
            }
            vdi->offset_to_ngvd_29 = strtof(tbi.first_non_blank, &tbi.last_non_blank);
            if (tbi.last_non_blank == tbi.first_non_blank) {
                free(xml);
                return INVALID_NGVD_29_OFFSET_VALUE_IN_XML;
            }
            err_msg = find_text_between(&tbi, offset_buf[i], "estimate=\"", "\"");
            if (err_msg != NULL || tbi.len_non_blank == 0) {
                printf("%s\n", err_msg);
                free(xml);
                return INVALID_NGVD_29_OFFSET_BLOCK_IN_XML;
            }
            if (!strncmp(tbi.first_non_blank, "true", tbi.len_non_blank)) {
                vdi->offset_to_ngvd_29_is_estimate = TRUE;
            }
            else if (!strncmp(tbi.first_non_blank, "false", tbi.len_non_blank)) {
                vdi->offset_to_ngvd_29_is_estimate = FALSE;
            }
            else {
                free(xml);
                return INVALID_NGVD_29_OFFSET_BLOCK_IN_XML;
            }
        }
        else if (!strncmp(tbi.first_non_blank, "NAVD-88", 7)) {
            if (navd_88_offset_processed) {
                free(xml);
                return MULTIPLE_NAVD_88_OFFSET_BLOCKS_IN_XML;
            }
            navd_88_offset_processed = 1;
            err_msg = find_text_between(&tbi, offset_buf[i], "<value>", "</value>");
            if (err_msg != NULL || tbi.len_non_blank == 0) {
                printf("%s\n", err_msg);
                free(xml);
                return NO_NAVD_88_OFFSET_VALUE_IN_XML;
            }
            vdi->offset_to_navd_88 = strtof(tbi.first_non_blank, &tbi.last_non_blank);
            if (tbi.last_non_blank == tbi.first_non_blank) {
                free(xml);
                return INVALID_NAVD_88_OFFSET_VALUE_IN_XML;
            }
            err_msg = find_text_between(&tbi, offset_buf[i], "estimate=\"", "\"");
            if (err_msg != NULL || tbi.len_non_blank == 0) {
                printf("%s\n", err_msg);
                free(xml);
                return INVALID_NAVD_88_OFFSET_BLOCK_IN_XML;
            }
            if (!strncmp(tbi.first_non_blank, "true", tbi.len_non_blank)) {
                vdi->offset_to_navd_88_is_estimate = 1;
            }
            else if (!strncmp(tbi.first_non_blank, "false", tbi.len_non_blank)) {
                vdi->offset_to_navd_88_is_estimate = 0;
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
char *vertical_datum_info_to_string(char **results, vertical_datum_info *vdi, int generate_compressed) {
    char  xml[4096];
    char *cp = xml;
    char *err_msg;
 
    sprintf(cp, "<vertical-datum-info unit=\"%s\">\n", vdi->unit);
    while (*cp)++cp;
    if (!strcmp(vdi->native_datum, "NGVD-29") ||
        !strcmp(vdi->native_datum, "NGVD-29") ||
        !strcmp(vdi->native_datum, "OTHER")) {
 
        sprintf(cp, "  <native-datum>%s</native-datum>\n", vdi->native_datum);
    }
    else {
        sprintf(cp, "  <native-datum>OTHER</native-datum>\n  <local-datum-name>%s</local-datum-name>\n", vdi->native_datum);
    }
    while (*cp)++cp;
    if (vdi->elevation != UNDEFINED) {
        sprintf(cp, "  <elevation>%f</elevation>\n", vdi->elevation);
        while (*cp)++cp;
    }
    if (vdi->offset_to_ngvd_29 != UNDEFINED) {
        sprintf(
            cp,
            "  offset estimate=\"%c\">\n    <to-datum>NGVD-29</to-datum>\n    <value>%f</value>\n  </offset>\n",
            vdi->offset_to_ngvd_29_is_estimate ? 'T' : 'F',
            vdi->offset_to_ngvd_29);
        while (*cp)++cp;
    }
    if (vdi->offset_to_navd_88 != UNDEFINED) {
        sprintf(
            cp,
            "  offset estimate=\"%c\">\n    <to-datum>NAVD-88</to-datum>\n    <value>%f</value>\n  </offset>\n",
            vdi->offset_to_navd_88_is_estimate ? 'T' : 'F',
            vdi->offset_to_navd_88);
        while (*cp)++cp;
    }
    sprintf(cp, "</vertical-datup-info>");
    if (generate_compressed) {
        err_msg = compress_and_encode(results, xml);
        if (err_msg != NULL) {
            return err_msg;
        }
    }
    else {
        *results = (char *)malloc(strlen(xml)+1);
        strcpy(*results, xml);
    }
    return NULL;
}
//
// See verticalDatum.h for documentation
//
void vertical_datum_info_from_string_(
        const char *input_str,
        char    *native_datum,
        char    *unit,
        char    *error_message,
        float   *elevation,
        float   *offset_ngvd_29,
        int32_t *offset_ngvd_29_is_estimate,
        float   *offset_navd_88,
        int32_t *offset_navd_88_is_estimate,
        slen_t   len_input_str,
        slen_t   len_native_datum,
        slen_t   len_unit,
        slen_t   len_error_message) {
 
    char *l_input_str = (char *)malloc(len_input_str+1);
    char *err_msg;
    vertical_datum_info vdi;
 
    F2C(input_str, l_input_str, len_input_str, len_input_str+1);
    err_msg = vertical_datum_info_from_string(&vdi, l_input_str);
    C2F(err_msg, error_message, len_error_message);
    C2F(vdi.native_datum, native_datum, len_native_datum);
    C2F(vdi.unit, unit, len_unit);
    *elevation = vdi.elevation;
    *offset_ngvd_29 = vdi.offset_to_ngvd_29;
    *offset_ngvd_29_is_estimate = vdi.offset_to_ngvd_29_is_estimate;
    *offset_navd_88 = vdi.offset_to_navd_88;
    *offset_navd_88_is_estimate = vdi.offset_to_navd_88_is_estimate;
    free(l_input_str);
}
//
// See verticalDatum.h for documentation
//
void vertical_datum_info_to_string_(
        char    *output_str,
        char    *native_datum,
        char    *unit,
        char    *error_message,
        float   *elevation,
        float   *offset_ngvd_29,
        int32_t *offset_ngvd_29_is_estimate,
        float   *offset_navd_88,
        int32_t *offset_navd_88_is_estimate,
        int32_t *generate_compressed,
        slen_t   len_output_str,
        slen_t   len_native_datum,
        slen_t   len_unit,
        slen_t   len_error_message) {
         
    char *err_msg;
    char *results;
    vertical_datum_info vdi;
    F2C(native_datum, vdi.native_datum, len_native_datum, sizeof(vdi.native_datum));
    F2C(unit, vdi.unit, len_unit, sizeof(vdi.unit));
    vdi.elevation = *elevation;
    vdi.offset_to_ngvd_29 = *offset_ngvd_29;
    vdi.offset_to_navd_88_is_estimate = *offset_ngvd_29_is_estimate;
    vdi.offset_to_navd_88 = *offset_navd_88;
    vdi.offset_to_navd_88_is_estimate = *offset_navd_88;
    err_msg = vertical_datum_info_to_string(&results, &vdi, *generate_compressed);
    C2F(err_msg, error_message, len_error_message);
    C2F(results, output_str, len_output_str);
    free(results);
}