#ifndef __VERTICAL_DATUM_INFO_INCLUDED__
#define __VERTICAL_DATUM_INFO_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif
 
#include <float.h>
#include <stdint.h>
 
#if defined(_MSC_VER)
        #ifdef _WIN64
                typedef int64_t slen_t;
        #else
                typedef int32_t slen_t;
        #endif
#else
    # if __GNUC__
        #if __GNUC__ > 7
            typedef size_t slen_t;
        #else
            typedef int slen_t;
        #endif
    #else
        typedef int32_t slen_t;
    #endif
#endif
 
#define F2C(f, c, flen, clen) {                  \
    int len = flen < clen ? flen : clen - 1;     \
    int i;                                       \
    strncpy(c, f, len);                          \
    c[len] = '\0';                               \
    for (i = len-1; i > -1; --i) {               \
        if (c[i] != ' ' ) break;                 \
        c[i] = '\0';                             \
    }                                            \
}
#define C2F(c, f, flen) {                        \
    int i;                                       \
    if (c == NULL) {                             \
        for (i = 0; i < flen; ++i) f[i] = ' ';   \
    }                                            \
    else {                                       \
        i = strlen(c);                           \
        int len = i > flen ? flen : i;           \
        strncpy(f, c, len);                      \
        for (i = len; i < flen; ++i) f[i] = ' '; \
    }                                            \
}
 
#define FALSE 0
#define TRUE  1
#define UNDEFINED -FLT_MAX
 
//-----------------------------//
// Vertical datum constants    //
//                             //
// Same order as DataContainer //
//-----------------------------//
#define IVERTICAL_DATUM_UNSET  0
#define IVERTICAL_DATUM_NAVD88 1
#define IVERTICAL_DATUM_NGVD29 2
#define IVERTICAL_DATUM_OTHER  3
#define IVERTICAL_DATUM_LOCAL  IVERTICAL_DATUM_OTHER

#define CVERTICAL_DATUM_UNSET  "UNSET"
#define CVERTICAL_DATUM_NAVD88 "NAVD-88"
#define CVERTICAL_DATUM_NGVD29 "NGVD-29"
#define CVERTICAL_DATUM_OTHER  "OTHER"
#define CVERTICAL_DATUM_LOCAL  CVERTICAL_DATUM_OTHER

//----------------//
// error messages //
//----------------//
#define BASE64_DECODING_ERROR                           "Base64 decoding error"
#define BASE64_ENCODING_ERROR                           "Base64 encoding error"
#define BEGINNING_BOUNDARY_NOT_FOUND                    "Beginning boundary not found"
#define ENDING_BOUNDARY_NOT_FOUND                       "Ending boundary not found"
#define ERROR_ON_DEFLATE                                "Error on Deflate())"
#define ERROR_ON_DEFLATEEND                             "Error on DeflateEnd()"
#define ERROR_ON_DEFLATEINIT2                           "Error on DeflateInit2()"
#define ERROR_ON_INFLATE                                "Error on Inflate())"
#define ERROR_ON_INFLATEEND                             "Error on InflateEnd()"
#define ERROR_ON_INFLATEINIT2                           "Error on InflateInit2()"
#define INPUT_STRING_IS_NULL                            "Input string is NULL"
#define INVALID_DATUM_IN_SPECIFIED_IN_XML               "Invalid datum in <to-datum>...</to-datum> block in XML"
#define INVALID_ELEVATION_UNIT_IN_XML                   "Invalid elevation unit in XML"
#define INVALID_ELEVATION_VALUE_IN_XML                  "Invalid elevation value in XML"
#define INVALID_NATIVE_DATUM_IN_XML                     "Invalid native datum in XML"
#define INVALID_NAVD_88_OFFSET_BLOCK_IN_XML             "Invalid NAVD-88 offset block in XML"
#define INVALID_NAVD_88_OFFSET_VALUE_IN_XML             "Invalid NAVD-88 offset value in XML"
#define INVALID_NGVD_29_OFFSET_BLOCK_IN_XML             "Invalid NGVD-29 offset block in XML"
#define INVALID_NGVD_29_OFFSET_VALUE_IN_XML             "Invalid NGVD-29 offset value in XML"
#define INVALID_OFFSET_BLOCK_IN_XML                     "Invalid <offset>...</offset> block in XML"
#define INVALID_XML_STRUCTURE                           "Invalid XML Structure"
#define MULTIPLE_NAVD_88_OFFSET_BLOCKS_IN_XML           "Multiple NAVD-88 offset blocks in XML"
#define MULTIPLE_NGVD_29_OFFSET_BLOCKS_IN_XML           "Multiple NGVD-29 offset blocks in XML"
#define NO_ELEVATION_UNIT_IN_XML                        "No elevation unit in XML"
#define NO_NATIVE_DATUM_IN_XML                          "No native datum in XML"
#define NO_NAVD_88_OFFSET_VALUE_IN_XML                  "No NAVD-88 offset value in XML"
#define NO_NGVD_29_OFFSET_VALUE_IN_XML                  "No NGVD-29 offset value in XML"
#define XML_IS_NOT_A_VALID_VERTICAL_DATUM_INFO_INSTANCE "XML is not a valid vertical datum info instance"
#define XML_IS_NOT_WELL_FORMED                          "XML is not well formed"
 
#define MIN(a, b) ({         \
    __typeof__ (a) _a = (a); \
    __typeof__ (b) _b = (b); \
    _a < _b ? _a : _b;       \
})
 
typedef struct vertical_datum_info_s {
    char  native_datum[17];
    float elevation;
    char  unit[3];
    float offset_to_ngvd_29;
    int   offset_to_ngvd_29_is_estimate;
    float offset_to_navd_88;
    int   offset_to_navd_88_is_estimate;
} vertical_datum_info;

typedef struct text_boundary_info_s {
    char *first;
    char *first_non_blank;
    char *first_with_boundary;
    char *last;
    char *last_non_blank;
    char *last_with_boundary;
    int   offset;
    int   offset_non_blank;
    int   len;
    int   len_non_blank;
    int   len_with_boundaries;
} text_boundary_info;
/**
 * Returns a string representation of a DSS record user header
 * 
 * @param userHeader     The user header (integer array)
 * @param userHeaderSize The size of the user header in number of integers
 * 
 * @return The string representation of the user header. Memory for this buffer 
 *         is dynamically allocated using malloc() and must be freed using free().
 */
char *string_from_user_header(const int *userHeader, const int userHeaderSize);
/**
 * Creates a DSS record user header from a string
 * 
 * @param str            The string to create the user header from
 * @param userHeaderSize Pointer to an int that receives the number of integers allocated.
 *                       This should always be (len_str - 1) / 4 + 1.
 * 
 * @return The user header (array of integers).  Memory for this buffer is dynamically 
 *         allocated using malloc() and must be freed using free() unless it is a member
 *         zStructTime* structure. In that case the "allocated" structure member should
 *         be set to 1 to all zStructFree() to free the memory.
 */
int *string_to_user_header(const char *str, int *userHeaderSize);
/**
 * Returns the length of a buffer required to hold a base-64 encoding of an input buffer of the specified length.
 *
 * @param to_encode_len The length of the buffer to base64-encode
 *
 * @return -1 if to_encode_len < 0, otherwise the length of buffer necessary to hold the base-64 encoding.
 *         Always a multiple of 4. 
 */
int b64encoded_len(int to_encode_len);
/**
 * Returns the length of a buffer required to hold a base-64 decoding of an input buffer of the specified length.
 *
 * @param to_decode_len The length of the buffer to base64-decode. Must be a multiple of 4
 *
 * @return -1 if to_deocde_len < 0, -2 if to_decode_len is not a multiple of 4, otherwise the length of buffer
 *         necessary to hold the base-64 decoding. Any actual decoded length may be one or two bytes shorder
 *         depending on the actual input buffer (it will be shorter by the number of '=' characters at the end
 *         of the data to base64-decode).
 */
int b64decoded_len(int to_decode_len);
/**
 * Base64-encodes a buffer
 *
 * @param encoded       The encoded version of to_encode. Memory for this buffer is dynamically allocated
 *                      using malloc() and must be freed using free() by the caller if this call is successful.
 *                      This buffer is always a null-terminated string.
 * @param to_encode     A pointer to the buffer to encode
 * @param to_encode_len The length of the portion of to_encode to encode.
 *
 * @return -1 if to_encode_len < 0, otherwise 0 on success
 */
int b64encode(char **encoded, const char *to_encode, int to_encode_len);
/**
 * Base64-decodes a buffer
 *
 * @param decoded     The decoded data. Memory for this buffer is dynamically allocated using malloc()
 *                    and must be freed using free() by the caller if this call is successful.
 * @param decoded_len A pointer to an integer to receive the actual decoded length. This may be slightly less
 *                    than the value retuned by b64encoded_len(strlen(to_decode))
 * @param to_decode   A null-terminated string of the base64-encoded data
 *
 * @return -2 if strlen(to_decode) is not a multiple of 4, -3 if any character in to_decode is not a valie
 *         base64 encoding character, otherwise 0 on success
 */
int b64decode(char **decoded, int *decoded_len, const char *to_decode);
 
/**
 * Locates the first occurrence of text in buffer that is bounded by specified text strings;
 *
 * @param tbi    A pointer to an existing text_boundary_info structure to hold the the results
 * @param buf    The buffer to search in
 * @param after  The first boundary (the located text is immediately after this)
 * @param before The second boundary (the located text is immediately before this)
 *
 * @return An error message, which is NULL the function succeeds.
 */
char *find_text_between(text_boundary_info *tbi, const char *buf, const char *after, const char *before);
/**
 *  Returns a text string that has been gzipped and then base64 encoded to its original state
 *
 * @param results   The decoded and un-gzipped string. Memory for this buffer is dynamically allocated
 *                  using malloc() and must be freed using free() by the caller.
 * @param input_buf The buffer to decode and un-gzip.
 *
 * @return An error message, which is NULL if the function succeeds.
 */
char *decode_and_uncompress(char **results, const char *input_buf);
/**
 * Gzips and base64 encodes a text string
 *
 * @param results   The gzipped and encoded string. Memory for this buffer is dynamically allocated
 *                  using malloc() and must be freed using free() by the caller.
 * @param input_buf The buffer to gzip and encode.
 *
 * @return An error message, which is NULL if the function succeeds.
 */
char *compress_and_encode(char **results, const char *input_buf);
/**
 * Expands empty tags of the format <tag_name/> to <tag_name></tag_name> for purpose of easy
 * structure validation.
 *
 * @param output_buf The result of the expansion. Memory for this buffer is dynamically allocated
 *                   using malloc() and must be freed using free() by the caller.
 * @param input_buf  The XML instance to expand
 *
 * @return An error message, which is NULL the function succeeds.
 */
char *expand_empty_xml_tags(char **output_buf, const char *input_buf);
/**
 * Validates the well-formedness of an XML instance
 *
 * @param xml The XML instance to verify
 *
 * @return An error message, which is NULL the function succeeds.
 */
char *validate_xml_structure(const char *xml);
/**
 * Parses a standard vertical datum infomration XML instance into data structure
 *
 * @param vdi        A ponter to a previously existing vertical_datum_info sturcture
 * @param intput_str The XML instance to parse. This may be either a plain text XML instance or one
 *                   that has been gzipped and base64 encoded
 */
char *vertical_datum_info_from_string(vertical_datum_info *vdi, const char *input_str);
/**
 * Creates a (compressed or uncompressed) string from vertical datum information
 *
 * @param results             The string containing the vertical datum infomation . Memory for this buffer is
 *                            dynamically allocated using malloc() and must be freed using free() by the caller.
 * @param vdi                 A ponter to a previously existing vertical_datum_info sturcture containing the information
 * @param generate_compressed A flag (TRUE/FALSE) that specifies whether to generate a compressed string.
 *                            If FALSE, the result will be an XML instance containing the vertical datum information.
 *                            If TRUE, the XML will be gzipped and base64 encoded.
 *
 * @return An error message, which is NULL the function succeeds.
 */
char *vertical_datum_info_to_string(char **results, vertical_datum_info *vdi, int generate_compressed);
/**
 * Fortan wrapper for vertical_datum_info_from_string
 *
 * Use the following Fortran interface for this routine:
 *
 *  interface
 *      subroutine vertical_datum_info_from_string( &
 *          input_str,                              &
 *          native_datum,                           &
 *          unit,                                   &
 *          error_message,                          &
 *          elevation,                              &
 *          offset_ngvd_29,                         &
 *          offset_ngvd_29_is_estimate,             &
 *          offset_navd_88,                         &
 *          offset_navd_88_is_estimate)
 *          character (len = *),  intent(in)  :: compressed
 *          character (len = *),  intent(out) :: native_datum
 *          character (len = *),  intent(out) :: unit
 *          character (len = *),  intent(out) :: error_message
 *          real      (kind = 4), intent(out) :: offset_ngvd_29
 *          logical   (kind = 4), intent(out) :: offset_ngvd_29_is_estimate
 *          real      (kind = 4), intent(out) :: offset_navd_88
 *          logical   (kind = 4), intent(out) :: offset_navd_88_is_estimate
 *      end subroutine vertical_datum_info_from_string
 *  end interface
 *
 * @param input_str                  Fortran CHARACTER (LEN=*) input for input XML in raw or compressed format.
 * @param native_datum               Fortran CHARACTER (LEN=*) output for native datum. Length should be >= 16.
 * @param unit                       Fortran CHARACTER (LEN=*) output for unit of elevation and offsets. Length should be >= 2
 * @param error_message              Fortran CHARACTER (LEN=*) output for error message. Empty on success. Length should be >= 64
 * @param elevation                  Fortran REAL (KIND=4) output for elevation. UNDEFINED if no value in XML
 * @param offset_ngvd_29             Fortran REAL (KIND=4) output for the offset to NGVD-29. UNDEFINED if no value in XML
 * @param offset_ngvd_29_is_estimate Fortran LOGICAL (KIND=4) output for whether the offset to NGVD-29 is estimated
 * @param offset_navd_88             Fortran REAL (KIND=4) output for the offset to NAVD-88. UNDEFINED if no value in XML
 * @param offset_navd_88_is_estimate Fortran LOGICAL (KIND=4) output for whether the offset to NAVD-88 is estimated
 * @param len_input_str              Fortran hidden parameter for declared length of input_str parameter
 * @param len_native_datum           Fortran hidden parameter for declared length of native_datum parameter
 * @param len_unit                   Fortran hidden parameter for declared length of unit parameter
 * @param len_error_message          Fortran hidden parameter for declared length of error_message parameter
 */
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
        slen_t   len_error_message);
/**
 * Fortan wrapper for vertical_datum_info_to_string
 *
 * Use the following Fortran interface for this routine:
 *
 *  interface
 *      subroutine vertical_datum_info_to_string( &
 *          output_str,                           &
 *          native_datum,                         &
 *          unit,                                 &
 *          error_message,                        &
 *          elevation,                            &
 *          offset_ngvd_29,                       &
 *          offset_ngvd_29_is_estimate,           &
 *          offset_navd_88,                       &
 *          offset_navd_88_is_estimate            &
 *          generate_compressed)
 *          character (len = *),  intent(out) :: compressed
 *          character (len = *),  intent(in)  :: native_datum
 *          character (len = *),  intent(in)  :: unit
 *          character (len = *),  intent(out) :: error_message
 *          real      (kind = 4), intent(in)  :: offset_ngvd_29
 *          logical   (kind = 4), intent(in)  :: offset_ngvd_29_is_estimate
 *          real      (kind = 4), intent(in)  :: offset_navd_88
 *          logical   (kind = 4), intent(in)  :: offset_navd_88_is_estimate
 *          logical   (kind = 4), intent(in)  :: generate_compressed
 *      end subroutine vertical_datum_info_from_string
 *  end interface
 *
 * @param output_str                 Fortran CHARACTER (LEN=*) output in raw (XML) or compressed format. Length should be >= 400
 * @param native_datum               Fortran CHARACTER (LEN=*) input for native datum.
 * @param unit                       Fortran CHARACTER (LEN=*) input for unit of elevation and offsets.
 * @param error_message              Fortran CHARACTER (LEN=*) output for error message. Empty on success. Length should be >= 64
 * @param elevation                  Fortran REAL (KIND=4) input for elevation. UNDEFINED if unknown or n/a
 * @param offset_ngvd_29             Fortran REAL (KIND=4) input for the offset to NGVD-29. UNDEFINED if n/a
 * @param offset_ngvd_29_is_estimate Fortran LOGICAL (KIND=4) input for whether the offset to NGVD-29 is estimated
 * @param offset_navd_88             Fortran REAL (KIND=4) input for the offset to NAVD-88. UNDEFINED if n/a
 * @param offset_navd_88_is_estimate Fortran LOGICAL (KIND=4) input for whether the offset to NAVD-88 is estimated
 * @param generate_compressed        Fortran LOGICAL (KIND=4) input for whether to generate compressed or raw (XML) string
 * @param len_output_str             Fortran hidden parameter for declared length of output_str parameter
 * @param len_native_datum           Fortran hidden parameter for declared length of native_datum parameter
 * @param len_unit                   Fortran hidden parameter for declared length of unit parameter
 * @param len_error_message          Fortran hidden parameter for declared length of error_message parameter
 */
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
        slen_t   len_error_message);
 
#ifdef __cplusplus
} // extern "C"
#endif
 
#endif //__VERTICAL_DATUM_INFO_INCLUDED__