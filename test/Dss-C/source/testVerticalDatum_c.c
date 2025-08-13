#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <heclib.h>
#include <verticalDatum.h>
#include <math.h>

void testDelimitedStringOps();
void testGzipAndEncodingOps();
void testUserHeaderOps();
void testVerticalDatumInfoSerialization();
void testZsetZquery();
void testStoreRetrieveTimeSeries();
void testV6TimeSeiresWithMultipleVerticalDatums();
void testStoreRetrievePairedData();
void testCopyRecordWithVdi();

const int SRC = 0;
const int DST = 1;

int test_vertical_datums_c() {
    testDelimitedStringOps();
    testGzipAndEncodingOps();
    testUserHeaderOps();
    testVerticalDatumInfoSerialization();
    testZsetZquery();
    testStoreRetrieveTimeSeries();
    testV6TimeSeiresWithMultipleVerticalDatums();
    testStoreRetrievePairedData();
    testCopyRecordWithVdi();
    return 0;
}
void testDelimitedStringOps() {
    // test extract_from_delimited_string() and insertIntoDelimitedString()
    int text_size = 512;
    char *cp;
    char *text_value = "theFirstParameter:theFirstValue;theSecondParameter:theSecondValue;";
    char* text = (char*)malloc(text_size);
    if (text) {
        sprintf(text, "%s", text_value);
    }
    else {
        assert(text);
    }
    cp = extractFromDelimitedString(&text, "theFirstParameter", ":", TRUE, FALSE, ';');
    assert(!strcmp(cp, "theFirstValue"));
    free(cp);
    cp = extractFromDelimitedString(&text, "theSecondParameter", ":", TRUE, FALSE, ';');
    assert(!strcmp(cp, "theSecondValue"));
    free(cp);
    cp = extractFromDelimitedString(&text, "THEFIRSTPARAMETER", ":", FALSE, FALSE, ';');
    assert(!strcmp(cp, "theFirstValue"));
    free(cp);
    cp = extractFromDelimitedString(&text, "THEFIRSTPARAMETER", ":", FALSE, TRUE, ';');
    assert(!strcmp(cp, "theFirstValue"));
    assert(!strcmp(text, "theSecondParameter:theSecondValue;"));
    sprintf(text, "%s", text_value);
    free(cp);
    cp = extractFromDelimitedString(&text, "theSecondParameter", ":", TRUE, TRUE, ';');
    assert(!strcmp(cp, "theSecondValue"));
    free(cp);
    assert(!strcmp(text, "theFirstParameter:theFirstValue;"));
    assert(insertIntoDelimitedString(&text, text_size, "anotherParameter", "anotherValue", ":", FALSE, ';') ==  0);
    assert(!strcmp(text, "theFirstParameter:theFirstValue;anotherParameter:anotherValue;"));
    assert(insertIntoDelimitedString(&text, text_size, "theFirstParameter", "theFirstValue", ":", TRUE, ';') ==  0);
    assert(!strcmp(text, "anotherParameter:anotherValue;theFirstParameter:theFirstValue;"));
    assert(insertIntoDelimitedString(&text, text_size, "theFirstParameter", "THEFIRSTVALUE", ":", TRUE, ';') ==  0);
    assert(!strcmp(text, "anotherParameter:anotherValue;theFirstParameter:THEFIRSTVALUE;"));
    free(extractFromDelimitedString(&text, "theFirstParameter", ":", TRUE, TRUE, ';'));
    free(extractFromDelimitedString(&text, "anotherParameter", ":", TRUE, TRUE, ';'));
    assert(strlen(text) == 0);
    assert(insertIntoDelimitedString(&text, text_size, "anotherParameter", "anotherValue", ":", FALSE, ';') == 0);
    assert(strcmp(text, "anotherParameter:anotherValue;") == 0);
    strcpy(text, "verticalDatumInfo:H4sIAAAAAAAAAHWQUQvCIBSF3/crZO+brmYYOCEIeuuxd6krCE5hu/r7cxlFtXyScw7fB1cmmNBetWtuGuPYWG8Cid7iUBusVUWI9BptgtKr8+lybDZ7ST/SZQYOUs6CV7uub0XXc/Z8naTvcpkGY2ZAAjPaUSNklXYzPGy5xfDteiVlkLSLoFjLJC3fhUkLdJ2PU1zBHzJeiL/4reB81ZDD36Op6g4WwiaKSwEAAA==;verticalDatum:NGVD-29;");
    assert(insertIntoDelimitedString(&text, text_size, "verticalDatumInfo", "H4sIAAAAAAAAAHWQUQvCIBSF3/crZO+brmYYOCEIeuuxd6krCE5hu/r7cxlFtXyScw7fB1cmmNBetWtuGuPYWG8Cid7iUBusVUWI9BptgtKr8+lybDZ7ST/SZQYOUs6CV7uub0XXc/Z8naTvcpkGY2ZAAjPaUSNklXYzPGy5xfDteiVlkLSLoFjLJC3fhUkLdJ2PU1zBHzJeiL/4reB81ZDD36Op6g4WwiaKSwEAAA==", ":", TRUE, ';') ==  0);
    assert(!strcmp(text, "verticalDatum:NGVD-29;verticalDatumInfo:H4sIAAAAAAAAAHWQUQvCIBSF3/crZO+brmYYOCEIeuuxd6krCE5hu/r7cxlFtXyScw7fB1cmmNBetWtuGuPYWG8Cid7iUBusVUWI9BptgtKr8+lybDZ7ST/SZQYOUs6CV7uub0XXc/Z8naTvcpkGY2ZAAjPaUSNklXYzPGy5xfDteiVlkLSLoFjLJC3fhUkLdJ2PU1zBHzJeiL/4reB81ZDD36Op6g4WwiaKSwEAAA==;"));
    free(text);
}
void testGzipAndEncodingOps() {
    // test gzipAndEncode() and decodeAndGunzip()
    char *errmsg;
    char *compressed;
    char *expanded;
    char *input_text =
        "<vertical-datum-info office=\"SWT\" unit=\"ft\">\n"
        "  <location>PENS</location>\n"
        "  <native-datum>OTHER</native-datum>\n"
        "  <local-datum-name>Pensacola</local-datum-name>\n"
        "  <elevation>757</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>1.457</value>\n"
        "  </offset>\n"
        "  <offset estimate=\"false\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>1.07</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n";

    errmsg = gzipAndEncode(&compressed, input_text);
    if (errmsg != NULL) printf("%s\n", errmsg);
    assert(errmsg == NULL);
    errmsg = decodeAndGunzip(&expanded, compressed);
    if (errmsg != NULL) printf("%s\n", errmsg);
    assert(errmsg == NULL);
    assert(!strcmp(expanded, input_text));
    free(compressed);
    free(expanded);
}
void testUserHeaderOps() {
    // test stringToUserHeader() and userHeaderToString()
    int  *userHeader;
    int   userHeaderNumber;
    char *userHeaderStringIn;
    char *userHeaderStringOut;
    userHeaderStringIn = "0123456789abcdefABCDEF0123456789abcd";
    userHeader = stringToUserHeader(userHeaderStringIn, &userHeaderNumber);
    assert(userHeaderNumber == (strlen(userHeaderStringIn)-1)/4+1);
    userHeaderStringOut = userHeaderToString(userHeader, userHeaderNumber);
    assert(!strcmp(userHeaderStringIn, userHeaderStringOut));
    free(userHeader);
    free(userHeaderStringOut);
}
void testVerticalDatumInfoSerialization() {
    // test stringToVerticalDatumInfo() and verticalDatumInfoToString()
    verticalDatumInfo vdi;
    char *errmsg;
    char *xml1 =
        "<vertical-datum-info office=\"SWT\" unit=\"ft\">\n"
        "  <location>TULA</location>\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.2</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>0.3855</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n";
    char *xml2 =
        "<vertical-datum-info office=\"SWT\" unit=\"ft\">\n"
        "  <location>PENS</location>\n"
        "  <native-datum>OTHER</native-datum>\n"
        "  <local-datum-name>Pensacola</local-datum-name>\n"
        "  <elevation>757</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>1.457</value>\n"
        "  </offset>\n"
        "  <offset estimate=\"false\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>1.07</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n";

    errmsg = stringToVerticalDatumInfo(&vdi, xml1);
    if (errmsg != NULL) printf("%s\n", errmsg);
    assert(errmsg == NULL);
    assert(!strcmp(vdi.nativeDatum, CVERTICAL_DATUM_NGVD29));
    assert(vdi.offsetToNavd88 == 0.3855);
    assert(vdi.offsetToNavd88IsEstimate == 1);
    assert(vdi.offsetToNgvd29 == 0.f);
    assert(vdi.offsetToNgvd29IsEstimate == 0);

    errmsg = stringToVerticalDatumInfo(&vdi, xml2);
    if (errmsg != NULL) printf("%s\n", errmsg);
    assert(errmsg == NULL);
    assert(!strcmp(vdi.nativeDatum, "Pensacola"));
    assert(vdi.offsetToNavd88 == 1.457);
    assert(vdi.offsetToNavd88IsEstimate == 1);
    assert(vdi.offsetToNgvd29 == 1.07);
    assert(vdi.offsetToNgvd29IsEstimate == 0);

}
void testZsetZquery() {
    // test zset() and zquery() for vertical datums
    int   intVal;
    char  charVal[PARAMETER_NAME_SIZE];
    int   status;

    charVal[0] = '\0';

    zquery("VERS", charVal, sizeof(charVal), &intVal);
    assert(intVal == 7);

    //----------------------------------------------------//
    // test setting an illegal value (Jira issue DSS-122) //
    //----------------------------------------------------//
    status = zset("XXXX", "", 1);
    printf("zset returned %d\n", status);
    assert(status == STATUS_NOT_OKAY);

    //------------------------------------//
    // query values that haven't been set //
    //------------------------------------//
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_UNSET);
    assert(!strcmp(charVal, CVERTICAL_DATUM_UNSET));

    zquery("VDOW", "", 0, &intVal);
    assert(intVal == FALSE);

    //-------------------------//
    // test setting by integer //
    //-------------------------//
    zset("VDTM", "", 1);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_NAVD88);
    assert(!strcmp(charVal, CVERTICAL_DATUM_NAVD88));

    zset("VDTM", "", 2);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_NGVD29);
    assert(!strcmp(charVal, CVERTICAL_DATUM_NGVD29));

    zset("VDTM", "", 4);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_NGVD29);         // unchanged from previous call
    assert(!strcmp(charVal, CVERTICAL_DATUM_NGVD29)); // unchanged from previous call

    zset("VDTM", "", 3);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_OTHER);
    assert(intVal == IVERTICAL_DATUM_LOCAL);
    assert(!strcmp(charVal, CVERTICAL_DATUM_OTHER));
    assert(!strcmp(charVal, CVERTICAL_DATUM_LOCAL));

    zset("VDTM", "", 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_UNSET);
    assert(!strcmp(charVal, CVERTICAL_DATUM_UNSET));

    //----------------------//
    // test setting by name //
    //----------------------//
    zset("VDTM", CVERTICAL_DATUM_NAVD88, 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_NAVD88);
    assert(!strcmp(charVal, CVERTICAL_DATUM_NAVD88));

    zset("VDTM", CVERTICAL_DATUM_NGVD29, 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_NGVD29);
    assert(!strcmp(charVal, CVERTICAL_DATUM_NGVD29));

    zset("VDTM", CVERTICAL_DATUM_OTHER, 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_OTHER);
    assert(intVal == IVERTICAL_DATUM_LOCAL);
    assert(!strcmp(charVal, CVERTICAL_DATUM_OTHER));
    assert(!strcmp(charVal, CVERTICAL_DATUM_LOCAL));

    zset("VDTM", CVERTICAL_DATUM_LOCAL, 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_OTHER);
    assert(intVal == IVERTICAL_DATUM_LOCAL);
    assert(!strcmp(charVal, CVERTICAL_DATUM_OTHER));
    assert(!strcmp(charVal, CVERTICAL_DATUM_LOCAL));

    zset("VDTM", "Pensacola", 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_OTHER);
    assert(intVal == IVERTICAL_DATUM_LOCAL);
    assert(!strcmp(charVal, "Pensacola"));

    zset("VDTM", CVERTICAL_DATUM_UNSET, 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_UNSET);
    assert(!strcmp(charVal, CVERTICAL_DATUM_UNSET));

    zset("VDOW", "", TRUE);
    zquery("VDOW", "", 0, &intVal);
    assert(intVal == TRUE);

    zset("VDOW", "", FALSE);
    zquery("VDOW", "", 0, &intVal);
    assert(intVal == FALSE);
}
void testV6TimeSeiresWithMultipleVerticalDatums() {
    long long ifltab[250] = {0};
    int status;
    zStructTimeSeries* tss = NULL;
    verticalDatumInfo vdi;
    int OLD_API = 0;
    int NEW_API = 1;
    int REGULAR = 0;
    int IRREGULAR = 1;
    int NGVD29 = 0;
    int NAVD88 = 1;
    int UNSET = 2;
    int nativeDatum;
    char* errmsg;
    char* filename = "v6_mult_vert_datum_ts.dss";
    char* pathnames[2][2] = { {"//TESTTSLOC/ELEV//1DAY/MULTVERTICALDATUMS-OLD/", "//TESTTSLOC/ELEV//IR-YEAR/MULTVERTICALDATUMS-OLD/"},
                              {"//TESTTSLOC/ELEV//1DAY/MULTVERTICALDATUMS-NEW/", "//TESTTSLOC/ELEV//IR-YEAR/MULTVERTICALDATUMS-NEW/"} };
    int maxValueCount = 365;
    int numberValues = maxValueCount;
    char* startDate[2] = { "01Jan2021", "01Jan2022" };
    char* endDate[2] = { "31Dec2021", "31Dec2022" };
    char* startTime = "01:00";
    int startJul;
    int startMinutes;
    int endJul;
    int endMinutes;
    int jan_01_2021 = 44195;
    int jan_01_2021_0000 = 63642240; // 01Jan2021 00:00
    int jan_01_2021_0100 = 63642300; // 01Jan2021 01:00
    int dec_21_2021_0000 = 64152000; // 21Dec2021 00:00
    int jan_01_2022_0001 = 64167841; // 01Jan2022 00:01
    double* values = (double *)malloc(numberValues * sizeof(double));
    double* dv = NULL;
    int* times = (int*)malloc(numberValues * sizeof(int));
    int* t = NULL;
    char* unit = "ft";
    char unit2[16];
    char* dataType = "INST-VAL";
    char dataType2[16];
    char* compressed = NULL;
    char* headerBuf = NULL;
    int   len = 0;
    int messageLevel;
    char alpha[PARAMETER_NAME_SIZE];
    double expectedValue;
    int basedate;
    int* quality = (int*)malloc(numberValues * sizeof(int));
    int storeFlags = TRUE;
    int readFlags = TRUE;
    int userHeaderSize = 500;
    int* userHeader = (int*)malloc(userHeaderSize * sizeof(int));
    int* userHeader2 = NULL;
    int userHeaderNumber;
    int storeFlag = 0;
    int readFlag = 0;
    int offsetMinutes;
    int compressionType;
    int compressionBase = FALSE;
    float compressionBaseVal = 0.f;
    int compressionHigh = FALSE;
    int compressionPrec = 0;

    char* xml[] = {
        // NGVD-29
        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.0</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>0.5</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",
        // NAVD-88
        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NAVD-88</native-datum>\n"
        "  <elevation>615.5</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>-0.5</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",
        // UNSET
        ""
    };

    int xmlCount = sizeof(xml) / sizeof(xml[0]);

    remove(filename);
    strcpy(unit2, unit);
    strcpy(dataType2, dataType);
    zquery("MLVL", alpha, sizeof(alpha) - 1, &messageLevel);
    zset("MLVL", "", 1);
    status = zopen6(ifltab, filename);
    assert(status == 0);
    for (int api = OLD_API; api <= NEW_API; ++api) {
        printf("\nUsing %s for retrieving time series\n", api == OLD_API ? "OLD API" : "NEW API");
        for (int workingVerticalDatum = IVERTICAL_DATUM_UNSET; workingVerticalDatum <= IVERTICAL_DATUM_NGVD29; ++workingVerticalDatum) {
            switch (workingVerticalDatum) {
            case IVERTICAL_DATUM_UNSET:
                printf("\tWorking vertical datum is %s\n", CVERTICAL_DATUM_UNSET);
                break;
            case IVERTICAL_DATUM_NGVD29:
                printf("\tWorking vertical datum is %s\n", CVERTICAL_DATUM_NGVD29);
                break;
            case IVERTICAL_DATUM_NAVD88:
                printf("\tWorking vertical datum is %s\n", CVERTICAL_DATUM_NAVD88);
                break;
            }
            for (int tsType = REGULAR; tsType <= IRREGULAR; ++tsType) {
                printf("\t\tTesting %s time series\n", tsType == REGULAR ? "REGULAR" : "IRREGULAR");
                //--------------------------------------------------//
                // store two consecutive records with different VDI //
                //--------------------------------------------------//
                for (int year = 2021; year <= 2022; ++year) {
                    int yearIndex = year - 2021;
                    for (int xmlIndex = 0; xmlIndex < xmlCount; ++xmlIndex) {
                        numberValues = 365; // 1 non-leap year of 1Day values
                        //--------------------------------------//
                        // get the VDI and make the data arrays //
                        //--------------------------------------//
                        errmsg = stringToVerticalDatumInfo(&vdi, xml[yearIndex]);
                        for (int i = 0; i < numberValues; ++i) {
                            values[i] = (double)(i + 1) - (errmsg == NULL ? vdi.offsetToNgvd29 : 0);
                            times[i] = jan_01_2021_0100 + (365 * yearIndex + i) * MINS_IN_1_DAY;
                            quality[i] = 3;
                        }
                        //-------------------------------------------------//
                        // assign native vertical datum info to the record //
                        //-------------------------------------------------//
                        nativeDatum = (yearIndex + xmlIndex) % xmlCount;
                        if (strlen(xml[nativeDatum]) > 0) {
                            errmsg = gzipAndEncode(&compressed, xml[(yearIndex + xmlIndex) % xmlCount]);
                            assert(errmsg == NULL);
                            len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 2;
                            headerBuf = (char*)malloc(len + 1);
                            assert(headerBuf != NULL);
                            memset(headerBuf, 0, len + 1);
                            status = insertIntoDelimitedString(
                                &headerBuf,
                                len + 1,
                                VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                                compressed,
                                ":",
                                FALSE,
                                ';');
                            assert(status == 0);
                            free(compressed);
                            userHeader2 = stringToUserHeader(headerBuf, &userHeaderNumber);
                            free(headerBuf);
                            headerBuf = NULL;
                        }
                        else {
                            userHeader2 = malloc(sizeof(int));
                            userHeader2[0] = 0;
                            userHeaderNumber = 0;
                        }
                        //----------------------------------------------------------------//
                        // set the default vertical datum to the native datum of the data //
                        //----------------------------------------------------------------//
                        verticalDatumInfo* pVdi = extractVerticalDatumInfoFromUserHeader(userHeader2, userHeaderNumber);
                        zset("VDTM", pVdi ? pVdi->nativeDatum : "UNSET", 0);
                        printf("\t\t\tStoring time series for %d with native vertical datum of %s ", year, pVdi ? pVdi->nativeDatum : "UNSET");
                        printf("using %s.\n", api == OLD_API ? tsType == REGULAR ? "zsrtsxd" : "zsitsxd" : "ztsStore");
                        printf("\t\t\t\tShould %s.\n", xmlIndex == 0 || nativeDatum == UNSET ? "succeed" : "fail");
                        free(pVdi);
                        if (api == OLD_API) {
                            compressionType = 0;
                            if (tsType == REGULAR) {
                                zsrtsxd_(
                                    ifltab,                         // <-> file table
                                    pathnames[api][tsType],         //  -> dataset name
                                    startDate[yearIndex],           //  -> date of first value
                                    startTime,                      //  -> time of first value
                                    &numberValues,                  //  -> number of values to store
                                    values,                         //  -> values to store
                                    quality,                        //  -> quality flags to store
                                    &storeFlags,                    //  -> whether to store quality flags (0/1)
                                    unit,                           //  -> data unit
                                    dataType,                       //  -> data type
                                    userHeader2,                    //  -> user header array
                                    &userHeaderNumber,              //  -> number of header array elements to store
                                    &storeFlag,                     //  -> data storage method 0=replace all)
                                    &compressionType,               //  -> data compression type to use (0=file default)
                                    &compressionBaseVal,            //  -> data compression base value for delta method
                                    &compressionBase,               //  -> whether to use base value for delta method data compression (0/1)
                                    &compressionHigh,               //  -> whether to use 2 bytes per compressed value for delta method (0=let software decide)
                                    &compressionPrec,               //  -> base 10 exponent of compressed values for delta method
                                    &status,                        // <-  status (0=success)
                                    strlen(pathnames[api][tsType]), //  -> fortran-required length of dataset name parameter
                                    strlen(startDate[yearIndex]),   //  -> fortran-required length of date of first value parameter
                                    strlen(startTime),              //  -> fortran-required length of time of first value parameter
                                    strlen(unit),                   //  -> fortran-required length of data unit parameter
                                    strlen(dataType));              //  -> fortran-required length of data type parameter
                                if (xmlIndex == 0 || nativeDatum == UNSET) {
                                    assert(status == 0);
                                    printf("\t\t\t\tSucceeded.\n");
                                }
                                else {
                                    assert(status == 13);
                                    printf("\t\t\t\tFailed.\n");
                                }
                            }
                            else {
                                startJul = dateToJulian(startDate[yearIndex]);
                                for (int i = 0; i < numberValues; ++i) {
                                    times[i] -= startJul * MINS_IN_1_DAY;
                                }
                                zsitsxd_(
                                    ifltab,                         // <-> file table
                                    pathnames[api][tsType],         //  -> dataset name
                                    times,                          //  -> times relative to base date
                                    values,                         //  -> values to store
                                    &numberValues,                  //  -> number of values to store
                                    &startJul,                      //  -> base date for times
                                    quality,                        //  -> quality flags to store
                                    &storeFlags,                    //  -> whether to store quality flags (0/1)
                                    unit,                           //  -> data unit
                                    dataType,                       //  -> data type
                                    userHeader2,                    //  -> user header array
                                    &userHeaderNumber,              //  -> number of user header array elements to store
                                    &storeFlag,                     //  -> data storage method (0=merge)
                                    &status,                        // <-  status (0=success)
                                    strlen(pathnames[api][tsType]), //  -> fortran-required size of dataset name parameter
                                    strlen(unit),                   //  -> fortran-required size of data unit parameter
                                    strlen(dataType));              //  -> fortran-required size of data type parameter
                                if (xmlIndex == 0 || nativeDatum == UNSET) {
                                    assert(status == 0);
                                    printf("\t\t\t\tSucceeded.\n");
                                }
                                else {
                                    assert(status == 13);
                                    printf("\t\t\t\tFailed.\n");
                                }
                            }
                            free(userHeader2);
                        }
                        else {
                            //-------------------------------------//
                            // create a TSS for the storing record //
                            //-------------------------------------//
                            if (tsType == REGULAR) {
                                tss = zstructTsNewRegDoubles(
                                    pathnames[api][tsType],     // dataset name
                                    values,                     // values
                                    numberValues,               // number of values
                                    startDate[yearIndex],       // start date
                                    startTime,                  // start time
                                    unit,                       // data unit
                                    dataType);                  // data type
                            }
                            else {
                                tss = zstructTsNewIrregDoubles(
                                    pathnames[api][tsType],     // dataset name
                                    values,                     // values
                                    numberValues,               // number of values
                                    times,                      // times
                                    SECS_IN_1_MINUTE,             // time granularity in seconds
                                    NULL,                       // base date (if other than 01Jan1900)
                                    unit,                       // data unit
                                    dataType);                  // data type
                            }
                            tss->quality = quality;
                            tss->qualityElementSize = 1;
                            tss->allocated[zSTRUCT_TS_quality] = FALSE; // will free memory separately
                            //------------------------------------------//
                            // assign vertical datum info to the record //
                            //------------------------------------------//
                            tss->userHeader = userHeader2;
                            tss->userHeaderNumber = userHeaderNumber;
                            tss->allocated[zSTRUCT_userHeader] = TRUE;
                            //-----------------------//
                            // store the time series //
                            //-----------------------//
                            status = ztsStore(
                                ifltab,        // file table
                                tss,           // time series struct
                                0);            // storage flag (0=reg:replace all, irr:merge)
                            if (xmlIndex == 0 || nativeDatum == UNSET) {
                                assert(status == 0);
                                printf("\t\t\t\tSucceeded.\n");
                            }
                            else {
                                assert(status == 13);
                                printf("\t\t\t\tFailed.\n");
                            }
                            zstructFree(tss);
                        }
                        userHeader2 = NULL;
                    }
                }
                //-----------------------------------------------//
                // set the default vertical datum for retrieving //
                //-----------------------------------------------//
                zset("VDTM", "", workingVerticalDatum);
                //-----------------------------------------------------//
                // read the consecutive records just stored and verify //
                //-----------------------------------------------------//
                for (int year = 2021; year <= 2022; ++year) {
                    int yearIndex = year - 2021;
                    printf("\t\t\tRetrieving time series for year %d ", year);
                    printf("with %s\n", api == NEW_API ? "ztsRetrieve" :  "skipping.. legacy API");
                    maxValueCount = 365;
                    if (api == OLD_API) {
                      continue;
                    }
                    else {
                        //----------------------------------------//
                        // create a TSS for retrieving the record //
                        //----------------------------------------//
                        tss = zstructTsNewTimes(
                            pathnames[api][tsType],
                            startDate[yearIndex],
                            "0001",
                            endDate[yearIndex],
                            "2400");
                        assert(tss != NULL);
                        //--------------------------//
                        // retrieve the time series //
                        //--------------------------//
                        status = ztsRetrieve(
                            ifltab,           // file table
                            tss,              // time series struct
                            0,                // retrieve flag (0=adhere to time window and [for reg] create times array)
                            0,                // retrieve doubles flag (0=as stored, 1=floats, 2=doubles)
                            1);               // retrieve quality flag (0/1)
                        assert(status == STATUS_OKAY);
                        headerBuf = userHeaderToString(tss->userHeader, tss->userHeaderNumber);
                        assert(headerBuf != NULL);
                        assert(strlen(headerBuf) > 0);
                        dv = tss->doubleValues;
                        t = tss->times;
                    }
                    char* currentVerticalDatum = extractFromDelimitedString(
                        &headerBuf,
                        VERTICAL_DATUM_USER_HEADER_PARAM,
                        ":",
                        FALSE,
                        FALSE,
                        ';');
                    switch (workingVerticalDatum) {
                    case IVERTICAL_DATUM_UNSET:
                        assert(currentVerticalDatum == NULL);
                        currentVerticalDatum = vdi.nativeDatum;
                        break;
                    case IVERTICAL_DATUM_NGVD29:
                        assert(!strcmp(currentVerticalDatum, CVERTICAL_DATUM_NGVD29));
                        break;
                    case IVERTICAL_DATUM_NAVD88:
                        assert(!strcmp(currentVerticalDatum, CVERTICAL_DATUM_NAVD88));
                        break;
                    }
                    printf("\t\t\t\tValues were retrieved with native datum of %s and current datum of %s\n", vdi.nativeDatum, currentVerticalDatum);
                    if (currentVerticalDatum != vdi.nativeDatum) {
                        free(currentVerticalDatum);
                    }
                    //--------------------------------------------//
                    // get the vertical datum info for the record //
                    //--------------------------------------------//
                    char* compressedVdi = extractFromDelimitedString(
                        &headerBuf,
                        VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                        ":",
                        FALSE,
                        FALSE,
                        ';');
                    assert(compressedVdi != NULL);
                    char* vdiStr = '\0';
                    errmsg = decodeAndGunzip(&vdiStr, compressedVdi);
                    assert(errmsg == NULL);
                    free(compressedVdi);
                    errmsg = stringToVerticalDatumInfo(&vdi, vdiStr);
                    assert(errmsg == NULL);
                    free(vdiStr);
                    free(headerBuf);
                    //----------------------------------------------------//
                    // verify the data values for the record              //
                    //                                                    //
                    // when there is no working vertical datum the values //
                    // should be in the record's native vertical datum -  //
                    // otherwise they should be in the working vertical   //
                    // datum                                              //
                    //----------------------------------------------------//
                    for (int i = 0; i < numberValues; i+= 30) {
                        switch (workingVerticalDatum) {
                        case IVERTICAL_DATUM_UNSET:
                            expectedValue = (i % 365) + 1 - vdi.offsetToNgvd29;
                            break;
                        case IVERTICAL_DATUM_NGVD29:
                            expectedValue = (i % 365) + 1;
                            break;
                        case IVERTICAL_DATUM_NAVD88:
                            expectedValue = (i % 365) + 1.5;
                            break;
                        }
                        assert(dv[i] == expectedValue);
                    }
                    if (api == NEW_API) {
                        zstructFree(tss);
                    }
                }
                //-----------------------------------------------------//
                // read a dataset that crosses record boundaries       //
                //                                                     //
                // when there is no working vertical datum the library //
                // should write a warning message out to the message   //
                // unit similar to the one shown below, but retrieve   //
                // the values                                          //
                //-----------------------------------------------------//
                //
                // *****DSS*** zrrtsi6:  WARNING  - Elevation values are in multiple native vertical datums
                // Use with caution!
                //
                printf("\t\t\tRetrieving time series that crosses record boundaries ");
                printf("with %s\n", api == NEW_API ? "ztsRetrieve" : "skipping legay API");
                maxValueCount = 21; // 21Dec -- 10Jan
                if (api == OLD_API) {
                  continue;
                }
                else {
                    tss = zstructTsNewTimes(
                        pathnames[api][tsType],  // dataset name
                        "21Dec2021",             // start date
                        "0001",                  // start time
                        "10Jan2022",             // end date
                        "2400");                 // end time
                    assert(tss != NULL);
                    status = ztsRetrieve(
                        ifltab,              // file table
                        tss,                 // time series struct
                        0,                   // retrieve flag (0=adhere to time window and [for reg] create times array)
                        0,                   // retrieve doubles flag (0=as stored, 1=floats, 2=doubles)
                        1);                  // retrieve quality flag (0/1)
                    assert(status == STATUS_OKAY);
                    headerBuf = userHeaderToString(tss->userHeader, tss->userHeaderNumber);
                    assert(headerBuf != NULL);
                    assert(strlen(headerBuf) > 0);
                    dv = tss->doubleValues;
                    t = tss->times;
                    numberValues = tss->numberValues;
                }
                //----------------------------------------------------//
                // verify the cross-boundary data values              //
                //                                                    //
                // when there is no working vertical datum the values //
                // from each record will be in the native vertical    //
                // datum for that record - otherwise they should all  //
                // be in the working vertical datum                   //
                //----------------------------------------------------//
                char* currentVerticalDatum = extractFromDelimitedString(
                    &headerBuf,
                    VERTICAL_DATUM_USER_HEADER_PARAM,
                    ":",
                    FALSE,
                    FALSE,
                    ';');
                char* compressedVdi = extractFromDelimitedString(
                    &headerBuf,
                    VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                    ":",
                    FALSE,
                    FALSE,
                    ';');
                assert(compressedVdi != NULL);
                char* vdiStr = '\0';
                errmsg = decodeAndGunzip(&vdiStr, compressedVdi);
                assert(errmsg == NULL);
                free(compressedVdi);
                errmsg = stringToVerticalDatumInfo(&vdi, vdiStr);
                assert(errmsg == NULL);
                free(vdiStr);
                free(headerBuf);
                printf(
                    "\t\t\t\tValues were retrieved with native datum of %s and current datum of %s\n",
                    vdi.nativeDatum,
                    currentVerticalDatum ? currentVerticalDatum : vdi.nativeDatum);
                free(currentVerticalDatum);
                for (int i = 0; i < numberValues; ++i) {
                    switch (workingVerticalDatum) {
                    case IVERTICAL_DATUM_UNSET:
                        if (t[i] < jan_01_2022_0001) {
                            expectedValue = ((t[i] / MINS_IN_1_DAY - 1) - jan_01_2021) % 365 + 1;
                        }
                        else {
                            expectedValue = ((t[i] / MINS_IN_1_DAY - 1) - jan_01_2021) % 365 + 1.5;
                        }
                        break;
                    case IVERTICAL_DATUM_NGVD29:
                        expectedValue = ((t[i] / MINS_IN_1_DAY - 1) - jan_01_2021) % 365 + 1;
                        break;
                    case IVERTICAL_DATUM_NAVD88:
                        expectedValue = ((t[i] / MINS_IN_1_DAY - 1) - jan_01_2021) % 365 + 1.5;
                        break;
                    }
                    assert(dv[i] == expectedValue);
                }
                if (api == NEW_API) {
                    memcpy(values, tss->doubleValues, maxValueCount * sizeof(double));
                    memcpy(times, tss->times , maxValueCount * sizeof(int));
                    zstructFree(tss);
                }
                //-------------------------------------------------//
                // store a dataset that crosses record boundaries  //
                //                                                 //
                // this should always fail becuase the two records //
                // have different native vertical datums and the   //
                // following error message should be output        //
                //-------------------------------------------------//
                //
                // *****DSS*** zsrtsi6:  ERROR - VERTICAL DATUM CONFLICT
                //     Elevation values in file are in multiple native vertical datums.
                //
                //     No values stored.
                //
                printf("\t\t\tStoring time series that crosses record boundaries ");
                printf("with %s\n", api == NEW_API ? "ztsStore" : tsType == REGULAR ? "zsrtsxd" : "zsitsxd");
                printf("\t\t\t\tShould fail.\n");
                if (api == OLD_API) {
                    compressionType = 0;
                    userHeaderNumber = 0;
                    numberValues = maxValueCount;
                    if (tsType == REGULAR) {
                        zsrtsxd_(
                            ifltab,                         // <-> file table
                            pathnames[api][tsType],         //  -> dataset name
                            "21Dec2021",                    //  -> date of first value
                            startTime,                      //  -> time of first value
                            &numberValues,                  //  -> number of values to store
                            values,                         //  -> values to store
                            quality,                        //  -> quality flags to store
                            &storeFlags,                    //  -> whether to store quality flags (0/1)
                            unit,                           //  -> data unit
                            dataType,                       //  -> data type
                            userHeader2,                    //  -> user header array
                            &userHeaderNumber,              //  -> number of header array elements to store
                            &storeFlag,                     //  -> data storage method 0=replace all)
                            &compressionType,               //  -> data compression type to use (0=file default)
                            &compressionBaseVal,            //  -> data compression base value for delta method
                            &compressionBase,               //  -> whether to use base value for delta method data compression (0/1)
                            &compressionHigh,               //  -> whether to use 2 bytes per compressed value for delta method (0=let software decide)
                            &compressionPrec,               //  -> base 10 exponent of compressed values for delta method
                            &status,                        // <-  status (0=success)
                            strlen(pathnames[api][tsType]), //  -> fortran-required length of dataset name parameter
                            strlen("21Dec2021"),            //  -> fortran-required length of date of first value parameter
                            strlen(startTime),              //  -> fortran-required length of time of first value parameter
                            strlen(unit),                   //  -> fortran-required length of data unit parameter
                            strlen(dataType));              //  -> fortran-required length of data type parameter
                    }
                    else {
                        startJul = dateToJulian("21Dec2021");
                        for (int i = 0; i < numberValues; ++i) {
                            times[i] -= startJul * MINS_IN_1_DAY;
                        }
                        zsitsxd_(
                            ifltab,                         // <-> file table
                            pathnames[api][tsType],         //  -> dataset name
                            times,                          //  -> times relative to base date
                            values,                         //  -> values to store
                            &numberValues,                  //  -> number of values to store
                            &startJul,                      //  -> base date for times
                            quality,                        //  -> quality flags to store
                            &storeFlags,                    //  -> whether to store quality flags (0/1)
                            unit,                           //  -> data unit
                            dataType,                       //  -> data type
                            userHeader2,                    //  -> user header array
                            &userHeaderNumber,              //  -> number of user header array elements to store
                            &storeFlag,                     //  -> data storage method (0=merge)
                            &status,                        // <-  status (0=success)
                            strlen(pathnames[api][tsType]), //  -> fortran-required size of dataset name parameter
                            strlen(unit),                   //  -> fortran-required size of data unit parameter
                            strlen(dataType));              //  -> fortran-required size of data type parameter
                    }
                }
                else {
                    //-------------------------------------//
                    // create a TSS for the storing record //
                    //-------------------------------------//
                    if (tsType == REGULAR) {
                        tss = zstructTsNewRegDoubles(
                            pathnames[api][tsType],     // dataset name
                            values,                     // values
                            numberValues,               // number of values
                            "21Dec2021",                // start date
                            startTime,                  // start time
                            unit,                       // data unit
                            dataType);                  // data type
                    }
                    else {
                        tss = zstructTsNewIrregDoubles(
                            pathnames[api][tsType],     // dataset name
                            values,                     // values
                            numberValues,               // number of values
                            times,                      // times
                            SECS_IN_1_MINUTE,             // time granularity in seconds
                            NULL,                       // base date (if other than 01Jan1900)
                            unit,                       // data unit
                            dataType);                  // data type
                    }
                    tss->quality = quality;
                    tss->qualityElementSize = 1;
                    tss->allocated[zSTRUCT_TS_quality] = FALSE; // will free memory separately
                    //-----------------------//
                    // store the time series //
                    //-----------------------//
                    status = ztsStore(
                        ifltab,        // file table
                        tss,           // time series struct
                        0);            // storage flag (0=reg:replace all, irr:merge)
                    zstructFree(tss);
                }
                printf("\t\t\t\t%s\n", status == 0 ? "Succeeded." : "Failed.");
                assert(status == 13);
            }
        }
    }
    zclose(ifltab);
    remove(filename);
    zset("MLVL", "", messageLevel);
    free(values);
    free(times);
    free(userHeader);
    free(quality);
}

void deleteTimeSeriesRecords(
        long long *ifltab, 
        const char* pathname, 
        const int startJul, 
        const int endJul, 
        const int blockSize,
        const int deleteLocationRecordAlso) {
    char recordPathname[(MAX_PART_SIZE - 1) * 6 + 8];
    char ePart[MAX_PART_SIZE];
    int recordJul = 0;
    int intervalSeconds = 0;
    strcpy(recordPathname, pathname);
    zpathnameGetPart(recordPathname, 5, ePart, sizeof(ePart));
    int isIrregular = ePart[0] == '~' || toupper(ePart[0]) == 'I';
    if (isIrregular) {
        recordJul = ztsIrregGetBlockStart(startJul, blockSize);
    }
    else {
        int operation = EPART_TO_SECONDS;
        int blksize = blockSize;
        ztsGetStandardInterval(7, &intervalSeconds, ePart, sizeof(ePart), &operation);
        recordJul = ztsRegGetBlockStart(startJul, intervalSeconds, &blksize);
    }
    while (recordJul <= endJul) {
        julianToDate(recordJul, 104, ePart, sizeof(ePart));
        zpathnameSetPart(recordPathname, sizeof(recordPathname), ePart, 4);
        zdelete(ifltab, recordPathname);
        recordJul = ztsIncrementBlock(recordJul, blockSize);
    }
    if (deleteLocationRecordAlso) {
        char *locationPath = zlocationPath(pathname);
        zdelete(ifltab, locationPath);
        free(locationPath);
    }
}

void printTsTestInfo(
    const int count,
    const int expectSuccess,
    const char* pathname,
    const int dataInFile,
    const verticalDatumInfo* fileVdi,
    const verticalDatumInfo* dataVdi,
    const char* currentVerticalDatum,
    const char* unit) {
    printf("Time series test %5d: expecting %s\n", count, expectSuccess ? "SUCCESS" : "ERROR");
    printf("    pathname               = %s\n", pathname);
    printf("    data in file           = %s\n", dataInFile ? "T" : "F");
    printf("    native datum in file   = %s\n", fileVdi->nativeDatum);
    printf("    incoming native datum  = %s\n", dataVdi->nativeDatum);
    printf("    incoming current datum = %s\n", currentVerticalDatum);
    printf("    incoming unit          = %s\n", unit);
}

void printPdTestInfo(
    const int count,
    const int expectSuccess,
    const char* pathname,
    const int dataInFile,
    const verticalDatumInfo* fileVdi,
    const verticalDatumInfo* dataVdi,
    const char* currentVerticalDatum,
    const char* indUnit,
    const char* depUnit) {
    printf("Paired data test %5d: expecting %s\n", count, expectSuccess ? "SUCCESS" : "ERROR");
    printf("    pathname               = %s\n", pathname);
    printf("    data in file           = %s\n", dataInFile ? "T" : "F");
    printf("    native datum in file   = %s\n", fileVdi->nativeDatum);
    printf("    incoming native datum  = %s\n", dataVdi->nativeDatum);
    printf("    incoming current datum = %s\n", currentVerticalDatum);
    printf("    incoming units         = %s, %s\n", indUnit, depUnit);
}

void testStoreRetrieveTimeSeries() {
// test storing and retriving time series data
    long long ifltab[250] = {0};
    int status;
    zStructTimeSeries *tss = NULL;
    verticalDatumInfo vdi;
    verticalDatumInfo vdiInFile;
    verticalDatumInfo blankVdi;
    char *errmsg;
    double offset;
    char *filename[]      = {"v6_c.dss", "v7_c.dss"};
    char *pathnames[2][2] = {{"//TestTsLoc/Elev//1Hour/Doubles/",    "//TestTsLoc/Elev//1Hour/Floats/"},
                             {"//TestTsLoc/Elev//IR-MONTH/Doubles/", "//TestTsLoc/Elev//IR-MONTH/Floats/"}};
    char nativeDatumInFile[16];
    double dvalues[3][6]  = {{1000,1001,1002,1003,1004,1005},                           // ft
                             {304.8,305.1048,305.4096,305.7144,306.0192,306.324},       // m
                             {1000,1001,1002,1003,1004,1005}};                          // cfs
    float  fvalues[3][6]  = {{1000.f,1001.f,1002.f,1003.f,1004.f,1005.f},               // ft
                             {304.8f,305.1048f,305.4096f,305.7144f,306.0192f,306.324f}, // m
                             {1000.f,1001.f,1002.f,1003.f,1004.f,1005.f}};              // cfs
    int numberValues   = 6;
    char *startDate    = "01Oct2021";
    char *startTime    = "01:00";
    char *endDate      = "01Oct2021";
    char *endTime      = "24:00";
    int   itimes[6]    = {64035420, 64035480, 64035540, 64035600, 64035660, 64035720};
    char *unit[]       = {"ft", "m", "cfs"};
    char *type         = "INST-VAL";
    char *compressed   = NULL;
    char *headerBuf    = NULL;
    int   len          = 0;
    char  unitSpec[24];
    char *xml[]       = {
        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.2</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>0.3855</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.2</elevation>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NAVD-88</native-datum>\n"
        "  <elevation>615.5885</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>-0.3855</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NAVD-88</native-datum>\n"
        "  <elevation>615.5885</elevation>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>OTHER</native-datum>\n"
        "  <local-datum-name>Pensacola</local-datum-name>\n"
        "  <elevation>757</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>1.457</value>\n"
        "  </offset>\n"
        "  <offset estimate=\"false\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>1.07</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>OTHER</native-datum>\n"
        "  <local-datum-name>Pensacola</local-datum-name>\n"
        "  <elevation>757</elevation>\n"
        "</vertical-datum-info>\n",

        ""
    };
    char *currentVerticalDatums[] = {
        CVERTICAL_DATUM_NAVD88,
        CVERTICAL_DATUM_NGVD29,
        "Pensacola",
        CVERTICAL_DATUM_UNSET
    };
    int unitCount = sizeof(unit) / sizeof(unit[0]);
    int xml_count = sizeof(xml) / sizeof(xml[0]);
    int currentVerticalDatumCount = sizeof(currentVerticalDatums) / sizeof(currentVerticalDatums[0]);
    int dataInFile = FALSE;
    int count = 0;
    int expectSuccess = TRUE;
    memset(nativeDatumInFile, 0, sizeof(nativeDatumInFile));
    initializeVerticalDatumInfo(&blankVdi);

    //
    // loop variables
    //
    // i = DSS file version
    //     0 = DSS 6
    //     1 = DSS 7
    //
    // j = xml blocks for native vertical datum
    //     0 = NGVD-29 native with offset to NAVD-88
    //     1 = NGVD-29 native without offset to NAVD-88
    //     2 = NAVD-88 native with offset to NGVD-29
    //     3 = NAVD-88 native without offset to NGVD-29
    //     4 = OTHER native with local datum named "Pensacola" with offsets to NAVD-88 and NGVD-29
    //     5 = OTHER native with local datum named "Pensacola" without offsets to NAVD-88 and NGVD-29
    //     6 = None
    //
    // k = current (aka default/session/working) vertical datum
    //     0 = NAVD-88
    //     1 = NGVD-29
    //     2 = OTHER (Pensacola)
    //     3 = UNSET
    //     k2  (k + 1) % 3
    //     k3  (k + 2) % 3
    //
    // l = data units
    //     0 = ft
    //     1 = m
    //     2 = cfs (invalid for datum conversion)
    //
    // m = vertical datum specification method
    //     0 = set default with zset
    //     1 = 0 plus override with user header
    //     2 = 1 plus override with unit spec
    //
    // n = time series type
    //     0 = regular time series
    //     1 = irregular time series
    //
    // o = data value type
    //     0 = doubles
    //     1 = floats
    //
    // p = specify vertical datum info in user header on store
    //     0 = specify
    //     1 = don't specify (use previously stored)
    //
    // q = ensure empty record
    //     0 = leave any existing data in file
    //     1 = delete existing record
    //
    zset("MLVL", "", 1);
    for (int i = 0; i < 2; ++i) {
        remove(filename[i]);
        for (int j = 0; j < xml_count; ++j) {
            for (int k = 0; k < currentVerticalDatumCount; ++k) {
                int k2 = (k+1) % currentVerticalDatumCount;
                int k3 = (k+2) % currentVerticalDatumCount;
                for (int l = 0; l < unitCount; ++ l) {
                    for (int m = 0; m < 3; ++m) {
                        for (int n = 0; n < 2; ++n) {
                            for (int o = 0; o < 2; ++o) {
                                for (int p = 0; p < 2; ++p) {
                                    for (int q = 0; q < 2; ++q) {
                                        ++count;
                                        len = 0;
                                        headerBuf = NULL;
                                        nativeDatumInFile[0] = '\0';
                                        initializeVerticalDatumInfo(&vdiInFile);
                                        if (i == 0) {
                                            //-------//
                                            // DSS 6 //
                                            //-------//
                                            status = zopen6(ifltab, filename[i]);
                                            assert(status == STATUS_OKAY);
                                            //----------------------------------------------------------------------------//
                                            // get whether data exists in file and native datum in file for this pathname //
                                            //----------------------------------------------------------------------------//
                                            tss = zstructTsNewTimes(
                                                pathnames[n][o],
                                                startDate,
                                                startTime,
                                                endDate,
                                                endTime);
                                            assert(tss != NULL);
                                            zset("VDTM", CVERTICAL_DATUM_UNSET, 0);
                                            status = ztsRetrieve(
                                                ifltab,
                                                tss,
                                                n == 0 ? -1 : 0, // trim regular time series
                                                0,
                                                1);
                                            dataInFile = status == STATUS_OKAY && tss->numberValues > 0;
                                            if (status == STATUS_OKAY) {
                                                headerBuf = userHeaderToString(tss->userHeader, tss->userHeaderNumber);
                                                if (headerBuf != NULL) {
                                                    char* compressedVdi = extractFromDelimitedString(
                                                        &headerBuf,
                                                        VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                                                        ":",
                                                        TRUE,
                                                        FALSE,
                                                        ';');
                                                    if (compressedVdi) {
                                                        errmsg = stringToVerticalDatumInfo(&vdiInFile, compressedVdi);
                                                        assert(errmsg == NULL);
                                                        strcpy(nativeDatumInFile, vdiInFile.nativeDatum);
                                                        free(compressedVdi);
                                                    }
                                                    free(headerBuf);
                                                }
                                                headerBuf = NULL;
                                            }
                                            zstructFree(tss);
                                        }
                                        else {
                                            //-------//
                                            // DSS 7 //
                                            //-------//
                                            status = zopen7(ifltab, filename[i]);
                                            assert(status == STATUS_OKAY);
                                            //----------------------------------------------------------------------------//
                                            // get whether data exists in file and native datum in file for this pathname //
                                            //----------------------------------------------------------------------------//
                                            tss = zstructTsNewTimes(
                                                pathnames[n][o],
                                                startDate,
                                                startTime,
                                                endDate,
                                                endTime);
                                            assert(tss != NULL);
                                            zset("VDTM", CVERTICAL_DATUM_UNSET, 0);
                                            status = ztsRetrieve(
                                                ifltab,
                                                tss,
                                                n == 0 ? -1 : 0, // trim regular time series
                                                0,
                                                1);
                                            dataInFile = status == STATUS_OKAY && tss->numberValues > 0;
                                            zstructFree(tss);
                                            zStructLocation* ls = zstructLocationNew(pathnames[n][o]);
                                            zlocationRetrieve(ifltab, ls);
                                            if (ls) {
                                                if (ls->supplemental) {
                                                    char* compressedVdi = extractFromDelimitedString(
                                                        &ls->supplemental,
                                                        VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                                                        ":",
                                                        TRUE,
                                                        FALSE,
                                                        ';');
                                                    if (compressedVdi) {
                                                        stringToVerticalDatumInfo(&vdiInFile, compressedVdi);
                                                        strcpy(nativeDatumInFile, vdiInFile.nativeDatum);
                                                        free(compressedVdi);
                                                    }
                                                }
                                                zstructFree(ls);
                                            }
                                        }
                                        //------------------------//
                                        // create the time series //
                                        //------------------------//
                                        if (n == 0) {
                                            if (o == 0) {
                                                tss = zstructTsNewRegDoubles(
                                                    pathnames[n][o],
                                                    dvalues[l],
                                                    numberValues,
                                                    startDate,
                                                    startTime,
                                                    unit[l],
                                                    type);
                                            }
                                            else {
                                                tss = zstructTsNewRegFloats(
                                                    pathnames[n][o],
                                                    fvalues[l],
                                                    numberValues,
                                                    startDate,
                                                    startTime,
                                                    unit[l],
                                                    type);
                                            }
                                        }
                                        else {
                                            if (o == 0) {
                                                tss = zstructTsNewIrregDoubles(
                                                    pathnames[n][o],
                                                    dvalues[l],
                                                    numberValues,
                                                    itimes,
                                                    SECS_IN_1_MINUTE,
                                                    NULL,
                                                    unit[l],
                                                    type);
                                            }
                                            else {
                                                tss = zstructTsNewIrregFloats(
                                                    pathnames[n][o],
                                                    fvalues[l],
                                                    numberValues,
                                                    itimes,
                                                    SECS_IN_1_MINUTE,
                                                    NULL,
                                                    unit[l],
                                                    type);
                                            }
                                        }
                                        assert(tss != NULL);
                                        int K = k;
                                        strcpy(unitSpec, unit[l]);
                                        //--------------------------------//
                                        // set the default vertical datum //
                                        //--------------------------------//
                                        zset("VDTM", currentVerticalDatums[K], 0);
                                        if (p == 0) {
                                            //------------------------------------------------//
                                            // add the vertical datum info to the user header //
                                            //------------------------------------------------//
                                            if (strlen(xml[j]) > 0) {
                                                stringToVerticalDatumInfo(&vdi, xml[j]);
                                                errmsg = gzipAndEncode(&compressed, xml[j]);
                                                assert(errmsg == NULL);
                                                len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 2;
                                                headerBuf = (char*)malloc(len + 1);
                                                memset(headerBuf, 0, len + 1);
                                                status = insertIntoDelimitedString(
                                                    &headerBuf,
                                                    len + 1,
                                                    VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                                                    compressed,
                                                    ":",
                                                    FALSE,
                                                    ';');
                                                assert(status == 0);
                                                free(compressed);
                                                tss->userHeader = stringToUserHeader(headerBuf, &tss->userHeaderNumber);
                                                tss->allocated[zSTRUCT_userHeader] = TRUE;
                                            }
                                        }
                                        else {
                                            initializeVerticalDatumInfo(&vdi);
                                        }
                                        if (m > 0) {
                                            //----------------------------------------------------------//
                                            // override the default vertical datum with the user header //
                                            //----------------------------------------------------------//
                                            K = k2;
                                            status = insertIntoDelimitedString(
                                                headerBuf ? &headerBuf : NULL,
                                                len,
                                                VERTICAL_DATUM_USER_HEADER_PARAM,
                                                currentVerticalDatums[K],
                                                ":",
                                                FALSE,
                                                ';');
                                            if (status != 0) {
                                                if (headerBuf) {
                                                    int oldlen = len;
                                                    len += VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(currentVerticalDatums[K]) + 3;
                                                    headerBuf = (char*)realloc(headerBuf, len);
                                                    assert(headerBuf != NULL);
                                                    memset(headerBuf + oldlen, 0, len - oldlen);
                                                }
                                                else {
                                                    len = VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(currentVerticalDatums[K]) + 3;
                                                    headerBuf = (char*)malloc(len);
                                                    headerBuf[0] = '\0';
                                                }
                                                assert(headerBuf != NULL);
                                                status = insertIntoDelimitedString(
                                                    &headerBuf,
                                                    len,
                                                    VERTICAL_DATUM_USER_HEADER_PARAM,
                                                    currentVerticalDatums[K],
                                                    ":",
                                                    FALSE,
                                                    ';');
                                                assert(status == 0);
                                            }
                                            free(tss->userHeader);
                                            tss->userHeader = stringToUserHeader(headerBuf, &tss->userHeaderNumber);
                                            tss->allocated[zSTRUCT_userHeader] = TRUE;
                                            if (m > 1) {
                                                //--------------------------------------------------------//
                                                // override default and user header datums with unit spec //
                                                //--------------------------------------------------------//
                                                K = k3;
                                                sprintf(unitSpec, "U=%s|V=%s", unit[l], currentVerticalDatums[K]);
                                                free(tss->units);
                                                tss->units = mallocAndCopy(unitSpec);
                                            }
                                        }
                                        if (q == 1) {
                                            //----------------------------//
                                            // delete any records in file //
                                            //----------------------------//
                                            ztsProcessTimes(ifltab, tss, TRUE);
                                            deleteTimeSeriesRecords(
                                                ifltab,
                                                tss->pathname,
                                                tss->timeWindow->startBlockJulian,
                                                tss->timeWindow->endBlockJulian,
                                                tss->timeWindow->blockSize,
                                                FALSE);
                                            dataInFile = FALSE;
                                            if (i == 0) {
                                                //--------------------------------------------------//
                                                // deleting records also deletes file VDI for DSS 6 //
                                                //--------------------------------------------------//
                                                initializeVerticalDatumInfo(&vdiInFile);
                                                nativeDatumInFile[0] = '\0';
                                            }
                                        }
                                        //-------------------------------------------------//
                                        // figure out whether expect ztsStore to succeeded //
                                        //-------------------------------------------------//
                                        errmsg = processStorageVdis(&offset, &vdiInFile, &vdi, currentVerticalDatums[K], dataInFile, unit[l]);
                                        expectSuccess = errmsg == NULL;
                                        //-------------------------------------------------------//
                                        // store the time series in the specified vertical datum //
                                        //-------------------------------------------------------//
                                        printTsTestInfo(count, expectSuccess, pathnames[n][o], dataInFile, &vdiInFile, &vdi, currentVerticalDatums[K], unitSpec);
                                        status = ztsStore(ifltab, tss, 0);
                                        assert((status == STATUS_OKAY) == expectSuccess);
                                        if (status != STATUS_OKAY) {
                                            if (errmsg != NULL && strstr(errmsg, "Data native datum") && strstr(errmsg, "conflicts with file native datum")) {
                                                //--------------------------------------//
                                                // change of vertical datum information //
                                                //                                      //
                                                // set VDOW to override file VDI        //
                                                // with data VDI and re-try             //
                                                //--------------------------------------//
                                                zset("VDOW", "", TRUE);
                                                free(errmsg);
                                                if (m > 1) {
                                                    //-----------------------------------------------//
                                                    // unit spec has already been removed from units //
                                                    //-----------------------------------------------//
                                                    free(tss->units);
                                                    tss->units = mallocAndCopy(unitSpec);
                                                }
                                                errmsg = processStorageVdis(&offset, &vdiInFile, &vdi, currentVerticalDatums[K], dataInFile, unit[l]);
                                                expectSuccess = errmsg == NULL;
                                                printTsTestInfo(++count, expectSuccess, pathnames[n][o], dataInFile, &vdiInFile, &vdi, currentVerticalDatums[K], unitSpec);
                                                status = ztsStore(ifltab, tss, 0);
                                                assert((status == STATUS_OKAY) == expectSuccess);
                                                assert (errmsg == NULL || !(strstr(errmsg, "Data native datum") && strstr(errmsg, "conflicts with file native datum")));
                                                zset("VDOW", "", FALSE);
                                            }
                                        }
                                        free(errmsg);
                                        zclose(ifltab);
                                        zstructFree(tss);
                                        if (status == STATUS_OKAY) {
                                            //--------------------------------------------------------//
                                            // retrieve the time series in the default vertical datum //
                                            //--------------------------------------------------------//
                                            if (i == 0) {
                                                status = zopen6(ifltab, filename[i]);
                                            }
                                            else {
                                                status = zopen7(ifltab, filename[i]);
                                            }
                                            assert(status == STATUS_OKAY);
                                            tss = zstructTsNewTimes(
                                                pathnames[n][o],
                                                startDate,
                                                startTime,
                                                endDate,
                                                endTime);
                                            assert(tss != NULL);
                                            //------------------------------------------------------------//
                                            // set the default vertical datum to the datum we stored with //
                                            //------------------------------------------------------------//
                                            zset("VDTM", currentVerticalDatums[K], 0);
                                            status = ztsRetrieve(
                                                ifltab,
                                                tss,
                                                n == 0 ? -1 : 0, // trim regular time series
                                                0,
                                                1);
                                            assert(status == STATUS_OKAY);
                                            //------------------------------------------------------//
                                            // compare the retrieved time seires to what was stored //
                                            //------------------------------------------------------//
                                            assert(tss->numberValues == numberValues);
                                            if (o == 0) {
                                                assert(tss->doubleValues != NULL);
                                                for (int ii = 0; ii < tss->numberValues; ++ii) {
                                                    assert(tss->doubleValues[ii] == dvalues[l][ii]);
                                                }
                                            }
                                            else {
                                                assert(tss->floatValues != NULL);
                                                for (int ii = 0; ii < tss->numberValues; ++ii) {
                                                    assert(tss->floatValues[ii] == fvalues[l][ii]);
                                                }
                                            }
                                            zclose(ifltab);
                                            zstructFree(tss);
                                        }
                                        if (headerBuf) free(headerBuf);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    printf("\n\n%5d Time series tests passed\n\n\n", count);
}

void testStoreRetrievePairedData() {
    // test storing and retrieving paired data
    long long ifltab[250] = {0};
    zStructPairedData *pds;
    verticalDatumInfo vdi;
    verticalDatumInfo vdiInFile;
    int    status;
    char  *errmsg;
    double offset;
    char  *filename[2]      = {"v6_c.dss", "v7_c.dss"};
    char  *pathnames[2][2]  = {{"//TestPdLoc/Stage-Elev///Doubles/", "//TestPdLoc/Stage-Elev///Floats/"},
                               {"//TestPdLoc/Elev-Stage///Doubles/", "//TestPdLoc/Elev-Stage///Floats/"}};
    char   nativeDatumInFile[16];
    char  *type             = "Linear";
    char  *unit[]           = {"ft", "m", "cfs"};
    double dordinates[3][6] = {{1000,1001,1002,1003,1004,1005},                           // ft
                               {304.8,305.1048,305.4096,305.7144,306.0192,306.324},       // m
                               {1000,1001,1002,1003,1004,1005}};                          // cfs
    double dvalues[3][6]    = {{1000,1001,1002,1003,1004,1005},                           // ft
                               {304.8,305.1048,305.4096,305.7144,306.0192,306.324},       // m
                               {1000,1001,1002,1003,1004,1005}};                          // cfs
    float  fordinates[3][6] = {{1000.f,1001.f,1002.f,1003.f,1004.f,1005.f},               // ft
                               {304.8f,305.1048f,305.4096f,305.7144f,306.0192f,306.324f}, // m
                               {1000.f,1001.f,1002.f,1003.f,1004.f,1005.f}};              // cfs
    float  fvalues[3][6]    = {{1000.f,1001.f,1002.f,1003.f,1004.f,1005.f},               // ft
                                {304.8f,305.1048f,305.4096f,305.7144f,306.0192f,306.324f}, // m
                                {1000.f,1001.f,1002.f,1003.f,1004.f,1005.f}};              // cfs
    int    numberOrdinates   = 6;
    int    numberCurves      = 1;
    char  *compressed   = NULL;
    char  *headerBuf    = NULL;
    int    len          = 0;
    char   unitSpec[24];
    char *xml[]       = {
        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.2</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>0.3855</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.2</elevation>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NAVD-88</native-datum>\n"
        "  <elevation>615.5885</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>-0.3855</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NAVD-88</native-datum>\n"
        "  <elevation>615.5885</elevation>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>OTHER</native-datum>\n"
        "  <local-datum-name>Pensacola</local-datum-name>\n"
        "  <elevation>757</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>1.457</value>\n"
        "  </offset>\n"
        "  <offset estimate=\"false\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>1.07</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>OTHER</native-datum>\n"
        "  <local-datum-name>Pensacola</local-datum-name>\n"
        "  <elevation>757</elevation>\n"
        "</vertical-datum-info>\n",

        ""
    };
    char* currentVerticalDatums[] = {
        CVERTICAL_DATUM_NAVD88,
        CVERTICAL_DATUM_NGVD29,
        "Pensacola",
        CVERTICAL_DATUM_UNSET
    };
    int unitCount = sizeof(unit) / sizeof(unit[0]);
    int xml_count = sizeof(xml) / sizeof(xml[0]);
    int currentVerticalDatumCount = sizeof(currentVerticalDatums) / sizeof(currentVerticalDatums[0]);
    int dataInFile = FALSE;
    int count = 0;
    int expectSuccess = FALSE;

    //
    // loop variables
    //
    // i = DSS file version
    //     0 = DSS 6
    //     1 = DSS 7
    //
    // j = xml blocks
    //     0 = NGVD-29 native
    //     1 = NAVD-88 native
    //     2 = OTHER native with local datum named "Pensacola"
    //     3 = None
    //
    // k = vertical datum
    //     0 = NAVD-88
    //     1 = NGVD-29
    //     2 = OTHER (Pensacola)
    //     3 = UNSET
    //     k2  (k + 1) % 3
    //     k3  (k + 2) % 3
    //
    // l = data units
    //     0 = ft
    //     1 = m
    //     2 = cfs (invalid for datum conversion)
    //
    // m = vertical datum specification method
    //     0 = set default with zset
    //     1 = 0 plus override with user header
    //     2 = 1 plus override with unit spec
    //
    // n = elevation param position
    //     0 = dependent parameter
    //     1 = independent parameter
    //
    // o = data value type
    //     0 = doubles
    //     1 = floats
    //
    // p = specify vertical datum info in user header on store
    //     0 = specify
    //     1 = don't specify (use previously stored)
    //
    // q = ensure empty record
    //     0 = leave any existing data in file
    //     1 = delete existing record
    //
    zset("MLVL", "", 1);
    for (int i = 0; i < 2; ++i) {
        remove(filename[i]);
        for (int j = 0; j < xml_count; ++j) {
            for (int k = 0; k < currentVerticalDatumCount; ++k) {
                int k2 = (k+1) % currentVerticalDatumCount;
                int k3 = (k+2) % currentVerticalDatumCount;
                for (int l = 0; l < unitCount; ++ l) {
                    for (int m = 0; m < 3; ++m) {
                        for (int n = 0; n < 2; ++n) {
                            for (int o = 0; o < 2; ++o) {
                                for (int p = 0; p < 2; ++p) {
                                    for (int q = 0; q < 2; ++q) {
                                        ++count;
                                        len = 0;
                                        headerBuf = NULL;
                                        nativeDatumInFile[0] = '\0';
                                        initializeVerticalDatumInfo(&vdiInFile);
                                        //------------------------//
                                        // create the paired data //
                                        //------------------------//
                                        if (i == 0) {
                                            //-------//
                                            // DSS 6 //
                                            //-------//
                                            status = zopen6(ifltab, filename[i]);
                                            assert(status == STATUS_OKAY);
                                            //----------------------------------------------------------------------------//
                                            // get whether data exists in file and native datum in file for this pathname //
                                            //----------------------------------------------------------------------------//
                                            pds = zstructPdNew(pathnames[n][o]);
                                            assert(pds != NULL);
                                            zset("VDTM", CVERTICAL_DATUM_UNSET, 0);
                                            status = zpdRetrieve(ifltab, pds, 0);
                                            dataInFile = status == STATUS_OKAY && pds->numberOrdinates > 0;
                                            if (status == STATUS_OKAY) {
                                                headerBuf = userHeaderToString(pds->userHeader, pds->userHeaderNumber);
                                                if (headerBuf != NULL) {
                                                    char* compressedVdi = extractFromDelimitedString(
                                                        &headerBuf,
                                                        VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                                                        ":",
                                                        TRUE,
                                                        FALSE,
                                                        ';');
                                                    if (compressedVdi) {
                                                        errmsg = stringToVerticalDatumInfo(&vdiInFile, compressedVdi);
                                                        assert(errmsg == NULL);
                                                        free(compressedVdi);
                                                    }
                                                    free(headerBuf);
                                                }
                                                headerBuf = NULL;
                                            }
                                            zstructFree(pds);
                                        }
                                        else {
                                            //-------//
                                            // DSS 7 //
                                            //-------//
                                            status = zopen7(ifltab, filename[i]);
                                            assert(status == STATUS_OKAY);
                                            //----------------------------------------------------------------------------//
                                            // get whether data exists in file and native datum in file for this pathname //
                                            //----------------------------------------------------------------------------//
                                            pds = zstructPdNew(pathnames[n][o]);
                                            assert(pds != NULL);
                                            zset("VDTM", CVERTICAL_DATUM_UNSET, 0);
                                            status = zpdRetrieve(ifltab, pds, 0);
                                            dataInFile = status == STATUS_OKAY && pds->numberOrdinates > 0;
                                            zStructLocation* ls = zstructLocationNew(pathnames[n][o]);
                                            zlocationRetrieve(ifltab, ls);
                                            if (ls) {
                                                if (ls->supplemental) {
                                                    char* compressedVdi = extractFromDelimitedString(
                                                        &ls->supplemental,
                                                        VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                                                        ":",
                                                        TRUE,
                                                        FALSE,
                                                        ';');
                                                    if (compressedVdi) {
                                                        stringToVerticalDatumInfo(&vdiInFile, compressedVdi);
                                                        free(compressedVdi);
                                                    }
                                                }
                                                zstructFree(ls);
                                            }
                                            zstructFree(pds);
                                        }
                                        strcpy(nativeDatumInFile, vdiInFile.nativeDatum);
                                        //------------------------//
                                        // create the paired data //
                                        //------------------------//
                                        if (o == 0) {
                                            pds = zstructPdNewDoubles(
                                                pathnames[n][o],
                                                dordinates[l],
                                                dvalues[l],
                                                numberOrdinates,
                                                numberCurves,
                                                unit[l],
                                                type,
                                                unit[l],
                                                type);
                                        }
                                        else {
                                            pds = zstructPdNewFloats(
                                                pathnames[n][o],
                                                fordinates[l],
                                                fvalues[l],
                                                numberOrdinates,
                                                numberCurves,
                                                unit[l],
                                                type,
                                                unit[l],
                                                type);
                                        }
                                        assert(pds != NULL);
                                        int K = k;
                                        //--------------------------------//
                                        // set the default vertical datum //
                                        //--------------------------------//
                                        zset("VDTM", currentVerticalDatums[K], 0);
                                        if (p == 0) {
                                            //------------------------------------------------//
                                            // add the vertical datum info to the user header //
                                            //------------------------------------------------//
                                            if (strlen(xml[j]) > 0) {
                                                stringToVerticalDatumInfo(&vdi, xml[j]);
                                                errmsg = gzipAndEncode(&compressed, xml[j]);
                                                assert(errmsg == NULL);
                                                len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 2;
                                                headerBuf = (char*)malloc(len + 1);
                                                memset(headerBuf, 0, len + 1);
                                                status = insertIntoDelimitedString(
                                                    &headerBuf,
                                                    len + 1,
                                                    VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                                                    compressed,
                                                    ":",
                                                    FALSE,
                                                    ';');
                                                assert(status == 0);
                                                free(compressed);
                                                pds->userHeader = stringToUserHeader(headerBuf, &pds->userHeaderNumber);
                                                pds->allocated[zSTRUCT_userHeader] = 1;
                                            }
                                        }
                                        else {
                                            initializeVerticalDatumInfo(&vdi);
                                        }
                                        if (m > 0) {
                                            //----------------------------------------------------------//
                                            // override the default vertical datum with the user header //
                                            //----------------------------------------------------------//
                                            K = k2;
                                            status = insertIntoDelimitedString(
                                                headerBuf ? &headerBuf : NULL,
                                                len,
                                                VERTICAL_DATUM_USER_HEADER_PARAM,
                                                currentVerticalDatums[K],
                                                ":",
                                                FALSE,
                                                ';');
                                            if (status != 0) {
                                                if (headerBuf) {
                                                    int oldlen = len;
                                                    len += VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(currentVerticalDatums[K]) + 3;
                                                    headerBuf = (char*)realloc(headerBuf, len);
                                                    assert(headerBuf != NULL);
                                                    memset(headerBuf + oldlen, 0, len - oldlen);
                                                }
                                                else {
                                                    len = VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(currentVerticalDatums[K]) + 3;
                                                    headerBuf = (char*)malloc(len);
                                                    headerBuf[0] = '\0';
                                                }
                                                assert(headerBuf != NULL);
                                                status = insertIntoDelimitedString(
                                                    &headerBuf,
                                                    len,
                                                    VERTICAL_DATUM_USER_HEADER_PARAM,
                                                    currentVerticalDatums[K],
                                                    ":",
                                                    FALSE,
                                                    ';');
                                                assert(status == 0);
                                            }
                                            free(pds->userHeader);
                                            pds->userHeader = stringToUserHeader(headerBuf, &pds->userHeaderNumber);
                                            pds->allocated[zSTRUCT_userHeader] = TRUE;
                                            if (m > 1) {
                                                //--------------------------------------------------------//
                                                // override default and user header datums with unit spec //
                                                //--------------------------------------------------------//
                                                K = k3;
                                                sprintf(unitSpec, "U=%s|V=%s", unit[l], currentVerticalDatums[K]);
                                                if (n == 0) {
                                                    free(pds->unitsDependent);
                                                    pds->unitsDependent = mallocAndCopy(unitSpec);
                                                }
                                                else {
                                                    free(pds->unitsIndependent);
                                                    pds->unitsIndependent = mallocAndCopy(unitSpec);
                                                }
                                            }
                                        }
                                        if (q == 1) {
                                            //---------------------------//
                                            // delete any record in file //
                                            //---------------------------//
                                            if (dataInFile) {
                                                assert(zdelete(ifltab, pathnames[n][o]) == STATUS_RECORD_FOUND);
                                            }
                                            dataInFile = FALSE;
                                            if (i == 0) {
                                                //--------------------------------------------------//
                                                // deleting records also deletes file VDI for DSS 6 //
                                                //--------------------------------------------------//
                                                initializeVerticalDatumInfo(&vdiInFile);
                                                nativeDatumInFile[0] = '\0';
                                            }
                                        }
                                        //------------------------------------------------//
                                        // figure out whether the zpdStore should succeed //
                                        //------------------------------------------------//
                                        errmsg = processStorageVdis(&offset, &vdiInFile, &vdi, currentVerticalDatums[K], dataInFile, unit[l]);
                                        expectSuccess = errmsg == NULL;
                                        //--------------------------------------------------------//
                                        // store the paired data in the overridden vertical datum //
                                        //--------------------------------------------------------//
                                        printPdTestInfo(count, expectSuccess, pathnames[n][o], dataInFile, &vdiInFile, &vdi, currentVerticalDatums[K], pds->unitsIndependent, pds->unitsDependent);
                                        status = zpdStore(ifltab, pds, 0);
                                        assert((status == STATUS_OKAY) == expectSuccess);
                                        if (status != STATUS_OKAY) {
                                            if (strstr(errmsg, "Data native datum") && strstr(errmsg, "conflicts with file native datum")) {
                                                //--------------------------------------//
                                                // change of vertical datum information //
                                                //                                      //
                                                // set VDOW to override file VDI        //
                                                // with data VDI and re-try             //
                                                //--------------------------------------//
                                                zset("VDOW", "", TRUE);
                                                free(errmsg);
                                                if (m > 1) {
                                                    //-----------------------------------------------//
                                                    // unit spec has already been removed from units //
                                                    //-----------------------------------------------//
                                                    if (n == 0) {
                                                        free(pds->unitsDependent);
                                                        pds->unitsDependent = mallocAndCopy(unitSpec);
                                                    }
                                                    else {
                                                        free(pds->unitsIndependent);
                                                        pds->unitsIndependent = mallocAndCopy(unitSpec);
                                                    }
                                                }
                                                errmsg = processStorageVdis(&offset, &vdiInFile, &vdi, currentVerticalDatums[K], dataInFile, unit[l]);
                                                expectSuccess = errmsg == NULL;
                                                printPdTestInfo(++count, expectSuccess, pathnames[n][o], dataInFile, &vdiInFile, &vdi, currentVerticalDatums[K], pds->unitsIndependent, pds->unitsDependent);
                                                status = zpdStore(ifltab, pds, 0);
                                                assert((status == STATUS_OKAY) == expectSuccess);
                                                assert(errmsg == NULL || !(strstr(errmsg, "Data native datum") && strstr(errmsg, "conflicts with file native datum")));
                                                zset("VDOW", "", FALSE);
                                                if (status == STATUS_OKAY) {
                                                }
                                            }
                                        }
                                        free(errmsg);
                                        zclose(ifltab);
                                        zstructFree(pds);
                                        if (status == STATUS_OKAY) {
                                            //------------------------------------------------------------//
                                            // set the default vertical datum to the datum we stored with //
                                            //------------------------------------------------------------//
                                            zset("VDTM", currentVerticalDatums[K], 0);
                                            //--------------------------------------------------------//
                                            // retrieve the paired data in the default vertical datum //
                                            //--------------------------------------------------------//
                                            if (i == 0) {
                                                status = zopen6(ifltab, filename[i]);
                                            }
                                            else {
                                                status = zopen7(ifltab, filename[i]);
                                            }
                                            assert(status == STATUS_OKAY);
                                            pds = zstructPdNew(pathnames[n][o]);
                                            assert(pds != NULL);
                                            status = zpdRetrieve(ifltab, pds, 0);
                                            assert(status == STATUS_OKAY);
                                            //------------------------------------------------------//
                                            // compare the retrieved paired data to what was stored //
                                            //------------------------------------------------------//
                                            assert(pds->numberOrdinates == numberOrdinates);
                                            assert(pds->numberCurves == numberCurves);
                                            if (o == 0) {
                                                assert(pds->doubleOrdinates != NULL);
                                                assert(pds->doubleValues != NULL);
                                            }
                                            else {
                                                assert(pds->floatOrdinates != NULL);
                                                assert(pds->floatValues != NULL);
                                            }
                                            if (o == 0) {
                                                for (int ii = 0; ii < pds->numberOrdinates; ++ii) {
                                                    assert(pds->doubleOrdinates[ii] == dordinates[l][ii]);
                                                }
                                                for (int ii = 0; ii < pds->numberCurves * pds->numberOrdinates; ++ii) {
                                                    assert(pds->doubleValues[ii] == dvalues[l][ii]);
                                                }
                                            }
                                            else {
                                                for (int ii = 0; ii < pds->numberOrdinates; ++ii) {
                                                    assert(pds->floatOrdinates[ii] == fordinates[l][ii]);
                                                }
                                                for (int ii = 0; ii < pds->numberCurves * pds->numberOrdinates; ++ii) {
                                                    assert(pds->floatValues[ii] == fvalues[l][ii]);
                                                }
                                            }
                                            zclose(ifltab);
                                            zstructFree(pds);
                                        }
                                        if (headerBuf) free(headerBuf);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    printf("\n\n%5d Paired data tests passed\n\n\n", count);
}

void deleteLocationRecord(long long * ifltab, const char *pathname) {
    char *locationPathname = zlocationPath(pathname);
    zdelete(ifltab, locationPathname);
    free(locationPathname);
}

void retrieveAndCompareVdi(long long *ifltab, const char* pathname, const void *sourceData) {
    zStructTimeSeries* tss[2];
    memset(tss, 0, sizeof(tss));
    zStructPairedData* pds[2];
    memset(pds, 0, sizeof(pds));
    zStructLocation* locStruct = NULL;
    verticalDatumInfo* pVdi = NULL;
    char* vdiXml[2];
    memset(vdiXml, 0, sizeof(vdiXml));
    char* errmsg = NULL;
    char* compressed = NULL;
    char startDate[10];
    char startTime[16]; // only using 6, but keeping compiler happy
    char endDate[10];
    char endTime[16]; // only using 6, but keeping compiler happy
    char filename[_MAX_PATH];
    memset(filename, 0, sizeof(filename));
    int* structType = (int*)sourceData;
    int status;
    int dummy = 0;
    int dssver;
    int* userHeader = NULL;
    int userHeaderNumber = 0;
    void* destinationData = NULL;

    zinquireChar(ifltab, "name", filename, sizeof(filename), &dummy);
    dssver = (int)zgetVersion(ifltab);

    fprintf(stderr, "%p\t%d\n", sourceData, *(int*)sourceData);
    switch (*(int*)sourceData) {
    case DATA_TYPE_RTS:
        //-------------//
        // time series //
        //-------------//
        tss[SRC] = (zStructTimeSeries*)sourceData;
        //--------------------//
        // get the source VDI //
        //--------------------//
        if (tss[SRC]->userHeaderNumber) {
            pVdi = extractVerticalDatumInfoFromUserHeader(tss[SRC]->userHeader, tss[SRC]->userHeaderNumber);
            assert(pVdi != NULL);
            errmsg = verticalDatumInfoToString(&vdiXml[SRC], pVdi, FALSE);
            free(pVdi);
            assert(errmsg == NULL);
            assert(vdiXml[SRC] != NULL);
        }
        else if (tss[SRC]->locationStruct != NULL && tss[SRC]->locationStruct->supplemental != NULL) {
            locStruct = tss[SRC]->locationStruct;
            compressed = extractFromDelimitedString(&locStruct->supplemental, VERTICAL_DATUM_INFO_USER_HEADER_PARAM, ":", TRUE, FALSE, ';');
            assert(compressed != NULL);
            errmsg = decodeAndGunzip(&vdiXml[SRC], compressed);
            free(compressed);
            assert(errmsg == NULL);
            assert(vdiXml[SRC] != NULL);
        }
        if (vdiXml[SRC] != NULL) {
            //--------------------------------------//
            // retrieve the destination time series //
            //--------------------------------------//
            julianToDate(tss[SRC]->timeWindow->startJulian, 4, startDate, sizeof(startDate));
            sprintf(startTime, "%2.2d:%2.2d", tss[SRC]->timeWindow->startTimeSeconds / SECS_IN_1_HOUR, tss[SRC]->timeWindow->startTimeSeconds % SECS_IN_1_HOUR);
            julianToDate(tss[SRC]->timeWindow->endJulian, 4, endDate, sizeof(endDate));
            sprintf(endTime, "%2.2d:%2.2d", tss[SRC]->timeWindow->endTimeSeconds / SECS_IN_1_HOUR, tss[SRC]->timeWindow->endTimeSeconds % SECS_IN_1_HOUR);
            tss[DST] = zstructTsNewTimes(
                pathname,
                startDate,
                startTime,
                endDate,
                endTime);
            assert(tss[DST] != NULL);
            status = ztsRetrieve(
                ifltab,   // file table
                tss[DST], // time series struct
                0,        // retrieve flag (0=adhere to time window and [for reg] create times array)
                0,        // retrieve doubles flag (0=as stored, 1=floats, 2=doubles)
                1);       // retrieve quality flag (0/1)
            assert(status == STATUS_OKAY);
            locStruct = tss[DST]->locationStruct;
            userHeader = tss[DST]->userHeader;
            userHeaderNumber = tss[DST]->userHeaderNumber;
            destinationData = tss[DST];
        }
        break;
    case DATA_TYPE_PD: 
        //-------------//
        // paired data //
        //-------------//
        pds[SRC] = (zStructPairedData*)sourceData;
        //--------------------//
        // get the source VDI //
        //--------------------//
        if (pds[SRC]->userHeaderNumber) {
            pVdi = extractVerticalDatumInfoFromUserHeader(pds[SRC]->userHeader, pds[SRC]->userHeaderNumber);
            assert(pVdi != NULL);
            errmsg = verticalDatumInfoToString(&vdiXml[SRC], pVdi, FALSE);
            free(pVdi);
            assert(errmsg == NULL);
        }
        else if (pds[SRC]->locationStruct != NULL && pds[SRC]->locationStruct->supplemental != NULL) {
            locStruct = pds[SRC]->locationStruct;
            compressed = extractFromDelimitedString(&locStruct->supplemental, VERTICAL_DATUM_INFO_USER_HEADER_PARAM, ":", TRUE, FALSE, ';');
            assert(compressed != NULL);
            errmsg = decodeAndGunzip(&vdiXml[SRC], compressed);
            free(compressed);
            assert(errmsg == NULL);
            assert(vdiXml[SRC] != NULL);
        }
        if (vdiXml[SRC] != NULL) {
            //--------------------------------------//
            // retrieve the destination time series //
            //--------------------------------------//
            pds[DST] = zstructPdNew(pathname);
            assert(pds[DST] != NULL);
            status = zpdRetrieve(ifltab, pds[DST], 0);
            assert(status == STATUS_OKAY);
            userHeader = pds[DST]->userHeader;
            userHeaderNumber = pds[DST]->userHeaderNumber;
            locStruct = pds[DST]->locationStruct;
            destinationData = pds[DST];
        }
        break;
    default:
        assert(FALSE);
    }
    //----------------------------------------------//
    // verify the destination VDI is same as source //
    //----------------------------------------------//
    if (userHeaderNumber != 0) {
        pVdi = extractVerticalDatumInfoFromUserHeader(userHeader, userHeaderNumber);
        assert(pVdi != NULL);
        errmsg = verticalDatumInfoToString(&vdiXml[DST], pVdi, FALSE);
        free(pVdi);
        assert(errmsg == NULL);
    }
    else if (locStruct != NULL && locStruct->supplemental != NULL) {
        compressed = extractFromDelimitedString(&locStruct->supplemental, VERTICAL_DATUM_INFO_USER_HEADER_PARAM, ":", TRUE, FALSE, ';');
        assert(compressed != NULL);
        errmsg = decodeAndGunzip(&vdiXml[DST], compressed);
        free(compressed);
        assert(errmsg == NULL);
        assert(vdiXml[DST] != NULL);
    }
    assert((vdiXml[SRC] == NULL) == (vdiXml[DST] == NULL));
    if (vdiXml[DST] != NULL) {
        printf("\n==> %s:%s (v%d)\n%s\n", filename, pathname, dssver, vdiXml[DST]);
        assert(strcmp(vdiXml[DST], vdiXml[SRC]) == 0);
        free(vdiXml[SRC]);
        free(vdiXml[DST]);
        zstructFree(destinationData);
    }
}

void testCopyRecordWithVdi_NoVdiInDestination() {
    long long ifltab[2][250];
    memset(ifltab, 0, sizeof(ifltab));
    int status;
    zStructTimeSeries* tss = NULL;
    zStructPairedData* pds = NULL;
    verticalDatumInfo vdi[2];
    memset(vdi, 0, sizeof(vdi));
    verticalDatumInfo* pVdi;
    const char* filename[2] = {"vdiCopyTestSource.dss", "vdiCopyTestDestination.dss"};
    const char* tsPathname[2] = { "//TsSourceLoc/Elev/01Oct2021/1Hour/Test/", "//TsDestinationLoc/Elev/01Oct2021/1Hour/Test/" };
    const char* pdPathname[2] = { "//PdSourceLoc/Stage-Elev///Test/", "//PdDestinationLoc/Stage-Elev///Test/" };
    double tsValues[] = {1000,1001,1002,1003,1004,1005};
    int numberTsValues = sizeof(tsValues) / sizeof(tsValues[0]);
    double pdOrdinates[] = { 1000,1001,1002,1003,1004,1005 };
    double pdValues[] = { 1000,1001,1002,1003,1004,1005 };
    int numberPdOrdinates = 6;
    int numberPdCurves = 1;
    char* startDate = "01Oct2021";
    char* startTime = "01:00";
    char* endDate = "01Oct2021";
    char* endTime = "24:00";
    char* unit = "ft";
    char* type = "INST-VAL";
    char* errmsg = NULL;
    char* compressed = NULL;
    char* headerBuf = NULL;
    int   len = 0;
    int   dssver;
    char* vdiXml = NULL;
    const char* _xml[] = {
        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.2</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>0.3855</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.2</elevation>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NAVD-88</native-datum>\n"
        "  <elevation>615.5885</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>-0.3855</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NAVD-88</native-datum>\n"
        "  <elevation>615.5885</elevation>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>OTHER</native-datum>\n"
        "  <local-datum-name>Pensacola</local-datum-name>\n"
        "  <elevation>757</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>1.457</value>\n"
        "  </offset>\n"
        "  <offset estimate=\"false\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>1.07</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>OTHER</native-datum>\n"
        "  <local-datum-name>Pensacola</local-datum-name>\n"
        "  <elevation>757</elevation>\n"
        "</vertical-datum-info>\n",

        ""
    };
    const int xmlCount = sizeof(_xml) / sizeof(_xml[0]);
    char *xml[sizeof(_xml) / sizeof(_xml[0])];
    memset(xml, 0, sizeof(xml));

    //-------------------//
    // normalize the xml //
    //-------------------//
    for (int i = 0; i < xmlCount; ++i) {
        if (strlen(_xml[i]) > 0) {
            errmsg = stringToVerticalDatumInfo(&vdi[0], _xml[i]);
            assert(errmsg == NULL);
            char *cp = NULL;
            errmsg = verticalDatumInfoToString(&cp, &vdi[0], FALSE);
            assert(errmsg == NULL);
            xml[i] = cp;
        }
        else {
            xml[i] = strdup("");
        }
    }
    for (int srcDssVer = 6; srcDssVer <= 7; ++srcDssVer) {
        for (int dstDssVer = 6; dstDssVer <= 7; ++dstDssVer) {

            remove(filename[SRC]);
            if (srcDssVer == 6) {
                status = zopen6(ifltab[SRC], filename[SRC]);
            }
            else {
                status = zopen7(ifltab[SRC], filename[SRC]);
            }
            assert(status == STATUS_OKAY);
            dssver = (int)zgetVersion(ifltab[SRC]);

            remove(filename[DST]);
            if (dstDssVer == 6) {
                status = zopen6(ifltab[DST], filename[DST]);
            }
            else {
                status = zopen7(ifltab[DST], filename[DST]);
            }
            assert(status == STATUS_OKAY);

            // pVdi = malloc(sizeof(verticalDatumInfo));
            // assert(pVdi != NULL);

            for (int i = 0; i < xmlCount; ++i) {
                printf("\n-------------------------------\n%3d\n-------------------------------\n", i);
                printf("==> Source (v%d)\n%s\n", dssver, xml[i]);
                //-----------------------------//
                // create a source time series //
                //-----------------------------//
                tss = zstructTsNewRegDoubles(
                    tsPathname[SRC],
                    tsValues,
                    numberTsValues,
                    startDate,
                    startTime,
                    unit,
                    type);
                assert(tss != NULL);
                //-----------------------------//
                // create a source paired data //
                //-----------------------------//
                pds = zstructPdNewDoubles(
                    pdPathname[SRC],
                    pdOrdinates,
                    pdValues,
                    numberPdOrdinates,
                    numberPdCurves,
                    unit,
                    "UNT",
                    unit,
                    "UNT");
                //-----------------------------------------------//
                // add VDI to source time series and paired data //
                //-----------------------------------------------//
                if (strlen(xml[i]) > 0) {
                    stringToVerticalDatumInfo(&vdi[SRC], xml[i]);
                    errmsg = gzipAndEncode(&compressed, xml[i]);
                    assert(errmsg == NULL);
                    len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 2;
                    headerBuf = (char*)malloc(len + 1);
                    assert(headerBuf != NULL);
                    memset(headerBuf, 0, len + 1);
                    status = insertIntoDelimitedString(
                        &headerBuf,
                        len + 1,
                        VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                        compressed,
                        ":",
                        FALSE,
                        ';');
                    assert(status == STATUS_OKAY);
                    free(compressed);
                    tss->userHeader = stringToUserHeader(headerBuf, &tss->userHeaderNumber);
                    tss->allocated[zSTRUCT_userHeader] = TRUE;
                    pds->userHeader = stringToUserHeader(headerBuf, &pds->userHeaderNumber);
                    pds->allocated[zSTRUCT_userHeader] = TRUE;
                    free(headerBuf);
                }
                //-----------------------------//
                // delete any previous records //
                //-----------------------------//
                zdelete(ifltab[SRC], tsPathname[SRC]);
                deleteLocationRecord(ifltab[SRC], tsPathname[SRC]);
                zdelete(ifltab[SRC], pdPathname[SRC]);
                deleteLocationRecord(ifltab[SRC], pdPathname[SRC]);
                //-----------------------------------------------------//
                // store the source time series to the source DSS file //
                //-----------------------------------------------------//
                status = ztsStore(
                    ifltab[SRC], // file table
                    tss,         // time series struct
                    0);          // storage flag (0=reg:replace all, irr:merge)
                assert(status == STATUS_OKAY);
                //-------------------------------//
                // verify that we stored the VDI //
                //-------------------------------//
                retrieveAndCompareVdi(ifltab[SRC], tsPathname[SRC], tss);
                //------------------------------//
                // store the source paired data //
                //------------------------------//
                status = zpdStore(ifltab[SRC], pds, 0);
                assert(status == STATUS_OKAY);
                //-------------------------------//
                // verify that we stored the VDI //
                //-------------------------------//
                retrieveAndCompareVdi(ifltab[SRC], pdPathname[SRC], pds);
                //-------------------------------------------------------------------------//
                // copy the time series to a location in the same DSS file that has no VDI //
                //-------------------------------------------------------------------------//
                zdelete(ifltab[SRC], tsPathname[DST]);
                deleteLocationRecord(ifltab[SRC], tsPathname[DST]);
                status = zcopyRecord(ifltab[SRC], ifltab[SRC], tsPathname[SRC], tsPathname[DST]);
                assert(status == STATUS_OKAY);
                //---------------------------//
                // verify the VDI was copied //
                //---------------------------//
                retrieveAndCompareVdi(ifltab[SRC], tsPathname[DST], tss);
                //-------------------------------------------------------------------------//
                // copy the paired data to a location in the same DSS file that has no VDI //
                //-------------------------------------------------------------------------//
                zdelete(ifltab[SRC], pdPathname[DST]);
                deleteLocationRecord(ifltab[SRC], pdPathname[DST]);
                status = zcopyRecord(ifltab[SRC], ifltab[SRC], pdPathname[SRC], pdPathname[DST]);
                assert(status == STATUS_OKAY);
                //---------------------------//
                // verify the VDI was copied //
                //---------------------------//
                retrieveAndCompareVdi(ifltab[SRC], pdPathname[DST], pds);
                //------------------------------------------------------------------------//
                // copy the time series to a location in another DSS file that has no VDI //
                //------------------------------------------------------------------------//
                zdelete(ifltab[DST], tsPathname[DST]);
                deleteLocationRecord(ifltab[DST], tsPathname[DST]);
                status = zcopyRecord(ifltab[SRC], ifltab[DST], tsPathname[SRC], tsPathname[DST]);
                assert(status == STATUS_OKAY);
                //---------------------------//
                // verify the VDI was copied //
                //---------------------------//
                retrieveAndCompareVdi(ifltab[DST], tsPathname[DST], tss);
                //------------------------------------------------------------------------//
                // copy the paired data to a location in another DSS file that has no VDI //
                //------------------------------------------------------------------------//
                zdelete(ifltab[DST], pdPathname[DST]);
                deleteLocationRecord(ifltab[DST], pdPathname[DST]);
                status = zcopyRecord(ifltab[SRC], ifltab[DST], pdPathname[SRC], pdPathname[DST]);
                assert(status == STATUS_OKAY);
                //---------------------------//
                // verify the VDI was copied //
                //---------------------------//
                retrieveAndCompareVdi(ifltab[DST], pdPathname[DST], pds);
                zstructFree(tss);
                zstructFree(pds);
            }
            zclose(ifltab[SRC]);
            zclose(ifltab[DST]);
        }
    }
    for (int i = 0; i < xmlCount; ++i) {
        free(xml[i]);
    }
    remove(filename[SRC]);
    remove(filename[DST]);
}

void testCopyRecordWithVdi_OtherNativeDatumInDestination() {
    long long ifltab[2][250];
    memset(ifltab, 0, sizeof(ifltab));
    int status;
    zStructTimeSeries* tss = NULL;
    zStructPairedData* pds = NULL;
    verticalDatumInfo vdi[2];
    memset(vdi, 0, sizeof(vdi));
    const char* filename[2] = { "vdiCopyTestSource.dss", "vdiCopyTestDestination.dss" };
    const char* tsPathname[2] = { "//TsSourceLoc/Elev/01Oct2021/1Hour/Test/", "//TsDestinationLoc/Elev/01Oct2021/1Hour/Test/" };
    const char* pdPathname[2] = { "//PdSourceLoc/Stage-Elev///Test/", "//PdDestinationLoc/Stage-Elev///Test/" };
    double tsValues[] = { 1000,1001,1002,1003,1004,1005 };
    int numberTsValues = sizeof(tsValues) / sizeof(tsValues[0]);
    double pdOrdinates[] = { 1000,1001,1002,1003,1004,1005 };
    double pdValues[] = { 1000,1001,1002,1003,1004,1005 };
    int numberPdOrdinates = 6;
    int numberPdCurves = 1;
    char* startDate = "01Oct2021";
    char* startTime = "01:00";
    char* endDate = "01Oct2021";
    char* endTime = "24:00";
    char* unit = "ft";
    char* type = "INST-VAL";
    char* errmsg = NULL;
    char* compressed = NULL;
    char* headerBuf = NULL;
    int   len = 0;
    int   dssver;
    char* vdiXml = NULL;
    char* xml[] = {
        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.2</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>0.3855</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",

        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NAVD-88</native-datum>\n"
        "  <elevation>615.5885</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>-0.3855</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n"
    };
    int xmlCount = sizeof(xml) / sizeof(xml[0]);

    zset("MLVL", "", 0);
    for (int srcDssVer = 6; srcDssVer <= 7; ++srcDssVer) {
        for (int dstDssVer = 6; dstDssVer <= 7; ++dstDssVer) {

            remove(filename[SRC]);
            if (srcDssVer == 6) {
                status = zopen6(ifltab[SRC], filename[SRC]);
            }
            else {
                status = zopen7(ifltab[SRC], filename[SRC]);
            }
            assert(status == STATUS_OKAY);
            dssver = (int)zgetVersion(ifltab[SRC]);

            remove(filename[DST]);
            if (dstDssVer == 6) {
                status = zopen6(ifltab[DST], filename[DST]);
            }
            else {
                status = zopen7(ifltab[DST], filename[DST]);
            }
            assert(status == STATUS_OKAY);

            for (int i = 0; i < xmlCount; ++i) {
                int j = (i + 1) % xmlCount; // for other VDI
                printf("\n-------------------------------\n%3d\n-------------------------------\n", i);
                stringToVerticalDatumInfo(&vdi[SRC], xml[i]);
                printf("==> Source (v%d) : %s\n", dssver, vdi[i].nativeDatum);
                //-----------------------------//
                // create a source time series //
                //-----------------------------//
                tss = zstructTsNewRegDoubles(
                    tsPathname[SRC],
                    tsValues,
                    numberTsValues,
                    startDate,
                    startTime,
                    unit,
                    type);
                assert(tss != NULL);
                //-----------------------------//
                // create a source paired data //
                //-----------------------------//
                pds = zstructPdNewDoubles(
                    pdPathname[SRC],
                    pdOrdinates,
                    pdValues,
                    numberPdOrdinates,
                    numberPdCurves,
                    unit,
                    "UNT",
                    unit,
                    "UNT");
                //-----------------------------------------------//
                // add VDI to source time series and paired data //
                //-----------------------------------------------//
                stringToVerticalDatumInfo(&vdi[SRC], xml[i]);
                errmsg = gzipAndEncode(&compressed, xml[i]);
                assert(errmsg == NULL);
                len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 2;
                headerBuf = (char*)malloc(len + 1);
                assert(headerBuf != NULL);
                memset(headerBuf, 0, len + 1);
                status = insertIntoDelimitedString(
                    &headerBuf,
                    len + 1,
                    VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                    compressed,
                    ":",
                    FALSE,
                    ';');
                assert(status == STATUS_OKAY);
                free(compressed);
                tss->userHeader = stringToUserHeader(headerBuf, &tss->userHeaderNumber);
                tss->allocated[zSTRUCT_userHeader] = TRUE;
                pds->userHeader = stringToUserHeader(headerBuf, &pds->userHeaderNumber);
                pds->allocated[zSTRUCT_userHeader] = TRUE;
                free(headerBuf);
                //-----------------------------//
                // delete any previous records //
                //-----------------------------//
                zdelete(ifltab[SRC], tsPathname[SRC]);
                deleteLocationRecord(ifltab[SRC], tsPathname[SRC]);
                zdelete(ifltab[SRC], pdPathname[SRC]);
                deleteLocationRecord(ifltab[SRC], pdPathname[SRC]);
                //-----------------------------------------------------//
                // store the source time series to the source DSS file //
                //-----------------------------------------------------//
                status = ztsStore(
                    ifltab[SRC], // file table
                    tss,         // time series struct
                    0);          // storage flag (0=reg:replace all, irr:merge)
                assert(status == STATUS_OKAY);
                zstructFree(tss);
                //-----------------------------------------------------//
                // store the source paired data to the source DSS file //
                //-----------------------------------------------------//
                status = zpdStore(ifltab[SRC], pds, 0);
                assert(status == STATUS_OKAY);
                zstructFree(pds);

                //----------------------------------//
                // create a destination time series //
                //----------------------------------//
                tss = zstructTsNewRegDoubles(
                    tsPathname[DST],
                    tsValues,
                    numberTsValues,
                    startDate,
                    startTime,
                    unit,
                    type);
                assert(tss != NULL);
                //----------------------------------//
                // create a destination paired data //
                //----------------------------------//
                pds = zstructPdNewDoubles(
                    pdPathname[DST],
                    pdOrdinates,
                    pdValues,
                    numberPdOrdinates,
                    numberPdCurves,
                    unit,
                    "UNT",
                    unit,
                    "UNT");
                //----------------------------------------------------//
                // add VDI to destination time series and paired data //
                //----------------------------------------------------//
                stringToVerticalDatumInfo(&vdi[DST], xml[j]);
                errmsg = gzipAndEncode(&compressed, xml[j]);
                assert(errmsg == NULL);
                len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 2;
                headerBuf = (char*)malloc(len + 1);
                assert(headerBuf != NULL);
                memset(headerBuf, 0, len + 1);
                status = insertIntoDelimitedString(
                    &headerBuf,
                    len + 1,
                    VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                    compressed,
                    ":",
                    FALSE,
                    ';');
                assert(status == STATUS_OKAY);
                free(compressed);
                tss->userHeader = stringToUserHeader(headerBuf, &tss->userHeaderNumber);
                tss->allocated[zSTRUCT_userHeader] = TRUE;
                pds->userHeader = stringToUserHeader(headerBuf, &pds->userHeaderNumber);
                pds->allocated[zSTRUCT_userHeader] = TRUE;
                free(headerBuf);
                //-----------------------------//
                // delete any previous records //
                //-----------------------------//
                zdelete(ifltab[SRC], tsPathname[DST]);
                deleteLocationRecord(ifltab[SRC], tsPathname[DST]);
                zdelete(ifltab[SRC], pdPathname[DST]);
                deleteLocationRecord(ifltab[SRC], pdPathname[DST]);
                zdelete(ifltab[DST], tsPathname[DST]);
                deleteLocationRecord(ifltab[DST], tsPathname[DST]);
                zdelete(ifltab[DST], pdPathname[DST]);
                deleteLocationRecord(ifltab[DST], pdPathname[DST]);
                //----------------------------------------------------------//
                // store the destination time series to the source DSS file //
                //----------------------------------------------------------//
                status = ztsStore(
                    ifltab[SRC], // file table
                    tss,         // time series struct
                    0);          // storage flag (0=reg:replace all, irr:merge)
                assert(status == STATUS_OKAY);
                //---------------------------------------------------------------//
                // store the destination time series to the destination DSS file //
                //---------------------------------------------------------------//
                status = ztsStore(
                    ifltab[DST], // file table
                    tss,         // time series struct
                    0);          // storage flag (0=reg:replace all, irr:merge)
                assert(status == STATUS_OKAY);
                //----------------------------------------------------------//
                // store the destination paired data to the source DSS file //
                //----------------------------------------------------------//
                status = zpdStore(ifltab[SRC], pds, 0);
                assert(status == STATUS_OKAY);
                //---------------------------------------------------------------//
                // store the destination paired data to the destination DSS file //
                //---------------------------------------------------------------//
                status = zpdStore(ifltab[DST], pds, 0);
                assert(status == STATUS_OKAY);
                zstructFree(tss);
                zstructFree(pds);
                if (dstDssVer == srcDssVer) {
                    //-------------------------------------------------------------------------------------------//
                    // copy the time series to a location in the same DSS file that has a different native datum //
                    //-------------------------------------------------------------------------------------------//
                    printf("==> %s:%s (v%d)\n", filename[SRC], tsPathname[DST], dstDssVer);
                    status = zcopyRecord(ifltab[SRC], ifltab[SRC], tsPathname[SRC], tsPathname[DST]);
                    if (dstDssVer == 6) {
                        assert(status == STATUS_OKAY);
                        //-------------------------------------------------------//
                        // retrieve the copied time series and verify the values //
                        //-------------------------------------------------------//
                        tss = zstructTsNewTimes(
                            tsPathname[DST],
                            startDate,
                            startTime,
                            endDate,
                            endTime);
                        assert(tss != NULL);
                        status = ztsRetrieve(
                            ifltab[SRC], // file table
                            tss,         // time series struct
                            0,           // retrieve flag (0=adhere to time window and [for reg] create times array)
                            0,           // retrieve doubles flag (0=as stored, 1=floats, 2=doubles)
                            1);          // retrieve quality flag (0/1)
                        assert(status == STATUS_OKAY);
                        for (int i = 0; i < numberTsValues; ++i) {
                            assert(fabs(tss->doubleValues[i] - tsValues[i]) < FLT_EPSILON);
                        }
                        zstructFree(tss);
                    }
                    else {
                        assert(status != STATUS_OKAY);
                    }
                }
                //------------------------------------------------------------------------------------------//
                // copy the time series to a location in another DSS file that has a different native datum //
                //------------------------------------------------------------------------------------------//
                printf("==> %s:%s (v%d)\n", filename[SRC], tsPathname[DST], dstDssVer);
                status = zcopyRecord(ifltab[SRC], ifltab[DST], tsPathname[SRC], tsPathname[DST]);
                if (dstDssVer == 6) {
                    assert(status == STATUS_OKAY);
                    //-------------------------------------------------------//
                    // retrieve the copied time series and verify the values //
                    //-------------------------------------------------------//
                    tss = zstructTsNewTimes(
                        tsPathname[DST],
                        startDate,
                        startTime,
                        endDate,
                        endTime);
                    assert(tss != NULL);
                    status = ztsRetrieve(
                        ifltab[DST], // file table
                        tss,         // time series struct
                        0,           // retrieve flag (0=adhere to time window and [for reg] create times array)
                        0,           // retrieve doubles flag (0=as stored, 1=floats, 2=doubles)
                        1);          // retrieve quality flag (0/1)
                    assert(status == STATUS_OKAY);
                    for (int i = 0; i < numberTsValues; ++i) {
                        assert(fabs(tss->doubleValues[i] - tsValues[i]) < FLT_EPSILON);
                    }
                    zstructFree(tss);
                }
                else {
                    assert(status != STATUS_OKAY);
                }
                if (dstDssVer == srcDssVer) {
                    //-------------------------------------------------------------------------------------------//
                    // copy the paired data to a location in the same DSS file that has a different native datum //
                    //-------------------------------------------------------------------------------------------//
                    printf("==> %s:%s (v%d)\n", filename[DST], pdPathname[DST], dstDssVer);
                    status = zcopyRecord(ifltab[SRC], ifltab[SRC], pdPathname[SRC], pdPathname[DST]);
                    if (dstDssVer == 6) {
                        assert(status == STATUS_OKAY);
                        //-------------------------------------------------------//
                        // retrieve the copied paired data and verify the values //
                        //-------------------------------------------------------//
                        pds = zstructPdNew(pdPathname[DST]);
                        assert(pds != NULL);
                        status = zpdRetrieve(ifltab[SRC], pds, 0);
                        assert(status == STATUS_OKAY);
                        for (int i = 0; i < numberPdOrdinates; ++i) {
                            assert(fabs(pds->doubleValues[i] - pdValues[i]) < FLT_EPSILON);
                        }
                        zstructFree(pds);
                    }
                    else {
                        assert(status != STATUS_OKAY);
                    }
                }
                //------------------------------------------------------------------------------------------//
                // copy the paired data to a location in another DSS file that has a different native datum //
                //------------------------------------------------------------------------------------------//
                printf("==> %s:%s (v%d)\n", filename[DST], pdPathname[DST], dstDssVer);
                status = zcopyRecord(ifltab[SRC], ifltab[DST], pdPathname[SRC], pdPathname[DST]);
                if (dstDssVer == 6) {
                    assert(status == STATUS_OKAY);
                    //-------------------------------------------------------//
                    // retrieve the copied paired data and verify the values //
                    //-------------------------------------------------------//
                    pds = zstructPdNew(pdPathname[DST]);
                    assert(pds != NULL);
                    status = zpdRetrieve(ifltab[DST], pds, 0);
                    assert(status == STATUS_OKAY);
                    for (int i = 0; i < numberPdOrdinates; ++i) {
                        assert(fabs(pds->doubleValues[i] - pdValues[i]) < FLT_EPSILON);
                    }
                    zstructFree(pds);
                }
                else {
                    assert(status != STATUS_OKAY);
                }
            }
            zclose(ifltab[SRC]);
            zclose(ifltab[DST]);
        }
    }
    remove(filename[SRC]);
    remove(filename[DST]);
}

void testCopyRecordWithVdi_SameNativeDatumInDestination() {
    long long ifltab[2][250];
    memset(ifltab, 0, sizeof(ifltab));
    int status;
    zStructTimeSeries* tss = NULL;
    zStructPairedData* pds = NULL;
    verticalDatumInfo vdi;
    const char* filename[2] = { "vdiCopyTestSource.dss", "vdiCopyTestDestination.dss" };
    const char* tsPathname[2] = { "//TsSourceLoc/Elev/01Oct2021/1Hour/Test/", "//TsDestinationLoc/Elev/01Oct2021/1Hour/Test/" };
    const char* pdPathname[2] = { "//PdSourceLoc/Stage-Elev///Test/", "//PdDestinationLoc/Stage-Elev///Test/" };
    double tsValues[2][6] = { { 1000,1001,1002,1003,1004,1005 }, {1,2,3,4,5} };
    int numberTsValues = 6;
    double pdOrdinates[] = { 1000,1001,1002,1003,1004,1005 };
    double pdValues[2][6] = { {1000,1001,1002,1003,1004,1005}, {1,2,3,4,5} };
    int numberPdOrdinates = 6;
    int numberPdCurves = 1;
    char* startDate = "01Oct2021";
    char* startTime = "01:00";
    char* endDate = "01Oct2021";
    char* endTime = "24:00";
    char* unit = "ft";
    char* type = "INST-VAL";
    char* errmsg = NULL;
    char* compressed = NULL;
    char* headerBuf = NULL;
    int   len = 0;
    int   dssver;
    int   i;
    char* vdiXml = NULL;
    char* xml = {
        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.2</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>0.3855</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n"
    };

    for (int srcDssVer = 6; srcDssVer <= 7; ++srcDssVer) {
        for (int dstDssVer = 6; dstDssVer <= 7; ++dstDssVer) {

            remove(filename[SRC]);
            if (srcDssVer == 6) {
                status = zopen6(ifltab[SRC], filename[SRC]);
            }
            else {
                status = zopen7(ifltab[SRC], filename[SRC]);
            }
            assert(status == STATUS_OKAY);
            dssver = (int)zgetVersion(ifltab[SRC]);

            remove(filename[DST]);
            if (dstDssVer == 6) {
                status = zopen6(ifltab[DST], filename[DST]);
            }
            else {
                status = zopen7(ifltab[DST], filename[DST]);
            }
            assert(status == STATUS_OKAY);

            stringToVerticalDatumInfo(&vdi, xml);
            printf("==> Source (v%d) : %s\n", dssver, vdi.nativeDatum);
            //-----------------------------//
            // create a source time series //
            //-----------------------------//
            tss = zstructTsNewRegDoubles(
                tsPathname[SRC],
                tsValues[SRC],
                numberTsValues,
                startDate,
                startTime,
                unit,
                type);
            assert(tss != NULL);
            //-----------------------------//
            // create a source paired data //
            //-----------------------------//
            pds = zstructPdNewDoubles(
                pdPathname[SRC],
                pdOrdinates,
                pdValues[SRC],
                numberPdOrdinates,
                numberPdCurves,
                unit,
                "UNT",
                unit,
                "UNT");
            //-----------------------------------------------//
            // add VDI to source time series and paired data //
            //-----------------------------------------------//
            stringToVerticalDatumInfo(&vdi, xml);
            errmsg = gzipAndEncode(&compressed, xml);
            assert(errmsg == NULL);
            len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 2;
            headerBuf = (char*)malloc(len + 1);
            assert(headerBuf != NULL);
            memset(headerBuf, 0, len + 1);
            status = insertIntoDelimitedString(
                &headerBuf,
                len + 1,
                VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                compressed,
                ":",
                FALSE,
                ';');
            assert(status == STATUS_OKAY);
            free(compressed);
            tss->userHeader = stringToUserHeader(headerBuf, &tss->userHeaderNumber);
            tss->allocated[zSTRUCT_userHeader] = TRUE;
            pds->userHeader = stringToUserHeader(headerBuf, &pds->userHeaderNumber);
            pds->allocated[zSTRUCT_userHeader] = TRUE;
            free(headerBuf);
            //-----------------------------//
            // delete any previous records //
            //-----------------------------//
            zdelete(ifltab[SRC], tsPathname[SRC]);
            deleteLocationRecord(ifltab[SRC], tsPathname[SRC]);
            zdelete(ifltab[SRC], pdPathname[SRC]);
            deleteLocationRecord(ifltab[SRC], pdPathname[SRC]);
            //-----------------------------------------------------//
            // store the source time series to the source DSS file //
            //-----------------------------------------------------//
            status = ztsStore(
                ifltab[SRC], // file table
                tss,         // time series struct
                0);          // storage flag (0=reg:replace all, irr:merge)
            assert(status == STATUS_OKAY);
            zstructFree(tss);
            //------------------------------//
            // store the source paired data //
            //------------------------------//
            status = zpdStore(ifltab[SRC], pds, 0);
            assert(status == STATUS_OKAY);
            zstructFree(pds);
            //----------------------------------//
            // create a destination time series //
            //----------------------------------//
            tss = zstructTsNewRegDoubles(
                tsPathname[DST],
                tsValues[DST],
                numberTsValues,
                startDate,
                startTime,
                unit,
                type);
            assert(tss != NULL);
            //----------------------------------//
            // create a destination paired data //
            //----------------------------------//
            pds = zstructPdNewDoubles(
                pdPathname[DST],
                pdOrdinates,
                pdValues[DST],
                numberPdOrdinates,
                numberPdCurves,
                unit,
                "UNT",
                unit,
                "UNT");
            //----------------------------------------------------//
            // add VDI to destination time series and paired data //
            //----------------------------------------------------//
            stringToVerticalDatumInfo(&vdi, xml);
            errmsg = gzipAndEncode(&compressed, xml);
            assert(errmsg == NULL);
            len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 2;
            headerBuf = (char*)malloc(len + 1);
            assert(headerBuf != NULL);
            memset(headerBuf, 0, len + 1);
            status = insertIntoDelimitedString(
                &headerBuf,
                len + 1,
                VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
                compressed,
                ":",
                FALSE,
                ';');
            assert(status == STATUS_OKAY);
            free(compressed);
            tss->userHeader = stringToUserHeader(headerBuf, &tss->userHeaderNumber);
            tss->allocated[zSTRUCT_userHeader] = TRUE;
            pds->userHeader = stringToUserHeader(headerBuf, &pds->userHeaderNumber);
            pds->allocated[zSTRUCT_userHeader] = TRUE;
            free(headerBuf);
            //-----------------------------//
            // delete any previous records //
            //-----------------------------//
            zdelete(ifltab[SRC], tsPathname[DST]);
            deleteLocationRecord(ifltab[SRC], tsPathname[DST]);
            zdelete(ifltab[SRC], pdPathname[DST]);
            deleteLocationRecord(ifltab[SRC], pdPathname[DST]);
            zdelete(ifltab[DST], tsPathname[DST]);
            deleteLocationRecord(ifltab[DST], tsPathname[DST]);
            zdelete(ifltab[DST], pdPathname[DST]);
            deleteLocationRecord(ifltab[DST], pdPathname[DST]);
            //----------------------------------------------------------//
            // store the destination time series to the source DSS file //
            //----------------------------------------------------------//
            status = ztsStore(
                ifltab[SRC], // file table
                tss,         // time series struct
                0);          // storage flag (0=reg:replace all, irr:merge)
            assert(status == STATUS_OKAY);
            //---------------------------------------------------------------//
            // store the destination time series to the destination DSS file //
            //---------------------------------------------------------------//
            status = ztsStore(
                ifltab[DST], // file table
                tss,         // time series struct
                0);          // storage flag (0=reg:replace all, irr:merge)
            assert(status == STATUS_OKAY);
            //----------------------------------------------------------//
            // store the destination paired data to the source DSS file //
            //----------------------------------------------------------//
            status = zpdStore(ifltab[SRC], pds, 0);
            assert(status == STATUS_OKAY);
            //---------------------------------------------------------------//
            // store the destination paired data to the destination DSS file //
            //---------------------------------------------------------------//
            status = zpdStore(ifltab[DST], pds, 0);
            assert(status == STATUS_OKAY);
            zstructFree(tss);
            zstructFree(pds);
            //-------------------------------------------------------------------------------------------//
            // copy the time series to a location in the same DSS file that has a different native datum //
            //-------------------------------------------------------------------------------------------//
            printf("==> %s:%s (v%d)\n", filename[SRC], tsPathname[DST], dstDssVer);
            status = zcopyRecord(ifltab[SRC], ifltab[SRC], tsPathname[SRC], tsPathname[DST]);
            assert(status == STATUS_OKAY);
            //-------------------------------------------------------//
            // retrieve the copied time series and verify the values //
            //-------------------------------------------------------//
            tss = zstructTsNewTimes(
                tsPathname[DST],
                startDate,
                startTime,
                endDate,
                endTime);
            assert(tss != NULL);
            status = ztsRetrieve(
                ifltab[SRC], // file table
                tss,         // time series struct
                0,           // retrieve flag (0=adhere to time window and [for reg] create times array)
                0,           // retrieve doubles flag (0=as stored, 1=floats, 2=doubles)
                1);          // retrieve quality flag (0/1)
            assert(status == STATUS_OKAY);
            for (int i = 0; i < numberTsValues; ++i) {
                assert(fabs(tss->doubleValues[i] - tsValues[SRC][i]) < FLT_EPSILON);
            }
            zstructFree(tss);
            //-------------------------------------------------------------------------------------------//
            // copy the paired data to a location in the same DSS file that has a different native datum //
            //-------------------------------------------------------------------------------------------//
            printf("==> %s:%s (v%d)\n", filename[SRC], pdPathname[DST], dstDssVer);
            status = zcopyRecord(ifltab[SRC], ifltab[SRC], pdPathname[SRC], pdPathname[DST]);
            assert(status == STATUS_OKAY);
            //-------------------------------------------------------//
            // retrieve the copied paired data and verify the values //
            //-------------------------------------------------------//
            pds = zstructPdNew(pdPathname[DST]);
            assert(pds != NULL);
            status = zpdRetrieve(ifltab[SRC], pds, 0);
            assert(status == STATUS_OKAY);
            for (int i = 0; i < numberPdOrdinates; ++i) {
                assert(fabs(pds->doubleValues[i] - pdValues[SRC][i]) < FLT_EPSILON);
            }
            zstructFree(pds);
            //------------------------------------------------------------------------------------------//
            // copy the time series to a location in another DSS file that has a different native datum //
            //------------------------------------------------------------------------------------------//
            printf("==> %s:%s (v%d)\n", filename[DST], tsPathname[DST], dstDssVer);
            status = zcopyRecord(ifltab[SRC], ifltab[DST], tsPathname[SRC], tsPathname[DST]);
            assert(status == STATUS_OKAY);
            //-------------------------------------------------------//
            // retrieve the copied time series and verify the values //
            //-------------------------------------------------------//
            tss = zstructTsNewTimes(
                tsPathname[DST],
                startDate,
                startTime,
                endDate,
                endTime);
            assert(tss != NULL);
            status = ztsRetrieve(
                ifltab[DST], // file table
                tss,         // time series struct
                0,           // retrieve flag (0=adhere to time window and [for reg] create times array)
                0,           // retrieve doubles flag (0=as stored, 1=floats, 2=doubles)
                1);          // retrieve quality flag (0/1)
            assert(status == STATUS_OKAY);
            for (int i = 0; i < numberTsValues; ++i) {
                assert(fabs(tss->doubleValues[i] - tsValues[SRC][i]) < FLT_EPSILON);
            }
            zstructFree(tss);
            //------------------------------------------------------------------------------------------//
            // copy the paired data to a location in another DSS file that has a different native datum //
            //------------------------------------------------------------------------------------------//
            printf("==> %s:%s (v%d)\n", filename[DST], pdPathname[DST], dstDssVer);
            status = zcopyRecord(ifltab[SRC], ifltab[DST], pdPathname[SRC], pdPathname[DST]);
            assert(status == STATUS_OKAY);
            //-------------------------------------------------------//
            // retrieve the copied paired data and verify the values //
            //-------------------------------------------------------//
            pds = zstructPdNew(pdPathname[DST]);
            assert(pds != NULL);
            status = zpdRetrieve(ifltab[DST], pds, 0);
            assert(status == STATUS_OKAY);
            for (int i = 0; i < numberPdOrdinates; ++i) {
                assert(fabs(pds->doubleValues[i] - pdValues[SRC][i]) < FLT_EPSILON);
            }
            zstructFree(pds);
            zclose(ifltab[SRC]);
            zclose(ifltab[DST]);
        }
    }
    remove(filename[SRC]);
    remove(filename[DST]);
}

void testCopyRecordWithVdi() {
    zset("MLVL", "", 0);
    testCopyRecordWithVdi_NoVdiInDestination();
    testCopyRecordWithVdi_OtherNativeDatumInDestination();
    testCopyRecordWithVdi_SameNativeDatumInDestination();
}

