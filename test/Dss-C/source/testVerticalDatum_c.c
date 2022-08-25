#include <assert.h>
#include <string.h>
#include <heclib.h>
#include <verticalDatum.h>

void testDelimitedStringOps();
void testGzipAndEncodingOps();
void testUserHeaderOps();
void testVerticalDatumInfoSerialization();
void testZsetZquery();
void testStoreRetrieveTimeSeries();
void testV6TimeSeiresWithMultipleVerticalDatums();
void testStoreRetrievePairedData();

int test_vertical_datums_c() {
    testDelimitedStringOps();
    testGzipAndEncodingOps();
    testUserHeaderOps();
    testVerticalDatumInfoSerialization();
    testZsetZquery();
    testStoreRetrieveTimeSeries();
    testV6TimeSeiresWithMultipleVerticalDatums();
    testStoreRetrievePairedData();
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
    char  charVal[17];
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
    long long ifltab[250];
    int status;
    zStructTimeSeries* tss = NULL;
    verticalDatumInfo vdi;
    int OLD_API = 0;
    int NEW_API = 1;
    int REGULAR = 0;
    int IRREGULAR = 1;
    int NGVD29 = 0;
    int NAVD88 = 1;
    char* errmsg;
    char* filename = "v6_mult_vert_datum_ts.dss";
    char* pathnames[2] = { "//TESTTSLOC/ELEV//1DAY/MULTVERTICALDATUMS/", "//TESTTSLOC/ELEV//IR-YEAR/MULTVERTICALDATUMS/" };
    int numberValues = 365 * 2; // 2021-2022
    char* startDate[2] = { "01Jan2021", "01Jan2022" };
    char* endDate[2] = { "31Dec2021", "31Dec2022" };
    char* startTime = "01:00";
    int jan_01_2021 = 44195;
    int jan_01_2021_0000 = 63642240; // 01Jan2021 00:00
    int jan_01_2021_0100 = 63642300; // 01Jan2021 01:00
    int dec_21_2021_0000 = 64152000; // 21Dec2021 00:00
    int jan_01_2022_0001 = 64167841; // 01Jan2022 00:01
    double* dvalues = (double *)malloc(numberValues * sizeof(double));
    double* dv = NULL;
    int* times = (int*)malloc(numberValues * sizeof(int));
    int* t = NULL;
    char* unit = "ft";
    char unit2[8];
    char* dataType = "INST-VAL";
    char dataType2[8];
    char* compressed = NULL;
    char* headerBuf = NULL;
    int   len = 0;
    int messageLevel;
    char alpha[17];
    double expectedValue;
    int numvals;
    int basedate;
    int* flags;
    int readFlags = TRUE;
    flags = (int*)malloc(numberValues * sizeof(int));
    int userHeaderSize = 500;
    int* userHeader = (int*)malloc(userHeaderSize * sizeof(int));
    int userHeaderNumber;
    int readFlag = 0;
    int offsetMinutes;
    int compressionType;

    char* xml[] = {
        // applied to first year record
        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NGVD-29</native-datum>\n"
        "  <elevation>615.0</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NAVD-88</to-datum>\n"
        "    <value>0.5</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",
        // applied to 2nd year record
        "<vertical-datum-info unit=\"ft\">\n"
        "  <native-datum>NAVD-88</native-datum>\n"
        "  <elevation>615.5</elevation>\n"
        "  <offset estimate=\"true\">\n"
        "    <to-datum>NGVD-29</to-datum>\n"
        "    <value>-0.5</value>\n"
        "  </offset>\n"
        "</vertical-datum-info>\n",
    };

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
                for (int i = 0; i < numberValues; ++i) {
                    if (i == 0 || i == 365) {
                        //---------------------------------------//
                        // get new VDI at the start of each year //
                        //---------------------------------------//
                        errmsg = stringToVerticalDatumInfo(&vdi, xml[i / 365]);
                        assert(errmsg == NULL);
                    }
                    dvalues[i] = (double)(i % 365 + 1) - vdi.offsetToNgvd29;
                    times[i] = i == 0 ? jan_01_2021_0100 : times[i - 1] + 1440;
                }
                for (int year = 2021; year <= 2022; ++year) {
                    //-------------------------------------//
                    // create a TSS for the storing record //
                    //-------------------------------------//
                    int index = year == 2021 ? 0 : numberValues / 2;
                    // don't care about API for storing, only retrieving
                    if (tsType == REGULAR) {
                        tss = zstructTsNewRegDoubles(
                            pathnames[REGULAR],         // dataset name
                            &dvalues[index],            // values
                            numberValues / 2,           // number of values
                            startDate[year - 2021],     // start date
                            startTime,                  // start time
                            unit,                       // data unit
                            dataType);                  // data type
                    }
                    else {
                        tss = zstructTsNewIrregDoubles(
                            pathnames[IRREGULAR],       // dataset name
                            &dvalues[index],            // values
                            numberValues / 2,           // number of values
                            &times[index],              // times
                            60,                         // time granularity in seconds
                            NULL,                       // base date (if other than 01Jan1900)
                            unit,                       // data unit
                            dataType);                  // data type
                    }
                    //------------------------------------------//
                    // assign vertical datum info to the record //
                    //------------------------------------------//
                    errmsg = gzipAndEncode(&compressed, xml[year - 2021]);
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
                    free(headerBuf);
                    headerBuf = NULL;
                    tss->allocated[zSTRUCT_userHeader] = TRUE;
                    //----------------------------------------------------------------//
                    // set the default vertical datum to the native datum of the data //
                    //----------------------------------------------------------------//
                    verticalDatumInfo* pVdi = extractVerticalDatumInfoFromUserHeader(tss->userHeader, tss->userHeaderNumber);
                    zset("VDTM", pVdi->nativeDatum, 0);
                    printf("\t\tStoring time series for %d with native vertical datum of %s\n", year, pVdi->nativeDatum);
                    free(pVdi);
                    //-----------------------//
                    // store the time series //
                    //-----------------------//
                    status = ztsStore(
                        ifltab,        // file table
                        tss,           // time series struct
                        0);            // storage flag (0=reg:replace all, irr:merge)
                    assert(status == 0);
                    zstructFree(tss);
                }
                //-----------------------------------------------//
                // set the default vertical datum for retrieving //
                //-----------------------------------------------//
                zset("VDTM", "", workingVerticalDatum);
                //-----------------------------------------------------//
                // read the consecutive records just stored and verify //
                //-----------------------------------------------------//
                for (int year = 2021; year <= 2022; ++year) {
                    printf("\t\tRetrieving time series for year %d ", year);
                    printf("with %s\n", api == NEW_API ? "ztsRetrieve" : tsType == REGULAR ? "zrrtsxd" : "zritsxd");
                    if (api == OLD_API) {
                        numvals = 365;
                        memset(dvalues, 0, numberValues * sizeof(double));
                        if (tsType == REGULAR) {
                            zrrtsxd_(
                                ifltab,                          // <-> file table
                                pathnames[REGULAR],              //  -> dataset name
                                startDate[year - 2021],          //  -> date of start of time window
                                "0001",                          //  -> time of start of time window
                                &numvals,                        // <-> max number of values to retrieve / number of values retrieved
                                dvalues,                         // <-  values array
                                flags,                           // <-  quality flags array
                                &readFlags,                      //  -> whether to retrieve quality flags if they exist (0/1)
                                &readFlags,                      // <-  whether quality flags were retrieved (0/1)
                                unit2,                           // <-  data unit
                                dataType2,                       // <-  data type
                                userHeader,                      // <-  user header array
                                &userHeaderSize,                 //  -> max number of user header elements to retrieve
                                &userHeaderNumber,               // <-  number of user header elements retrieved
                                &offsetMinutes,                  // <-  offset into interval of the time of each value
                                &compressionType,                // <-  compression method used if values were compressed in file
                                &status,                         // <-  status (0=success)
                                strlen(pathnames[REGULAR]),      //  -> size of dataset name string
                                strlen(startDate[year - 2021]),  //  -> size of time window start date string
                                strlen("0001"),                  //  -> size of time window start time string
                                sizeof(unit2),                   //  -> size of unit string
                                sizeof(dataType2));              //  -> size of data type string
                            //-----------------------------------------------------------------//
                            // create the times array from:                                    //
                            //  - the beginning time of the interval containing the start time //
                            //  - the index into the array                                     //
                            //  - the interval size in minutes                                 //
                            //  - the offset                                                   //
                            //-----------------------------------------------------------------//
                            for (int i = 0; i < numvals; ++i) {
                                times[i] = jan_01_2021_0000 + offsetMinutes + i * 1440;
                            }
                        }
                        else {
                            int startJul  = dateToJulian(startDate[year - 2021]);
                            int startTime = 1;
                            int endJul    = dateToJulian(endDate[year - 2021]);
                            int endTime   = 1440;
                            zritsxd_(
                                ifltab,                        // <-> file table
                                pathnames[IRREGULAR],          //  -> dataset name
                                &startJul,                     //  -> days since 31Dec1899 of start of time window
                                &startTime,                    //  -> minutes into day of start of time window
                                &endJul,                       //  -> days since 31Dec1899 of end of time window
                                &endTime,                      //  -> minutes into day of end of time window
                                times,                         // <-  times array as minutes offset from base date
                                dvalues,                       // <-  values array
                                &numberValues,                 //  -> max number of values to return
                                &numvals,                      // <-  number of values returned
                                &basedate,                     // <-  days since 31Dec1899 of time of first value
                                flags,                         // <-  quality flags array
                                &readFlags,                    //  -> whether to retrieve quality flags if they exist (0/1)
                                &readFlags,                    // <-  whether quality flags were retrieved (0/1)
                                unit2,                         // <-  data unit
                                dataType2,                     // <-  data type
                                userHeader,                    // <-  user header array
                                &userHeaderSize,               //  -> max number of user header elements to retrieve
                                &userHeaderNumber,             // <-  number of user header elements retrieved
                                &readFlag,                     //  -> read method (0=time window, 1=tw+prev, 2=tw+next 3=tw+prev+next)
                                &status,                       // <-  status (0=success)
                                strlen(pathnames[IRREGULAR]),  //  -> size of dataset name string
                                sizeof(unit2),                 //  -> size of unit string
                                sizeof(dataType2));            //  -> size of data type string
                            //-----------------------------------------------------------------//
                            // add the base date (in minutes) to each value in the times array //
                            //-----------------------------------------------------------------//
                            for (int i = 0; i < numvals; ++i) {
                                times[i] += basedate * 1440;
                            }
                        }
                        assert(status == STATUS_OKAY);
                        headerBuf = userHeaderToString(userHeader, userHeaderNumber);
                        assert(headerBuf != NULL);
                        assert(strlen(headerBuf) > 0);
                        dv = dvalues;
                        t = times;
                    }
                    else {
                        //----------------------------------------//
                        // create a TSS for retrieving the record //
                        //----------------------------------------//
                        tss = zstructTsNewTimes(
                            pathnames[REGULAR],
                            startDate[year - 2021],
                            "0001",
                            endDate[year - 2021],
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
                        numvals = tss->numberValues;
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
                    printf("\t\t\tValues were retrieved with native datum of %s and current datum of %s\n", vdi.nativeDatum, currentVerticalDatum);
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
                    for (int i = 0; i < numvals; i+= 30) {
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
                // *****DSS*** zrrtsi6:  WARNING  - ELEVATION VALUES ARE IN MULTIPLE NATIVE VERTICAL DATUMS
                // USE WITH CAUTION!
                //
                printf("\t\tRetrieving time series that crosses record boundaries ");
                printf("with %s\n", api == NEW_API ? "ztsRetrieve" : tsType == REGULAR ? "zrrtsxd" : "zritsxd");
                if (api == OLD_API) {
                    numvals = 21;
                    memset(dvalues, 0, numberValues * sizeof(double));
                    if (tsType == REGULAR) {
                        zrrtsxd_(
                            ifltab,                      // <-> file table
                            pathnames[REGULAR],          //  -> dataset name
                            "21Dec2021",                 //  -> date of start of time window
                            "0001",                      //  -> time of start of time window
                            &numvals,                    // <-> max number of values to retrieve / number of values retrieved
                            dvalues,                     // <-  values array
                            flags,                       // <-  quality flags array
                            &readFlags,                  //  -> whether to retrieve quality flags if they exist (0/1)
                            &readFlags,                  // <-  whether quality flags were retrieved (0/1)
                            unit2,                       // <-  data unit
                            dataType2,                   // <-  data type
                            userHeader,                  // <-  user header array
                            &userHeaderSize,             //  -> max number of user header elements to retrieve
                            &userHeaderNumber,           // <-  number of user header elements retrieved
                            &offsetMinutes,              // <-  offset into interval of the time of each value
                            &compressionType,            // <-  compression method used if values were compressed in file
                            &status,                     // <-  status (0=success)
                            strlen(pathnames[REGULAR]),  //  -> size of dataset name string
                            strlen("21Dec2021"),         //  -> size of time window start date string
                            strlen("0001"),              //  -> size of time window start time string
                            sizeof(unit2),               //  -> size of unit string
                            sizeof(dataType2));          //  -> size of data type string
                        assert(status == STATUS_OKAY);
                        //-----------------------------------------------------------------//
                        // create the times array from:                                    //
                        //  - the beginning time of the interval containing the start time //
                        //  - the index into the array                                     //
                        //  - the interval size in minutes                                 //
                        //  - the offset                                                   //
                        //-----------------------------------------------------------------//
                        for (int i = 0; i < numvals; ++i) {
                            times[i] = dec_21_2021_0000 + offsetMinutes + i * 1440;
                        }
                    }
                    else {
                        int startJul = dateToJulian("21Dec2021");
                        int startTime = 1;
                        int endJul = dateToJulian("10Jan2022");
                        int endTime = 1440;
                        zritsxd_(
                            ifltab,                        // <-> file table
                            pathnames[IRREGULAR],          //  -> dataset name
                            &startJul,                     //  -> days since 31Dec1899 of start of time window
                            &startTime,                    //  -> minutes into day of start of time window
                            &endJul,                       //  -> days since 31Dec1899 of end of time window
                            &endTime,                      //  -> minutes into day of end of time window
                            times,                         // <-  times array as minutes offset from base date
                            dvalues,                       // <-  values array
                            &numberValues,                 //  -> max number of values to return
                            &numvals,                      // <-  number of values returned
                            &basedate,                     // <-  days since 31Dec1899 of time of first value
                            flags,                         // <-  quality flags
                            &readFlags,                    //  -> whether to retrieve quality flags if they exist (0/1)
                            &readFlags,                    // <-  whether quality flags were retrieved (0/1)
                            unit2,                         // <-  data unit
                            dataType2,                     // <-  data type
                            userHeader,                    // <-  user header array
                            &userHeaderSize,               //  -> max number of user header elements to retrieve
                            &userHeaderNumber,             // <-  number of user header elements retrieved
                            &readFlag,                     //  -> read method (0=time window, 1=tw+prev, 2=tw+next 3=tw+prev+next)
                            &status,                       // <-  status (0=success)
                            strlen(pathnames[IRREGULAR]),  //  -> size of dataset name string
                            sizeof(unit2),                 //  -> size of unit string
                            sizeof(dataType2));            //  -> size of data type string
                        assert(status == STATUS_OKAY);
                        //-----------------------------------------------------------------//
                        // add the base date (in minutes) to each value in the times array //
                        //-----------------------------------------------------------------//
                        for (int i = 0; i < numvals; ++i) {
                            times[i] += basedate * 1440;
                        }
                    }
                    headerBuf = userHeaderToString(userHeader, userHeaderNumber);
                    assert(headerBuf != NULL);
                    assert(strlen(headerBuf) > 0);
                    dv = dvalues;
                    t = times;
                }
                else {
                    tss = zstructTsNewTimes(
                        pathnames[REGULAR],  // dataset name
                        "21Dec2021",         // start date
                        "0001",              // start time
                        "10Jan2022",         // end date 
                        "2400");             // end time
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
                    numvals = tss->numberValues;
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
                currentVerticalDatum = currentVerticalDatum ? currentVerticalDatum : vdi.nativeDatum;
                printf("\t\t\tValues were retrieved with native datum of %s and current datum of %s\n", vdi.nativeDatum, currentVerticalDatum);
                for (int i = 0; i < numvals; ++i) {
                    switch (workingVerticalDatum) {
                    case IVERTICAL_DATUM_UNSET:
                        if (t[i] < jan_01_2022_0001) {
                            expectedValue = ((t[i] / 1440 - 1) - jan_01_2021) % 365 + 1;
                        }
                        else {
                            expectedValue = ((t[i] / 1440 - 1) - jan_01_2021) % 365 + 1.5;
                        }
                        break;
                    case IVERTICAL_DATUM_NGVD29:
                        expectedValue = ((t[i] / 1440 - 1) - jan_01_2021) % 365 + 1;
                        break;
                    case IVERTICAL_DATUM_NAVD88:
                        expectedValue = ((t[i] / 1440 - 1) - jan_01_2021) % 365 + 1.5;
                        break;
                    }
                    assert(dv[i] == expectedValue);
                }
                if (api == NEW_API) {
                    zstructFree(tss);
                }
            }
        }
    }
    zclose(ifltab);
    zset("MLVL", "", messageLevel);
}
void testStoreRetrieveTimeSeries() {
// test storing and retriving time series data
    long long ifltab[250];
    int status;
    zStructTimeSeries *tss = NULL;
    verticalDatumInfo vdi;
    char *errmsg;
    char *filename[]      = {"v6_c.dss", "v7_c.dss"};
    char *pathnames[2][2] = {{"//TestTsLoc/Elev//1Hour/Doubles/",    "//TestTsLoc/Elev//1Hour/Floats/"},
                             {"//TestTsLoc/Elev//IR-MONTH/Doubles/", "//TestTsLoc/Elev//IR-MONTH/Floats/"}};
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
        "</vertical-datum-info>\n"
    };
    char *verticalDatums[] = {
        CVERTICAL_DATUM_NAVD88,
        CVERTICAL_DATUM_NGVD29,
        "Pensacola"
    };
    int unitCount = sizeof(unit) / sizeof(unit[0]);
    int xml_count = sizeof(xml) / sizeof(xml[0]);
    int verticalDatumCount = sizeof(verticalDatums) / sizeof(verticalDatums[0]);
    int count = 0;
    int expectSuccess = TRUE;

    //
    // loop variables
    //
    // i = DSS file version
    //     0 = DSS 6
    //     1 = DSS 7
    //
    // j = xml blocks
    //     0 = NGVD-29 native
    //     1 = OTHER native with local datum named "Pensacola"
    //
    // k = vertical datum
    //     0 = NAVD-88
    //     1 = NGVD-29
    //     2 = OTHER (Pensacola)
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
    zset("MLVL", "", 1);
    for (int i = 0; i < 2; ++i) {
        remove(filename[i]);
        for (int j = 0; j < xml_count; ++j) {
            for (int k = 0; k < verticalDatumCount; ++k) {
                int k2 = (k+1) % verticalDatumCount;
                int k3 = (k+2) % verticalDatumCount;
                for (int l = 0; l < unitCount; ++ l) {
                    for (int m = 0; m < 3; ++m) {
                        for (int n = 0; n < 2; ++n) {
                            for (int o = 0; o < 2; ++o) {
                                for (int p = 0; p < 2; ++p) {
                                    ++count;
                                    len = 0;
                                    headerBuf = NULL;
                                    //------------------------//
                                    // create the time series //
                                    //------------------------//
                                    // printf("%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n", i,j,k,l,m,n,o,p);
                                    if (i == 0) {
                                        status = zopen6(ifltab, filename[i]);
                                    }
                                    else {
                                        status = zopen7(ifltab, filename[i]);
                                    }
                                    assert(status == STATUS_OKAY);
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
                                                60,
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
                                                60,
                                                NULL,
                                                unit[l],
                                                type);
                                        }
                                    }
                                    assert(tss != NULL);
                                    //--------------------------------//
                                    // set the default vertical datum //
                                    //--------------------------------//
                                    int K = k;
                                    zset("VDTM", verticalDatums[K], 0);
                                    if (p == 0) {
                                        //------------------------------------------------//
                                        // add the vertical datum info to the user header //
                                        //------------------------------------------------//
                                        errmsg = gzipAndEncode(&compressed, xml[j]);
                                        assert(errmsg == NULL);
                                        len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 2;
                                        headerBuf = (char *)malloc(len+1);
                                        memset(headerBuf, 0, len+1);
                                        status = insertIntoDelimitedString(
                                            &headerBuf,
                                            len+1,
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
                                    if (m > 0) {
                                        //----------------------------------------------------------//
                                        // override the default vertical datum with the user header //
                                        //----------------------------------------------------------//
                                        K = k2;
                                        status = insertIntoDelimitedString(
                                            headerBuf ? &headerBuf : NULL,
                                            len,
                                            VERTICAL_DATUM_USER_HEADER_PARAM,
                                            verticalDatums[K],
                                            ":",
                                            FALSE,
                                            ';');
                                        if (status != 0) {
                                            if (headerBuf) {
                                                int oldlen = len;
                                                len += VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(verticalDatums[K]) + 3;
                                                headerBuf = (char *)realloc(headerBuf, len);
                                                memset(headerBuf+oldlen, 0, len-oldlen);
                                            }
                                            else {
                                                len = VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(verticalDatums[K]) + 3;
                                                headerBuf = (char *)malloc(len);
                                                headerBuf[0] = '\0';
                                            }
                                            assert(headerBuf != NULL);
                                            status = insertIntoDelimitedString(
                                                &headerBuf,
                                                len,
                                                VERTICAL_DATUM_USER_HEADER_PARAM,
                                                verticalDatums[K],
                                                ":",
                                                FALSE,
                                                ';');
                                            assert(status == 0);
                                        }
                                        if (tss->userHeader) free(tss->userHeader);
                                        tss->userHeader = stringToUserHeader(headerBuf, &tss->userHeaderNumber);
                                        tss->allocated[zSTRUCT_userHeader] = TRUE;
                                    }
                                    if (m > 1) {
                                        //--------------------------------------------------------//
                                        // override default and user header datums with unit spec //
                                        //--------------------------------------------------------//
                                        K = k3;
                                        sprintf(unitSpec, "U=%s|V=%s", unit[l], verticalDatums[K]);
                                        free(tss->units);
                                        tss->units = mallocAndCopy(unitSpec);
                                    }
                                    //-------------------------------------------------//
                                    // figure out whether expect ztsStore to succeeded //
                                    //-------------------------------------------------//
                                    stringToVerticalDatumInfo(&vdi, xml[j]);
                                    if (i == 1 && j == 1 && k+l+n+m+o+p == 0) {
                                        //-------------------------------------------------------------------------------//
                                        // change of vertical datum information in DSS 7, need to update location record //
                                        //-------------------------------------------------------------------------------//
                                        expectSuccess = FALSE;
                                    }
                                    else if (i == 0 && p == 1 && headerBuf != NULL) {
                                        //---------------------------------------------------------------------------------//
                                        // current vertical datum in header, but no vertical datum info in header in DSS 6 //
                                        //---------------------------------------------------------------------------------//
                                        expectSuccess = FALSE;
                                    }
                                    else if (!strcmp(vdi.nativeDatum, verticalDatums[K])) {
                                        //-------------------------------------//
                                        // same datum, no conversion necessary //
                                        //-------------------------------------//
                                        expectSuccess = TRUE;
                                    }
                                    else if (strcmp(verticalDatums[K], CVERTICAL_DATUM_NAVD88) && strcmp(verticalDatums[K], CVERTICAL_DATUM_NGVD29)) {
                                        //--------------------------//
                                        // requested datum is local //
                                        //--------------------------//
                                        if (!strcmp(vdi.nativeDatum, CVERTICAL_DATUM_NAVD88) || !strcmp(vdi.nativeDatum, CVERTICAL_DATUM_NGVD29)) {
                                            //---------------------------//
                                            // native datum is non-local //
                                            //---------------------------//
                                            expectSuccess = FALSE;
                                        }
                                        else {
                                            //-----------------------//
                                            // native datum is local //
                                            //-----------------------//
                                            expectSuccess = TRUE;
                                        }
                                    }
                                    else if (!strcmp(verticalDatums[K], CVERTICAL_DATUM_NAVD88) && vdi.offsetToNavd88 != UNDEFINED_VERTICAL_DATUM_VALUE) {
                                        //-------------------------------------------------------------//
                                        // specified datum is NAVD-88 and we have an offset to NAVD-88 //
                                        //-------------------------------------------------------------//
                                        if (unitIsFeet(unit[l]) || unitIsMeters(unit[l])) {
                                            expectSuccess = TRUE;
                                        }
                                        else {
                                            expectSuccess = FALSE;
                                        }
                                    }
                                    else if (!strcmp(verticalDatums[K], CVERTICAL_DATUM_NGVD29) && vdi.offsetToNgvd29 != UNDEFINED_VERTICAL_DATUM_VALUE) {
                                        //-------------------------------------------------------------//
                                        // specified datum is NGVD-29 and we have an offset to NGVD-29 //
                                        //-------------------------------------------------------------//
                                        if (unitIsFeet(unit[l]) || unitIsMeters(unit[l])) {
                                            expectSuccess = TRUE;
                                        }
                                        else {
                                            expectSuccess = FALSE;
                                        }
                                    }
                                    else {
                                        //-----------------//
                                        // all other cases //
                                        //-----------------//
                                        expectSuccess = TRUE;
                                    }
                                    //-------------------------------------------------------//
                                    // store the time series in the specified vertical datum //
                                    //-------------------------------------------------------//
                                    printf("Time series test %3d: expecting %s\n", count, expectSuccess ? "SUCESS" : "ERROR");
                                    status = ztsStore(ifltab, tss, 0);
                                    assert((status == STATUS_OKAY) == expectSuccess);
                                    if (i == 1 && j == 1 && k+l+n+m+o+p == 0) {
                                        //-------------------------------------------------------------------------------//
                                        // change of vertical datum information in DSS 7, need to update location record //
                                        //-------------------------------------------------------------------------------//
                                        zset("VDOW", "", TRUE);
                                        printf("Test %d: expecting SUCESS\n", ++count);
                                        status = ztsStore(ifltab, tss, 0);
                                        assert(status == STATUS_OKAY);
                                        zset("VDOW", "", FALSE);
                                    }
                                    zclose(ifltab);
                                    zstructFree(tss);
                                    if (status == STATUS_OKAY) {
                                        //--------------------------------------------------------//
                                        // retrieve the time series in the default vertical datum //
                                        //-------------------------------------------------------//
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
                                        zset("VDTM", verticalDatums[K], 0);
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
    printf("\n\n%3d Time series tests passed\n\n\n", count);
}
void testStoreRetrievePairedData() {
    // test storing and retrieving paired data
    long long ifltab[250];
    zStructPairedData *pds;
    verticalDatumInfo vdi;
    int    status;
    char  *errmsg;
    char  *filename[2]      = {"v6_c.dss", "v7_c.dss"};
    char  *pathnames[2][2]  = {{"//TestPdLoc/Stage-Elev///Doubles/", "//TestPdLoc/Stage-Elev///Floats/"},
                               {"//TestPdLoc/Elev-Stage///Doubles/", "//TestPdLoc/Elev-Stage///Floats/"}};
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
        "</vertical-datum-info>\n"
    };
    char *verticalDatums[] = {
        CVERTICAL_DATUM_NAVD88,
        CVERTICAL_DATUM_NGVD29,
        "Pensacola"
    };
    int unitCount = sizeof(unit) / sizeof(unit[0]);
    int xml_count = sizeof(xml) / sizeof(xml[0]);
    int verticalDatumCount = sizeof(verticalDatums) / sizeof(verticalDatums[0]);
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
    //     1 = OTHER native with local datum named "Pensacola"
    //
    // k = vertical datum
    //     0 = NAVD-88
    //     1 = NGVD-29
    //     2 = OTHER (Pensacola)
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
    zset("MLVL", "", 1);
    for (int i = 0; i < 2; ++i) {
        remove(filename[i]);
        for (int j = 0; j < xml_count; ++j) {
            for (int k = 0; k < verticalDatumCount; ++k) {
                int k2 = (k+1) % verticalDatumCount;
                int k3 = (k+2) % verticalDatumCount;
                for (int l = 0; l < unitCount; ++ l) {
                    for (int m = 0; m < 3; ++m) {
                        for (int n = 0; n < 2; ++n) {
                            for (int o = 0; o < 2; ++o) {
                                for (int p = 0; p < 2; ++p) {
                                    ++count;
                                    len = 0;
                                    headerBuf = NULL;
                                    //------------------------//
                                    // create the paired data //
                                    //------------------------//
                                    // printf("%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n", i,j,k,l,m,n,o,p);
                                    if (i == 0) {
                                        status = zopen6(ifltab, filename[i]);
                                    }
                                    else {
                                        status = zopen7(ifltab, filename[i]);
                                    }
                                    assert(status == STATUS_OKAY);
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
                                    //--------------------------------//
                                    // set the default vertical datum //
                                    //--------------------------------//
                                    int K = k;
                                    zset("VDTM", verticalDatums[K], 0);
                                    if (p == 0) {
                                        //------------------------------------------------//
                                        // add the vertical datum info to the user header //
                                        //------------------------------------------------//
                                        errmsg = gzipAndEncode(&compressed, xml[j]);
                                        assert(errmsg == NULL);
                                        len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 2;
                                        headerBuf = (char *)malloc(len+1);
                                        memset(headerBuf, 0, len+1);
                                        status = insertIntoDelimitedString(
                                            &headerBuf,
                                            len+1,
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
                                    if (m > 0) {
                                        //----------------------------------------------------------//
                                        // override the default vertical datum with the user header //
                                        //----------------------------------------------------------//
                                        K = k2;
                                        status = insertIntoDelimitedString(
                                            headerBuf ? &headerBuf : NULL,
                                            len,
                                            VERTICAL_DATUM_USER_HEADER_PARAM,
                                            verticalDatums[K],
                                            ":",
                                            FALSE,
                                            ';');
                                        if (status != 0) {
                                            if (headerBuf) {
                                                int oldlen = len;
                                                len += VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(verticalDatums[K]) + 3;
                                                headerBuf = (char *)realloc(headerBuf, len);
                                                memset(headerBuf+oldlen, 0, len-oldlen);
                                            }
                                            else {
                                                len = VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(verticalDatums[K]) + 3;
                                                headerBuf = (char *)malloc(len);
                                                headerBuf[0] = '\0';
                                            }
                                            assert(headerBuf != NULL);
                                            status = insertIntoDelimitedString(
                                                &headerBuf,
                                                len,
                                                VERTICAL_DATUM_USER_HEADER_PARAM,
                                                verticalDatums[K],
                                                ":",
                                                FALSE,
                                                ';');
                                            assert(status == 0);
                                        }
                                        if (pds->userHeader) free(pds->userHeader);
                                        pds->userHeader = stringToUserHeader(headerBuf, &pds->userHeaderNumber);
                                        pds->allocated[zSTRUCT_userHeader] = 1;
                                        if (m > 1) {
                                            //--------------------------------------------------------//
                                            // override default and user header datums with unit spec //
                                            //--------------------------------------------------------//
                                            K = k3;
                                            sprintf(unitSpec, "U=%s|V=%s", unit[l], verticalDatums[K]);
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
                                    //------------------------------------------------//
                                    // figure out whether the zpdStore should succeed //
                                    //------------------------------------------------//
                                    stringToVerticalDatumInfo(&vdi, xml[j]);
                                    if (i == 1 && j == 1 && k+l+n+m+o+p == 0) {
                                        //-------------------------------------------------------------------------------//
                                        // change of vertical datum information in DSS 7, need to update location record //
                                        //-------------------------------------------------------------------------------//
                                        expectSuccess = FALSE;
                                    }
                                    else if (i == 0 && p == 1 && headerBuf != NULL) {
                                        //---------------------------------------------------------------------------------//
                                        // current vertical datum in header, but no vertical datum info in header in DSS 6 //
                                        //---------------------------------------------------------------------------------//
                                        expectSuccess = FALSE;
                                    }
                                    else if (!strcmp(vdi.nativeDatum, verticalDatums[K])) {
                                        //-------------------------------------//
                                        // same datum, no conversion necessary //
                                        //-------------------------------------//
                                        expectSuccess = TRUE;
                                    }
                                    else if (strcmp(verticalDatums[K], CVERTICAL_DATUM_NAVD88) && strcmp(verticalDatums[K], CVERTICAL_DATUM_NGVD29)) {
                                        //--------------------------//
                                        // requested datum is local //
                                        //--------------------------//
                                        if (!strcmp(vdi.nativeDatum, CVERTICAL_DATUM_NAVD88) || !strcmp(vdi.nativeDatum, CVERTICAL_DATUM_NGVD29)) {
                                            //---------------------------//
                                            // native datum is non-local //
                                            //---------------------------//
                                            expectSuccess = FALSE;
                                        }
                                        else {
                                            //-----------------------//
                                            // native datum is local //
                                            //-----------------------//
                                            expectSuccess = TRUE;
                                        }
                                    }
                                    else if (!strcmp(verticalDatums[K], CVERTICAL_DATUM_NAVD88) && vdi.offsetToNavd88 != UNDEFINED_VERTICAL_DATUM_VALUE) {
                                        //-------------------------------------------------------------//
                                        // specified datum is NAVD-88 and we have an offset to NAVD-88 //
                                        //-------------------------------------------------------------//
                                        if (unitIsFeet(unit[l]) || unitIsMeters(unit[l])) {
                                            expectSuccess = TRUE;
                                        }
                                        else {
                                            expectSuccess = FALSE;
                                        }
                                    }
                                    else if (!strcmp(verticalDatums[K], CVERTICAL_DATUM_NGVD29) && vdi.offsetToNgvd29 != UNDEFINED_VERTICAL_DATUM_VALUE) {
                                        //-------------------------------------------------------------//
                                        // specified datum is NGVD-29 and we have an offset to NGVD-29 //
                                        //-------------------------------------------------------------//
                                        if (unitIsFeet(unit[l]) || unitIsMeters(unit[l])) {
                                            expectSuccess = TRUE;
                                        }
                                        else {
                                            expectSuccess = FALSE;
                                        }
                                    }
                                    else {
                                        //-----------------//
                                        // all other cases //
                                        //-----------------//
                                        expectSuccess = FALSE;
                                    }
                                    //--------------------------------------------------------//
                                    // store the paired data in the overridden vertical datum //
                                    //--------------------------------------------------------//
                                    printf("Paired data test %3d: expecting %s\n", count, expectSuccess ? "SUCESS" : "ERROR");
                                    status = zpdStore(ifltab, pds, 0);
                                    assert((status == STATUS_OKAY) == expectSuccess);
                                    if (i == 1 && j == 1 && k+l+n+m+o+p == 0) {
                                        //-------------------------------------------------------------------------------//
                                        // change of vertical datum information in DSS 7, need to update location record //
                                        //-------------------------------------------------------------------------------//
                                        zset("VDOW", "", TRUE);
                                        printf("Test %d: expecting SUCESS\n", ++count);
                                        status = zpdStore(ifltab, pds, 0);
                                        assert(status == STATUS_OKAY);
                                        zset("VDOW", "", FALSE);
                                    }
                                    zclose(ifltab);
                                    zstructFree(pds);
                                    if (status == STATUS_OKAY) {
                                        //------------------------------------------------------------//
                                        // set the default vertical datum to the datum we stored with //
                                        //------------------------------------------------------------//
                                        zset("VDTM", verticalDatums[K], 0);
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
    printf("\n\n%3d Paired data tests passed\n\n\n", count);
}
