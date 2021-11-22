#include <assert.h>
#include <string.h>
#include <heclib.h>
#include <verticalDatum.h>

void main (int argc, char *argv[]) {
    // test extract_from_delimited_string() and insertIntoDelimitedString()
    {
        int text_size = 256;
        char *text = (char *)malloc(text_size);
        char *cp;
        char *text_value = "theFirstParameter:theFirstValue;theSecondParameter:theSecondValue";
        sprintf(text, text_value);
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
        assert(!strcmp(text, "theSecondParameter:theSecondValue"));
        sprintf(text, text_value);
        free(cp);
        cp = extractFromDelimitedString(&text, "theSecondParameter", ":", TRUE, TRUE, ';');
        assert(!strcmp(cp, "theSecondValue"));
        assert(!strcmp(text, "theFirstParameter:theFirstValue"));
        assert(insertIntoDelimitedString(&text, text_size, "anotherParameter", "anotherValue", ":", FALSE, ';') ==  0);
        assert(!strcmp(text, "theFirstParameter:theFirstValue;anotherParameter:anotherValue"));
        free(extractFromDelimitedString(&text, "theFirstParameter", ":", TRUE, TRUE, ';'));
        free(extractFromDelimitedString(&text, "anotherParameter", ":", TRUE, TRUE, ';'));
        assert(strlen(text) == 0);
        assert(insertIntoDelimitedString(&text, text_size, "anotherParameter", "anotherValue", ":", FALSE, ';') == 0);
        assert(strcmp(text, "anotherParameter:anotherValue") == 0);
        free(text);
    }
    // test gzipAndEncode() and decodeAndGunzip()
    {
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
        if (errmsg != NULL) fprintf(stderr, "%s\n", errmsg);
        assert(errmsg == NULL);
        errmsg = decodeAndGunzip(&expanded, compressed);
        if (errmsg != NULL) fprintf(stderr, "%s\n", errmsg);
        assert(errmsg == NULL);
        assert(!strcmp(expanded, input_text));
    }
    // test stringToUserHeader() and userHeaderToString()
    {
        int  *userHeader;
        int   userHeaderSize;
        char *userHeaderStringIn;
        char *userHeaderStringOut;
        userHeaderStringIn = "This is a test string for the user header";
        userHeader = stringToUserHeader(userHeaderStringIn, &userHeaderSize);
        assert(userHeaderSize == (strlen(userHeaderStringIn)-1)/4+1);
        userHeaderStringOut = userHeaderToString(userHeader, userHeaderSize);
        assert(!strcmp(userHeaderStringIn, userHeaderStringOut));
        free(userHeader);
        free(userHeaderStringOut);
    }
    // test stringToVerticalDatumInfo() and verticalDatumInfoToString()
    {
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
        if (errmsg != NULL) fprintf(stderr, "%s\n", errmsg);
        assert(errmsg == NULL);
        assert(!strcmp(vdi.nativeDatum, CVERTICAL_DATUM_NGVD29));
        assert(vdi.elevation == 615.2);
        assert(vdi.offsetToNavd88 == 0.3855);
        assert(vdi.offsetToNavd88IsEstimate == 1);
        assert(vdi.offsetToNgvd29 == 0.f);
        assert(vdi.offsetToNgvd29IsEstimate == 0);

        errmsg = stringToVerticalDatumInfo(&vdi, xml2);
        if (errmsg != NULL) fprintf(stderr, "%s\n", errmsg);
        assert(errmsg == NULL);
        assert(!strcmp(vdi.nativeDatum, "Pensacola"));
        assert(vdi.elevation == 757.);
        assert(vdi.offsetToNavd88 == 1.457);
        assert(vdi.offsetToNavd88IsEstimate == 1);
        assert(vdi.offsetToNgvd29 == 1.07);
        assert(vdi.offsetToNgvd29IsEstimate == 0);

    }
    // test zset() and zquery() for vertical datums
    {
        int   intVal;
        char  charVal[17];

        zquery("VERS", charVal, sizeof(charVal), &intVal);
        assert(intVal == 7);
        
        zquery("VDTM", charVal, sizeof(charVal), &intVal);
        assert(intVal == IVERTICAL_DATUM_UNSET);
        assert(!strcmp(charVal, CVERTICAL_DATUM_UNSET));

        zquery("VDOW", "", 0, &intVal);
        assert(intVal == FALSE);

        zset("VDTM", "", 1);
        zquery("VDTM", charVal, sizeof(charVal), &intVal);
        assert(intVal == IVERTICAL_DATUM_NAVD88);
        assert(!strcmp(charVal, CVERTICAL_DATUM_NAVD88));

        zset("VDTM", "", 2);
        zquery("VDTM", charVal, sizeof(charVal), &intVal);
        assert(intVal == IVERTICAL_DATUM_NGVD29);
        assert(!strcmp(charVal, CVERTICAL_DATUM_NGVD29));

        intVal = zset("VDTM", "", 4);
        // assert(intVal == STATUS_NOT_OKAY); bug, always returns STATUS_OK - Jira issue DSS-122
        zquery("VDTM", charVal, sizeof(charVal), &intVal);
        assert(intVal == IVERTICAL_DATUM_NGVD29);
        assert(!strcmp(charVal, CVERTICAL_DATUM_NGVD29));

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
    // test storing and retriving time series data
    {
        long long ifltab[250];
        int status;
        zStructTimeSeries *tss = NULL;
        verticalDatumInfo vdi;
        char *errmsg;
        char *filename[]      = {"v6_c.dss", "v7_c.dss"};
        char *pathnames[2][2] = {{"//TestTsLoc/Elev//1Hour/Doubles/",    "//TestTsLoc/Elev//1Hour/Floats/"},
                                 {"//TestTsLoc/Elev//Ir-Month/Doubles/", "//TestTsLoc/Elev//Ir-Month/Floats/"}};
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
        char  buf[80];
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
        zset("MLVL", "", 1);
        for (int i = 0; i < 2; ++i) {
            for (int j = 0; j < xml_count; ++j) {
                for (int k = 0; k < verticalDatumCount; ++k) {
                    int k2 = (k+1) % verticalDatumCount;
                    int k3 = (k+2) % verticalDatumCount;
                    for (int l = 0; l < unitCount; ++ l) {
                        for (int m = 0; m < 3; ++m) {
                            for (int n = 0; n < 2; ++n) {
                                for (int o = 0; o < 2; ++o) {
                                    //------------------------//
                                    // create the time series //
                                    //------------------------//
                                    // fprintf(stderr, "%d\t%d\t%d\t%d\t%d\t%d\t%d\n", i,j,k,l,m,n,o);
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
                                    //------------------------------------------------//
                                    // add the vertical datum info to the user header //
                                    //------------------------------------------------//
                                    errmsg = gzipAndEncode(&compressed, xml[j]);
                                    assert(errmsg == NULL);
                                    len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 1;
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
                                    tss->userHeader = stringToUserHeader(headerBuf, &tss->userHeaderSize);
                                    tss->userHeaderNumber = tss->userHeaderSize; 
                                    tss->allocated[zSTRUCT_userHeader] = TRUE;
                                    free(headerBuf);
                                    if (m > 0) {
                                        //----------------------------------------------------------//
                                        // override the default vertical datum with the user header //
                                        //----------------------------------------------------------//
                                        K = k2;
                                        char *headerString = userHeaderToString(tss->userHeader, tss->userHeaderSize);
                                        int headerLen = strlen(headerString);
                                        status = insertIntoDelimitedString(
                                            &headerString, 
                                            headerLen, 
                                            VERTICAL_DATUM_USER_HEADER_PARAM, 
                                            verticalDatums[K], 
                                            ":",
                                            FALSE,
                                            ';');
                                        if (status != 0) {
                                            headerLen += VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(verticalDatums[K]) + 3;
                                            headerString = (char *)realloc(headerString, headerLen);
                                            status = insertIntoDelimitedString(
                                                &headerString, 
                                                headerLen, 
                                                VERTICAL_DATUM_USER_HEADER_PARAM, 
                                                verticalDatums[K], 
                                                ":",
                                                FALSE,
                                                ';');
                                            assert(status == 0);
                                        }
                                        free(tss->userHeader);
                                        tss->userHeader = stringToUserHeader(headerString, &tss->userHeaderSize);
                                        tss->userHeaderNumber = tss->userHeaderSize;
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
                                    //-------------------------------------------------------//
                                    // store the time series in the specified vertical datum //
                                    //-------------------------------------------------------//
                                    status = ztsStore(ifltab, tss, 0);
                                    //-----------------------------------------------------------------------------//
                                    // figure out whether the ztsStore should have succeeded, and test accordingly //
                                    //-----------------------------------------------------------------------------//
                                    stringToVerticalDatumInfo(&vdi, xml[j]);
                                    if (i == 1 && j == 1 && k+l+n+m+o == 0) {
                                        //-------------------------------------------------------------------------------//
                                        // change of vertical datum information in DSS 7, need to update location record //
                                        //-------------------------------------------------------------------------------//
                                        assert(status != STATUS_OKAY);
                                        zset("VDOW", "", TRUE);
                                        status = ztsStore(ifltab, tss, 0);
                                        zset("VDOW", "", FALSE);
                                    }
                                    if (!strcmp(vdi.nativeDatum, verticalDatums[K])) {
                                        //-------------------------------------//
                                        // same datum, no conversion necessary //
                                        //-------------------------------------//
                                        assert(status == STATUS_OKAY);
                                    }
                                    else if (strcmp(verticalDatums[K], CVERTICAL_DATUM_NAVD88) && strcmp(verticalDatums[K], CVERTICAL_DATUM_NGVD29)) {
                                        //------------------------------------------------------------------------------//
                                        // specified datum is local, so by definition it is already in the native datum //
                                        // and no conversion is necessary                                               //
                                        //------------------------------------------------------------------------------//
                                        assert(status == STATUS_OKAY);
                                    }
                                    else if (!strcmp(verticalDatums[K], CVERTICAL_DATUM_NAVD88) && vdi.offsetToNavd88 != UNDEFINED_VERTICAL_DATUM_VALUE) {
                                        //-------------------------------------------------------------//
                                        // specified datum is NAVD-88 and we have an offset to NAVD-88 //
                                        //-------------------------------------------------------------//
                                        if (unitIsFeet(unit[l]) || unitIsMeters(unit[l])) {
                                            assert(status == STATUS_OKAY);
                                        }
                                        else {
                                            assert(status != STATUS_OKAY);
                                        }
                                    }
                                    else if (!strcmp(verticalDatums[K], CVERTICAL_DATUM_NGVD29) && vdi.offsetToNgvd29 != UNDEFINED_VERTICAL_DATUM_VALUE) {
                                        //-------------------------------------------------------------//
                                        // specified datum is NGVD-29 and we have an offset to NGVD-29 //
                                        //-------------------------------------------------------------//
                                        if (unitIsFeet(unit[l]) || unitIsMeters(unit[l])) {
                                            assert(status == STATUS_OKAY);
                                        }
                                        else {
                                            assert(status != STATUS_OKAY);
                                        }
                                    }
                                    else {
                                        //-----------------//
                                        // all other cases //
                                        //-----------------//
                                        assert(status != STATUS_OKAY);
                                    }                                            
                                    zclose(ifltab);
                                    zstructFree(tss);
                                    if (status == STATUS_OKAY) {
                                        //------------------------------------------------------------//
                                        // set the default vertical datum to the datum we stored with //
                                        //------------------------------------------------------------//
                                        zset("VDTM", verticalDatums[K], 0);
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
                                        status = ztsRetrieve(ifltab, tss, -1, 0, 1);
                                        assert(status == STATUS_OKAY);
                                        //------------------------------------------------------//
                                        // compare the retrieved time seires to what was stored //
                                        //------------------------------------------------------//
                                        if (tss->numberValues != numberValues) {
                                            fprintf(stderr, "Expected %d values, got %d\n", numberValues, tss->numberValues);
                                        }
                                        assert(tss->numberValues == numberValues);
                                        if (o == 0) {
                                            assert(tss->doubleValues != NULL);
                                            for (int ii = 0; ii < tss->numberValues; ++ii) {
                                                if (tss->doubleValues[ii] != dvalues[l][ii]) {
                                                    fprintf(stderr, "%f != %f\n", tss->doubleValues[ii], dvalues[l][ii]);
                                                }
                                                assert(tss->doubleValues[ii] == dvalues[l][ii]);
                                            }
                                        }
                                        else {
                                            assert(tss->floatValues != NULL);
                                            for (int ii = 0; ii < tss->numberValues; ++ii) {
                                                if (tss->floatValues[ii] != fvalues[l][ii]) {
                                                    fprintf(stderr, "%f != %f\n", tss->floatValues[ii], fvalues[l][ii]);
                                                }
                                                assert(tss->floatValues[ii] == fvalues[l][ii]);
                                            }
                                        }
                                        zclose(ifltab);
                                        zstructFree(tss);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    // test storing and retrieving paired data
//     {
//         long long ifltab[250];
//         zStructPairedData *pds;
//         int    status;
//         char  *errmsg;
//         char  *filename[2]      = {"v6_c.dss", "v7_c.dss"};
//         char  *pathnames[2][2]  = {{"//TestPdLoc/Stage-Elev///Doubles/", "//TestPdLoc/Stage-Elev///Floats/"},
//                                    {"//TestPdLoc/Elev-Stage///Doubles/", "//TestPdLoc/Elev-Stage///Floats/"}};
//         char  *type             = "Linear";
//         char  *unit[]           = {"ft", "m", "cfs"};
//         double dordinates[3][6] = {{1000,1001,1002,1003,1004,1005},                           // ft
//                                    {304.8,305.1048,305.4096,305.7144,306.0192,306.324},       // m
//                                    {1000,1001,1002,1003,1004,1005}};                          // cfs
//         double dvalues[3][6]    = {{1000,1001,1002,1003,1004,1005},                           // ft
//                                    {304.8,305.1048,305.4096,305.7144,306.0192,306.324},       // m
//                                    {1000,1001,1002,1003,1004,1005}};                          // cfs
//         float  fordinates[3][6] = {{1000.f,1001.f,1002.f,1003.f,1004.f,1005.f},               // ft
//                                    {304.8f,305.1048f,305.4096f,305.7144f,306.0192f,306.324f}, // m
//                                    {1000.f,1001.f,1002.f,1003.f,1004.f,1005.f}};              // cfs
//         float  fvalues[3][6]    = {{1000.f,1001.f,1002.f,1003.f,1004.f,1005.f},               // ft
//                                    {304.8f,305.1048f,305.4096f,305.7144f,306.0192f,306.324f}, // m
//                                    {1000.f,1001.f,1002.f,1003.f,1004.f,1005.f}};              // cfs
//         int    numberOrdinates   = 6;
//         int    numberCurves      = 1;
//         char  *compressed   = NULL;
//         char  *headerBuf    = NULL;
//         int    len          = 0;
//         char   buf[80];
//         char   unitSpec[24];
//         char *xml[]       = {
//             "<vertical-datum-info unit=\"ft\">\n"
//             "  <native-datum>NGVD-29</native-datum>\n"
//             "  <elevation>615.2</elevation>\n"
//             "  <offset estimate=\"true\">\n"
//             "    <to-datum>NAVD-88</to-datum>\n"
//             "    <value>0.3855</value>\n"
//             "  </offset>\n"
//             "</vertical-datum-info>\n",

//             "<vertical-datum-info unit=\"ft\">\n"
//             "  <native-datum>OTHER</native-datum>\n"
//             "  <local-datum-name>Pensacola</local-datum-name>\n"
//             "  <elevation>757</elevation>\n"
//             "  <offset estimate=\"true\">\n"
//             "    <to-datum>NAVD-88</to-datum>\n"
//             "    <value>1.457</value>\n"
//             "  </offset>\n"
//             "  <offset estimate=\"false\">\n"
//             "    <to-datum>NGVD-29</to-datum>\n"
//             "    <value>1.07</value>\n"
//             "  </offset>\n"
//             "</vertical-datum-info>\n"
//         };
//         char *verticalDatums[] = {
//             CVERTICAL_DATUM_NAVD88,
//             CVERTICAL_DATUM_NGVD29,
//             "Pensacola"
//         };
//         int unitCount = sizeof(unit) / sizeof(unit[0]);
//         int xml_count = sizeof(xml) / sizeof(xml[0]);
//         int verticalDatumCount = sizeof(verticalDatums) / sizeof(verticalDatums[0]);
//         //
//         // loop variables
//         //
//         // i = DSS file version
//         //     0 = DSS 6
//         //     1 = DSS 7
//         //
//         // j = xml blocks
//         //     0 = NGVD-29 native
//         //     1 = OTHER native with local datum named "Pensacola"
//         //
//         // k = vertical datum
//         //     0 = NAVD-88
//         //     1 = NGVD-29
//         //     2 = OTHER (Pensacola)
//         //     k2  (k + 1) % 3
//         //     k3  (k + 2) % 3
//         //
//         // l = data units
//         //     0 = ft
//         //     1 = m
//         //     2 = cfs (invalid for datum conversion)
//         //
//         // m = vertical datum specification method
//         //     0 = set default with zset
//         //     1 = 0 plus override with user header
//         //     2 = 1 plus override with unit spec
//         //
//         // n = elevation param position
//         //     0 = dependent parameter
//         //     1 = independent parameter
//         //
//         // o = data value type
//         //     0 = doubles
//         //     1 = floats 
//         zset("MLVL", "", 0);
//         for (int i = 0; i < 2; ++i) {
//             for (int j = 0; j < xml_count; ++j) {
//                 for (int k = 0; k < verticalDatumCount; ++k) {
//                     int k2 = (k+1) % verticalDatumCount;
//                     int k3 = (k+2) % verticalDatumCount;
//                     for (int l = 0; l < unitCount; ++ l) {
//                         for (int m = 0; m < 3; ++m) {
//                             for (int n = 0; n < 2; ++n) {
//                                 for (int o = 0; o < 2; ++o) {
//                                     //------------------------//
//                                     // create the paired data //
//                                     //------------------------//
//                                     // fprintf(stderr, "%d\t%d\t%d\t%d\t%d\t%d\t%d\n", i,j,k,l,m,n,o);
//                                     if (i == 0) {
//                                         status = zopen6(ifltab, filename[i]);
//                                     }
//                                     else {
//                                         status = zopen7(ifltab, filename[i]);
//                                     }
//                                     assert(status == STATUS_OKAY);
//                                     if (o == 0) {
//                                         pds = zstructPdNewDoubles(
//                                             pathnames[n][o],
//                                             dordinates[l],
//                                             dvalues[l],
//                                             numberOrdinates,
//                                             numberCurves,
//                                             unit[l],
//                                             type,
//                                             unit[l],
//                                             type);
//                                     }
//                                     else {
//                                         pds = zstructPdNewFloats(
//                                             pathnames[n][o],
//                                             fordinates[l],
//                                             fvalues[l],
//                                             numberOrdinates,
//                                             numberCurves,
//                                             unit[l],
//                                             type,
//                                             unit[l],
//                                             type);
//                                     }
//                                     assert(pds != NULL);
//                                     //--------------------------------//        
//                                     // set the default vertical datum //
//                                     //--------------------------------//
//                                     int K = k;
//                                     zset("VDTM", verticalDatums[K], 0);
//                                     //------------------------------------------------//
//                                     // add the vertical datum info to the user header //
//                                     //------------------------------------------------//
//                                     errmsg = gzipAndEncode(&compressed, xml[j]);
//                                     assert(errmsg == NULL);
//                                     len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed) + 1;
//                                     headerBuf = (char *)malloc(len+1);
//                                     memset(headerBuf, 0, len+1);
//                                     status = insertIntoDelimitedString(
//                                         &headerBuf, 
//                                         len+1, 
//                                         VERTICAL_DATUM_INFO_USER_HEADER_PARAM, 
//                                         compressed, 
//                                         ":",
//                                         FALSE,
//                                         ';');
//                                     assert(status == 0);
//                                     pds->userHeader = stringToUserHeader(headerBuf, &pds->userHeaderNumber);
//                                     pds->userHeaderNumber = pds->userHeaderNumber; 
//                                     pds->allocated[zSTRUCT_userHeader] = 1;
//                                     free(headerBuf);
//                                     if (m > 0) {
//                                         //----------------------------------------------------------//
//                                         // override the default vertical datum with the user header //
//                                         //----------------------------------------------------------//
//                                         K = k2;
//                                         char *headerString = userHeaderToString(pds->userHeader, pds->userHeaderNumber);
//                                         int headerLen = strlen(headerString);
//                                         status = insertIntoDelimitedString(
//                                             &headerString, 
//                                             headerLen, 
//                                             VERTICAL_DATUM_USER_HEADER_PARAM, 
//                                             verticalDatums[K], 
//                                             ":",
//                                             FALSE,
//                                             ';');
//                                         if (status != 0) {
//                                             headerLen += VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(verticalDatums[K]) + 3;
//                                             headerString = (char *)realloc(headerString, headerLen);
//                                             status = insertIntoDelimitedString(
//                                                 &headerString, 
//                                                 headerLen, 
//                                                 VERTICAL_DATUM_USER_HEADER_PARAM, 
//                                                 verticalDatums[K], 
//                                                 ":",
//                                                 FALSE,
//                                                 ';');
//                                             assert(status == 0);
//                                         }
//                                         pds->userHeader = stringToUserHeader(headerString, &pds->userHeaderNumber);
//                                         if (m > 1) {
//                                             //--------------------------------------------------------//
//                                             // override default and user header datums with unit spec //
//                                             //--------------------------------------------------------//
//                                             K = k3;
//                                             sprintf(unitSpec, "U=%s|V=%s", unit[l], verticalDatums[K]);
//                                             if (n == 0) {
//                                                 free(pds->unitsDependent);
//                                                 pds->unitsDependent = mallocAndCopy(unitSpec);
//                                             }
//                                             else {
//                                                 free(pds->unitsIndependent);
//                                                 pds->unitsIndependent = mallocAndCopy(unitSpec);
//                                             }
//                                         }
//                                     }
//                                     //--------------------------------------------------------//
//                                     // store the paired data in the overridden vertical datum //
//                                     //--------------------------------------------------------//
//                                     status = zpdStore(ifltab, pds, 0);
//                                     //-----------------------------------------------------------------------------//
//                                     // figure out whether the zpdStore should have succeeded, and test accordingly //
//                                     //-----------------------------------------------------------------------------//
//                                     if (!strstr(xml[j], verticalDatums[K])) {
//                                         assert(status != STATUS_OKAY);
//                                     }
//                                     else if (i == 1 && j == 1 && k+l+n+m+o == 0) {
//                                         // change of vertical datum information
//                                         assert(status != STATUS_OKAY);
//                                         zset("VDOW", "", TRUE);
//                                         status = zpdStore(ifltab, pds, 0);
//                                         assert(status == STATUS_OKAY);
//                                         zset("VDOW", "", FALSE);
//                                     }
//                                     else if (strcmp(unit[l], "ft") && strcmp(unit[l], "m")) {
//                                         sprintf(buf, "<native-datum>%s</native-datum>", verticalDatums[K]);
//                                         if (strstr(xml[j], buf)) {
//                                             assert(status == STATUS_OKAY);
//                                         }
//                                         else {
//                                             sprintf(buf, "<local-datum-name>%s</local-datum-name", verticalDatums[K]);
//                                             if (strstr(xml[j], buf)) {
//                                                 assert(status == STATUS_OKAY);
//                                             }
//                                             else {
//                                                 assert(status != STATUS_OKAY);
//                                             }
//                                         }
//                                     }
//                                     else {
//                                         assert(status == STATUS_OKAY);
//                                     }
//                                     zclose(ifltab);
//                                     zstructFree(pds);
//                                     if (status == STATUS_OKAY) {
//                                         //------------------------------------------------------------//
//                                         // set the default vertical datum to the datum we stored with //
//                                         //------------------------------------------------------------//
//                                         zset("VDTM", verticalDatums[K], 0);
//                                         double tolerance = 0.0000001;
//                                         double diff;
//                                         //--------------------------------------------------------//
//                                         // retrieve the time series in the default vertical datum //
//                                         //-------------------------------------------------------//
//                                         if (i == 0) {
//                                             status = zopen6(ifltab, filename[i]);
//                                         }
//                                         else {
//                                             status = zopen7(ifltab, filename[i]);
//                                         }
//                                         assert(status == STATUS_OKAY);
//                                         pds = zstructPdNew(pathnames[n][o]);
//                                         assert(pds != NULL);
//                                         status = zpdRetrieve(ifltab, pds, 0);
//                                         assert(status == STATUS_OKAY);
//                                         //------------------------------------------------------//
//                                         // compare the retrieved paired data to what was stored //
//                                         //------------------------------------------------------//
//                                         if (pds->numberOrdinates != numberOrdinates) {
//                                             fprintf(stderr, "Expected %d ordinates, got %d\n", numberOrdinates, pds->numberOrdinates);
//                                         }
//                                         assert(pds->numberOrdinates == numberOrdinates);
//                                         if (pds->numberCurves != numberCurves) {
//                                             fprintf(stderr, "Expected %d curves, got %d\n", numberCurves, pds->numberCurves);
//                                         }
//                                         assert(pds->numberCurves == numberCurves);
//                                         if (o == 0) {
//                                             assert(pds->doubleOrdinates != NULL);
//                                             assert(pds->doubleValues != NULL);
//                                         }
//                                         else {
//                                             assert(pds->floatOrdinates != NULL);
//                                             assert(pds->floatValues != NULL);
//                                         }
//                                         if (o == 0) {
//                                             for (int ii = 0; ii < pds->numberOrdinates; ++ii) {
//                                                 if (pds->doubleOrdinates[ii] != dordinates[l][ii]) {
//                                                     fprintf(stderr, "ordinate %f != %f\n", pds->doubleOrdinates[ii], dordinates[l][ii]);
//                                                 }
//                                                 assert(pds->doubleOrdinates[ii] == dordinates[l][ii]);
//                                             }
//                                             for (int ii = 0; ii < pds->numberCurves * pds->numberOrdinates; ++ii) {
//                                                 if (pds->doubleValues[ii] != dvalues[l][ii]) {
//                                                     fprintf(stderr, "value %f != %f\n", pds->doubleValues[ii], dvalues[l][ii]);
//                                                 }
//                                                 assert(pds->doubleValues[ii] == dvalues[l][ii]);
//                                             }
//                                         }
//                                         else {
//                                             for (int ii = 0; ii < pds->numberOrdinates; ++ii) {
//                                                 if (pds->floatOrdinates[ii] != fordinates[l][ii]) {
//                                                     fprintf(stderr, "ordinate %f != %f\n", pds->floatOrdinates[ii], fordinates[l][ii]);
//                                                 }
//                                                 assert(pds->floatOrdinates[ii] == fordinates[l][ii]);
//                                             }
//                                             for (int ii = 0; ii < pds->numberCurves * pds->numberOrdinates; ++ii) {
//                                                 if (pds->floatValues[ii] != fvalues[l][ii]) {
//                                                     fprintf(stderr, "value %f != %f\n", pds->floatValues[ii], fvalues[l][ii]);
//                                                 }
//                                                 assert(pds->floatValues[ii] == fvalues[l][ii]);
//                                             }
//                                         }
//                                         zclose(ifltab);
//                                         zstructFree(pds);
//                                     }
//                                 }
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//    }
   exit(0);
}