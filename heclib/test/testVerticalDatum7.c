#include <assert.h>
#include <string.h>
#include <heclib.h>
#include <verticalDatum.h>

void main (int argc, char *argv[]) {
    // test extract_from_user_header_string() and insert_into_user_header_string()
    {
        int text_size = 256;
        char *text = (char *)malloc(text_size);
        char *cp;
        char *text_value = "theFirstParameter:theFirstValue;theSecondParameter:theSecondValue";
        sprintf(text, text_value);
        cp = extract_from_user_header_string(&text, "theFirstParameter", TRUE, FALSE);
        assert(!strcmp(cp, "theFirstValue"));
        cp = extract_from_user_header_string(&text, "theSecondParameter", TRUE, FALSE);
        assert(!strcmp(cp, "theSecondValue"));
        cp = extract_from_user_header_string(&text, "THEFIRSTPARAMETER", FALSE, FALSE);
        assert(!strcmp(cp, "theFirstValue"));
        cp = extract_from_user_header_string(&text, "THEFIRSTPARAMETER", FALSE, TRUE);
        assert(!strcmp(cp, "theFirstValue"));
        assert(!strcmp(text, "theSecondParameter:theSecondValue"));
        sprintf(text, text_value);
        cp = extract_from_user_header_string(&text, "theSecondParameter", TRUE, TRUE);
        assert(!strcmp(cp, "theSecondValue"));
        assert(!strcmp(text, "theFirstParameter:theFirstValue"));
        assert(insert_into_user_header_string(&text, text_size, "anotherParameter", "anotherValue", FALSE) ==  0);
        assert(!strcmp(text, "theFirstParameter:theFirstValue;anotherParameter:anotherValue"));
        extract_from_user_header_string(&text, "theFirstParameter", TRUE, TRUE);
        extract_from_user_header_string(&text, "anotherParameter", TRUE, TRUE);
        assert(strlen(text) == 0);
        assert(insert_into_user_header_string(&text, text_size, "anotherParameter", "anotherValue", FALSE) == 0);
        assert(strcmp(text, "anotherParameter:anotherValue") == 0);
        free(text);
    }
    // test compress_and_encode() and decode_and_uncompress()
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

        errmsg = compress_and_encode(&compressed, input_text);
        if (errmsg != NULL) fprintf(stderr, "%s\n", errmsg);
        assert(errmsg == NULL);
        errmsg = decode_and_uncompress(&expanded, compressed);
        if (errmsg != NULL) fprintf(stderr, "%s\n", errmsg);
        assert(errmsg == NULL);
        assert(!strcmp(expanded, input_text));
    }
    // test vertical_datum_info_from_string() and vertical_datum_info_to_string()
    {
        vertical_datum_info vdi;
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

        errmsg = vertical_datum_info_from_string(&vdi, xml1);
        if (errmsg != NULL) fprintf(stderr, "%s\n", errmsg);
        assert(errmsg == NULL);
        assert(!strcmp(vdi.native_datum, CVERTICAL_DATUM_NGVD29));
        assert(vdi.elevation == 615.2f);
        assert(vdi.offset_to_navd_88 == 0.3855f);
        assert(vdi.offset_to_navd_88_is_estimate == 1);
        assert(vdi.offset_to_ngvd_29 == 0.f);
        assert(vdi.offset_to_ngvd_29_is_estimate == 0);

        errmsg = vertical_datum_info_from_string(&vdi, xml2);
        if (errmsg != NULL) fprintf(stderr, "%s\n", errmsg);
        assert(errmsg == NULL);
        assert(!strcmp(vdi.native_datum, "Pensacola"));
        assert(vdi.elevation == 757.f);
        assert(vdi.offset_to_navd_88 == 1.457f);
        assert(vdi.offset_to_navd_88_is_estimate == 1);
        assert(vdi.offset_to_ngvd_29 == 1.07f);
        assert(vdi.offset_to_ngvd_29_is_estimate == 0);

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
    }
    // test string_to_user_header() and string_from_user_header()
    {
        int  *userHeader;
        int   userHeaderSize;
        char *userHeaderStringIn;
        char *userHeaderStringOut;
        userHeaderStringIn = "This is a test string for the user header ";
        userHeader = string_to_user_header(userHeaderStringIn, &userHeaderSize);
        assert(userHeaderSize == (strlen(userHeaderStringIn)-1)/4+1);
        userHeaderStringOut = string_from_user_header(userHeader, userHeaderSize);
        assert(!strcmp(userHeaderStringIn, userHeaderStringOut));
        free(userHeader);
        free(userHeaderStringOut);
    }
    // test storing and retriving time series data
    {
        long long ifltab[250];
        int status;
        zStructTimeSeries *tss = NULL;
        vertical_datum_info vdi;
        char *errmsg;
        char *filename    = "v7.dss";
        char *pathnames[2][2] = {{"//TestLoc/Elev//1Hour/TestDoubles/",    "//TestLoc/Elev//1Hour/TestFloats/"},
                                 {"//TestLoc/Elev//Ir-Month/TestDoubles/", "//TestLoc/Elev//Ir-Month/Floats/"}};
        double dvalues[3][6]  = {{1000,1001,1002,1003,1004,1005},                           // ft
                                 {304.8,305.1048,305.4096,305.7144,306.0192,306.324},       // m
                                 {1000,1001,1002,1003,1004,1005}};                          // cfs
        float  fvalues[3][6]  = {{1000.f,1001.f,1002.f,1003.f,1004.f,1005.f},               // ft
                                 {304.8f,305.1048f,305.4096f,305.7144f,306.0192f,306.324f}, // m
                                 {1000.f,1001.f,1002.f,1003.f,1004.f,1005.f}};              // cfs
        int numberValues  = 6;
        char *startDate   = "01Oct2021";
        char *startTime   = "01:00";
        char *endDate     = "31Oct2021";
        char *endTime     = "24:00";
        int   itimes[]    = {64035420, 64035480, 64035540, 64035600, 64035660, 64035720};
        char *unit[]      = {"ft", "m", "cfs"};
        char *type        = "INST-VAL";
        char *compressed  = NULL;
        char *headerBuf   = NULL;
        int   len         = 0;
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
        // i = xml blocks
        //     0 = NGVD-29 native
        //     1 = OTHER native with local datum named "Pensacola"
        //
        // j = vertical datum
        //     0 = NAVD-88
        //     1 = NGVD-29
        //     2 = OTHER (Pensacola)
        //     j2  (j + 1) % 3
        //     j3  (j + 2) % 3
        //
        // k = data units
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
        // p = data value type
        //     0 = doubles
        //     1 = floats 
        zset("MLVL", "", 0);
        for (int i = 0; i < xml_count; ++i) {
            for (int j = 0; j < verticalDatumCount; ++j) {
                int j2 = (j+1) % verticalDatumCount;
                int j3 = (j+2) % verticalDatumCount;
                for (int k = 0; k < unitCount; ++ k) {
                    for (int m = 0; m < 3; ++m) {
                        for (int n = 0; n < 2; ++n) {
                            for (int p = 0; p < 2; ++p) {
                                //------------------------//
                                // create the time series //
                                //------------------------//
                                status = zopen(ifltab, filename);
                                assert(status == STATUS_OKAY);
                                if (n == 0) {
                                    if (p == 0) {
                                        tss = zstructTsNewRegDoubles(
                                            pathnames[n][p],
                                            dvalues[k],
                                            numberValues,
                                            startDate,
                                            startTime,
                                            unit[k],
                                            type);
                                    }
                                    else {
                                        tss = zstructTsNewRegFloats(
                                            pathnames[n][p],
                                            fvalues[k],
                                            numberValues,
                                            startDate,
                                            startTime,
                                            unit[k],
                                            type);
                                    }
                                }
                                else {
                                    if (p == 0) {
                                        tss = zstructTsNewIrregDoubles(
                                            pathnames[n][p],
                                            dvalues[k],
                                            numberValues,
                                            itimes,
                                            60,
                                            NULL,
                                            unit[k],
                                            type);
                                    }
                                    else {
                                        tss = zstructTsNewIrregFloats(
                                            pathnames[n][p],
                                            fvalues[k],
                                            numberValues,
                                            itimes,
                                            60,
                                            NULL,
                                            unit[k],
                                            type);
                                    }
                                }
                                assert(tss != NULL);
                                //--------------------------------//        
                                // set the default vertical datum //
                                //--------------------------------//
                                int J = j;
                                zset("VDTM", verticalDatums[J], 0);
                                //------------------------------------------------//
                                // add the vertical datum info to the user header //
                                //------------------------------------------------//
                                errmsg = compress_and_encode(&compressed, xml[i]);
                                assert(errmsg == NULL);
                                len = VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + strlen(compressed);
                                headerBuf = (char *)malloc(len+1);
                                memset(headerBuf, 0, len+1);
                                status = insert_into_user_header_string(
                                    &headerBuf, 
                                    len+1, 
                                    VERTICAL_DATUM_INFO_USER_HEADER_PARAM, 
                                    compressed, 
                                    FALSE);
                                assert(status == 0);
                                tss->userHeader = string_to_user_header(headerBuf, &tss->userHeaderSize);
                                tss->userHeaderNumber = tss->userHeaderSize; 
                                tss->allocated[zSTRUCT_userHeader] = 1;
                                free(headerBuf);
                                if (m > 0) {
                                    //----------------------------------------------------------//
                                    // override the default vertical datum with the user header //
                                    //----------------------------------------------------------//
                                    J = j2;
                                    char *headerString = string_from_user_header(tss->userHeader, tss->userHeaderSize);
                                    int headerLen = strlen(headerString);
                                    status = insert_into_user_header_string(
                                        &headerString, 
                                        headerLen, 
                                        VERTICAL_DATUM_USER_HEADER_PARAM, 
                                        verticalDatums[J], 
                                        FALSE);
                                    if (status != 0) {
                                        headerLen += VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(verticalDatums[J]) + 2;
                                        headerString = (char *)realloc(headerString, headerLen);
                                        status = insert_into_user_header_string(
                                            &headerString, 
                                            headerLen, 
                                            VERTICAL_DATUM_USER_HEADER_PARAM, 
                                            verticalDatums[J], 
                                            FALSE);
                                        assert(status == 0);
                                    }
                                    tss->userHeader = string_to_user_header(headerString, &tss->userHeaderSize);
                                    tss->userHeaderNumber = tss->userHeaderSize;
                                    if (m > 1) {
                                        //--------------------------------------------------------//
                                        // override default and user header datums with unit spec //
                                        //--------------------------------------------------------//
                                        J = j3;
                                        sprintf(unitSpec, "U=%s|V=%s", unit[k], verticalDatums[J]);
                                        free(tss->units);
                                        tss->units = mallocAndCopy(unitSpec);
                                    }
                                }
                                //--------------------------------------------------------//
                                // store the time series in the overridden vertical datum //
                                //--------------------------------------------------------//
                                status = ztsStore(ifltab, tss, 0);
                                zclose(ifltab);
                                zstructFree(tss);
                                //-----------------------------------------------------------------------------//
                                // figure out whether the ztsStore should have succeeded, and test accordingly //
                                //-----------------------------------------------------------------------------//
                                if (!strstr(xml[i], verticalDatums[J])) {
                                    assert(status != STATUS_OKAY);
                                }
                                else if (strcmp(unit[k], "ft") && strcmp(unit[k], "m")) {
                                    sprintf(buf, "<native-datum>%s</native-datum>", verticalDatums[J]);
                                    if (strstr(xml[i], buf)) {
                                        assert(status == STATUS_OKAY);
                                    }
                                    else {
                                        sprintf(buf, "<local-datum-name>%s</local-datum-name", verticalDatums[J]);
                                        if (strstr(xml[i], buf)) {
                                            assert(status == STATUS_OKAY);
                                        }
                                        else {
                                            assert(status != STATUS_OKAY);
                                        }
                                    }
                                }
                                else {
                                    assert(status == STATUS_OKAY);
                                }
                                if (status == STATUS_OKAY) {
                                    //------------------------------------------------------------//
                                    // set the default vertical datum to the datum we stored with //
                                    //------------------------------------------------------------//
                                    zset("VDTM", verticalDatums[J], 0);
                                    //--------------------------------------------------------//
                                    // retrieve the time series in the default vertical datum //
                                    //-------------------------------------------------------//
                                    status = zopen(ifltab, filename);
                                    assert(status == STATUS_OKAY);
                                    tss = zstructTsNewTimes(
                                        pathnames[n][p],
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
                                    if (p == 0) {
                                        assert(tss->doubleValues != NULL);
                                    }
                                    else {
                                        assert(tss->floatValues != NULL);
                                    }
                                    for (int ii = 0; ii < tss->numberValues; ++ii) {
                                        if (p == 0) {
                                            if (tss->doubleValues[ii] != dvalues[k][ii]) {
                                                fprintf(stderr, "%f != %f\n", tss->doubleValues[ii], dvalues[k][ii]);
                                            }
                                            assert(tss->doubleValues[ii] == dvalues[k][ii]);
                                        }
                                        else {
                                            if (tss->floatValues[ii] != fvalues[k][ii]) {
                                                fprintf(stderr, "%f != %f\n", tss->floatValues[ii], fvalues[k][ii]);
                                            }
                                            assert(tss->floatValues[ii] == fvalues[k][ii]);
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

    exit(0);
}