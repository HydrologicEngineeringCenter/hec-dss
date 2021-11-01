#include <assert.h>
#include <string.h>
#include <heclib.h>
#include <verticalDatum.h>

void main (int argc, char *argv[]) {
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
    zset("MLVL", "", 0);
    {
        long long ifltab[250];
        int status;
        zStructTimeSeries *tss = NULL;
        vertical_datum_info vdi;
        char *errmsg;
        char *filename    = "v7.dss";
        char *pathname    = "//TestLoc/Elev//1Hour/Test/";
        double values[3][6] = {{1000,1001,1002,1003,1004,1005},                    // ft
                              {304.8,305.1048,305.4096,305.7144,306.0192,306.324}, // m
                              {1000,1001,1002,1003,1004,1005}};                    // cfs
        int numberValues  = 6;
        char *startDate   = "01Oct2021";
        char *startTime   = "01:00";
        char *endDate     = "31Oct2021";
        char *endTime     = "24:00";
        char *unit[]      = {"ft", "m", "cfs"};
        char *type        = "INST-VAL";
        char *compressed  = NULL;
        char *headerBuf   = NULL;
        int   len         = 0;
        char  unitSpec[24];
        char *xml[]       = {
            "<vertical-datum-info office=\"SWT\" unit=\"ft\">\n"
            "  <location>TULA</location>\n"
            "  <native-datum>NGVD-29</native-datum>\n"
            "  <elevation>615.2</elevation>\n"
            "  <offset estimate=\"true\">\n"
            "    <to-datum>NAVD-88</to-datum>\n"
            "    <value>0.3855</value>\n"
            "  </offset>\n"
            "</vertical-datum-info>\n",

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

        for (int i = 0; i < xml_count; ++i) {
            // fprintf(stderr, "\nVertical Datum Info :\n%s\n", xml[i]);
            for (int j = 0; j < verticalDatumCount; ++j) {
                // fprintf(stderr, "Default vertical datum = %s\n", verticalDatums[j]);
                for (int k = 0; k < unitCount; ++ k) {
                    // fprintf(stderr, "unit = %s\n", unit[k]);
                    status = zopen(ifltab, filename);
                    assert(status == STATUS_OKAY);
                    //--------------------------------//        
                    // set the default vertical datum //
                    //--------------------------------//        
                    zset("VDTM", verticalDatums[j], 0);
                    //------------------------//
                    // create the time series //
                    //------------------------//
                    tss = zstructTsNewRegDoubles(
                        pathname,
                        values[k],
                        numberValues,
                        startDate,
                        startTime,
                        unit[k],
                        type);
                    assert(tss != NULL);
                    //------------------------------------------------//
                    // add the vertical datum info to the user header //
                    //------------------------------------------------//
                    errmsg = compress_and_encode(&compressed, xml[i]);
                    assert(errmsg == NULL);
                    len = VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(compressed);
                    headerBuf = (char *)malloc(len+1);
                    sprintf(headerBuf, "%s%s", VERTICAL_DATUM_USER_HEADER_PARAM, compressed);
                    tss->userHeaderNumber = numberIntsInBytes(len);
                    tss->userHeaderSize = tss->userHeaderNumber; 
                    tss->userHeader = (int *)calloc(tss->userHeaderNumber, 4);
                    tss->allocated[zSTRUCT_userHeader] = 1;
                    charInt ((void *)headerBuf, tss->userHeader, len, (tss->userHeaderNumber * 4), 0, 1, 0);
                    //-----------------------------------------------------//
                    // store the time series in the default vertical datum //
                    //-----------------------------------------------------//
                    status = ztsStore(ifltab, tss, 0);
                    zclose(ifltab);
                    zstructFree(tss);
                    if (status != STATUS_OKAY) {
                        continue;
                    }
                    //--------------------------------------------------------//
                    // retrieve the time series in the default vertical datum //
                    //-------------------------------------------------------//
                    status = zopen(ifltab, filename);
                    assert(status == STATUS_OKAY);
                    tss = zstructTsNewTimes(
                        pathname,
                        startDate,
                        startTime,
                        endDate,
                        endTime);
                    assert(tss != NULL);
                    status = ztsRetrieve(ifltab, tss, -1, 2, 1);
                    // fprintf(stderr, "Retrieve status = %d\n");
                    
                    // assert(status == STATUS_OKAY);
                    //------------------------------------------------------//
                    // compare the retrieved time seires to what was stored //
                    //------------------------------------------------------//
                    if (tss->numberValues != numberValues) {
                        fprintf(stderr, "Expected %d values, got %d\n", numberValues, tss->numberValues);
                    }
                    assert(tss->numberValues == numberValues);
                    assert(tss->doubleValues != NULL);
                    for (int m = 0; m < tss->numberValues; ++m) {
                        if (tss->doubleValues[m] != values[k][m]) {
                            fprintf(stderr, "%f != %f\n", tss->doubleValues[m], values[k][m]);
                        }
                        assert(tss->doubleValues[m] == values[k][m]);
                    }
                    zstructFree(tss);
                    //-------------------------------------//
                    // override the default vertical datum //
                    //-------------------------------------//
                    // fprintf(stderr, "Overriding %s with %s\n", verticalDatums[j], verticalDatums[(j+1)%verticalDatumCount]);
                    sprintf(unitSpec, "U=%s|V=%s", unit[k], verticalDatums[(j+1)%verticalDatumCount]);
                    // fprintf(stderr, "%s\n", unitSpec);
                    //------------------------//
                    // create the time series //
                    //------------------------//
                    tss = zstructTsNewRegDoubles(
                        pathname,
                        values[k],
                        numberValues,
                        startDate,
                        startTime,
                        unitSpec,
                        type);
                    assert(tss != NULL);
                    //------------------------------------------------//
                    // add the vertical datum info to the user header //
                    //------------------------------------------------//
                    errmsg = compress_and_encode(&compressed, xml[i]);
                    assert(errmsg == NULL);
                    len = VERTICAL_DATUM_USER_HEADER_PARAM_LEN + strlen(compressed);
                    headerBuf = (char *)malloc(len+1);
                    sprintf(headerBuf, "%s%s", VERTICAL_DATUM_USER_HEADER_PARAM, compressed);
                    tss->userHeaderNumber = numberIntsInBytes(len);
                    tss->userHeaderSize = tss->userHeaderNumber; 
                    tss->userHeader = (int *)calloc(tss->userHeaderNumber, 4);
                    tss->allocated[zSTRUCT_userHeader] = 1;
                    charInt ((void *)headerBuf, tss->userHeader, len, (tss->userHeaderNumber * 4), 0, 1, 0);
                    //--------------------------------------------------------//
                    // store the time series in the overridden vertical datum //
                    //--------------------------------------------------------//
                    status = ztsStore(ifltab, tss, 0);
                    zclose(ifltab);
                    zstructFree(tss);
                    if (strcmp(verticalDatums[(j+1)%verticalDatumCount], CVERTICAL_DATUM_NAVD88) && 
                        strcmp(verticalDatums[(j+1)%verticalDatumCount], CVERTICAL_DATUM_NGVD29) &&
                        !strstr(xml[i], verticalDatums[(j+1)%verticalDatumCount])) {

                    }
                    else {
                        assert(status == STATUS_OKAY);
                    }
                    //-------------------------------------------------------------//
                    // set the default vertical datum to the datum of the unitSpec //
                    //-------------------------------------------------------------//
                    zset("VDTM", verticalDatums[(j+1)%verticalDatumCount], 0);
                    //--------------------------------------------------------//
                    // retrieve the time series in the default vertical datum //
                    //-------------------------------------------------------//
                    status = zopen(ifltab, filename);
                    assert(status == STATUS_OKAY);
                    tss = zstructTsNewTimes(
                        pathname,
                        startDate,
                        startTime,
                        endDate,
                        endTime);
                    assert(tss != NULL);
                    status = ztsRetrieve(ifltab, tss, -1, 2, 1);
                    assert(status == STATUS_OKAY);
                    zclose(ifltab);
                    //------------------------------------------------------//
                    // compare the retrieved time seires to what was stored //
                    //------------------------------------------------------//
                    if (tss->numberValues != numberValues) {
                        fprintf(stderr, "Expected %d values, got %d\n", numberValues, tss->numberValues);
                    }
                    assert(tss->numberValues == numberValues);
                    assert(tss->doubleValues != NULL);
                    for (int m = 0; m < tss->numberValues; ++m) {
                        if (tss->doubleValues[m] != values[k][m]) {
                            printf("%f != %f\n", tss->doubleValues[m], values[k][m]);
                        }
                        assert(tss->doubleValues[m] == values[k][m]);
                    }
                    zstructFree(tss);
                }
            }
        }
    }

    exit(0);
}