#include <string.h>
#include "hecdss.h"
#include "heclib.h"

// public declaration
typedef struct dss_file dss_file;

// private definition 
struct dss_file {
    long long ifltab[250];
};

HECDSS_API int hec_dss_test_string_const(const char* s, int size) {

    printf("string='%s'  length=%d\n", s,(int)strlen(s));
    return 0;
}

HECDSS_API int hec_dss_test_string_buffer(char* outStr, int size) {
    char* tmp = "abc";
    strcpy(outStr, tmp);
    return 0;
}

HECDSS_API int hec_dss_test_string_char(char* outStr, int size) {
    char* tmp = "DEF\0";
    
    strcpy(outStr, tmp);
    printf("outStr = '%s'\n", outStr);
    return 0;
}

HECDSS_API dss_file* hec_dss_open(const char* filename)
{
    dss_file *f = (dss_file*)malloc(sizeof(dss_file));
    if (f == 0)
        return 0;

    int status = zopen(f->ifltab,filename);
    return f;
}
HECDSS_API int hec_dss_close(dss_file *dss)
{
    int status = zclose(dss->ifltab);
    return status;
}

HECDSS_API long long* hec_dss_deprecated_ifltab(dss_file* dss) {
    return &dss->ifltab[0];
}

HECDSS_API void hec_dss_deprecated_ifltab_print(long long* ifltab) {
    for (size_t i = 0; i < 10; i++)
    {
        printf("\nhec_dss_deprecated_ifltab_info ifltab[%d] = %ld",(int)i, (int)ifltab[i]);
    }
    
}
// get length of time series
HECDSS_API int hec_dss_tsGetSizes(dss_file* pdss, const char* pathname,
    const char* startDate, const char* startTime,
    const char* endDate, const char* endTime,
    int* numberValues) {
    

    zStructRecordSize* recordSize = zstructRecordSizeNew(pathname);
    zStructTimeSeries* tss = zstructTsNew(pathname);

    tss->startJulianDate = dateToJulian(startDate);
    tss->startTimeSeconds = timeStringToSeconds(startTime);
    tss->endJulianDate = dateToJulian(endDate);
    tss->endTimeSeconds = timeStringToSeconds(endTime);

    int status = ztsGetSizes(pdss->ifltab, tss, recordSize);
    if( status ==0 )
    *numberValues = recordSize->logicalNumberValues;
    

    if( tss)
      zstructFree(tss);
    if( recordSize)
      zstructFree(recordSize);

    return status;

}

HECDSS_API int hec_dss_tsRetrieve(dss_file* pdss, const char *pathname, 
                                  const char *startDate, const char *startTime, 
                                  const char* endDate,   const char *endTime,
                                  int *timeArray, double *valueArray, const int arraySize,
                                  int *numberValuesRead, int* julianBaseDate,
                                  char* units,const int unitsLength, char* type,const int typeLength)
{
    *numberValuesRead = 0; 
    zStructTimeSeries* tss = zstructTsNew(pathname);

    tss->startJulianDate = dateToJulian(startDate);
    tss->startTimeSeconds = timeStringToSeconds(startTime);
    tss->endJulianDate = dateToJulian(endDate);
    tss->endTimeSeconds = timeStringToSeconds(endTime);
    
    int retrieveDoublesFlag = 2; // get doubles
    int boolRetrieveAnyQualityNotes = 1;
    int retrieveUsingTimeWindowFlag = 0; // Adhere to time window provided and generate the time array

    int status = ztsRetrieve(pdss->ifltab, tss, retrieveUsingTimeWindowFlag,retrieveDoublesFlag, boolRetrieveAnyQualityNotes);
    if (status == 0) {
        *numberValuesRead = tss->numberValues;
        *julianBaseDate = tss->julianBaseDate;
        stringCopy(units, unitsLength, tss->units, strlen(tss->units));
        stringCopy(type, typeLength, tss->type, strlen(tss->type));
        for (int i = 0; i < tss->numberValues; i++) {
            timeArray[i] = tss->times[i];
            valueArray[i] = tss->doubleValues[i];
            // to Do quality....
            // tss->quality
        }
    }

    zstructFree(tss);
    return status;
}
