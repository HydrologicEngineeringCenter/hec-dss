#include <string.h>
#include "hecdss.h"
#include "heclib.h"
// public definition
typedef struct dss_file dss_file;

#include "zdssKeys.h"

/*
 hecdss.c contains code for a shared object/dll, providing an API to work with DSS files.

 Only DSS version 7 files are supported.  DSS version 6 files can be converted using HEC-DSSVue
 https://www.hec.usace.army.mil/software/hec-dssvue/


 This API is designed with perspective that the calling/client code is in charge of managing memory.
 The only exception is hec_dss_open(const char* filename, dss_file** dss).   hec_dss_open allocates 
 one internal structure that must be freed by calling hec_dss_close;

 For reading data: The client passes in pre-allocated arrays, with the size, then the API copies data 
 into those arrays
 

*/

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
}

HECDSS_API int hec_dss_CONSTANT_MAX_PATH_SIZE() {
  return MAX_PATHNAME_SIZE;
}

HECDSS_API int hec_dss_log_error(const char* message) {
  printf("\nError %s", message);
  return 0;
}
HECDSS_API int hec_dss_log_warning(const char* message) {
  printf("\nWarning: %s", message);
  return 0;
}

HECDSS_API int hec_dss_open(const char* filename, dss_file** dss)
{
    dss_file* f = (dss_file*)malloc(sizeof(dss_file));
    if (f == 0)
        return -1;
    
    int status = zopen(f->ifltab,filename);
    return f;
}
HECDSS_API int hec_dss_close(dss_file *dss)
{
    int status = zclose(dss->ifltab);
    return status;
}

// get length of time series
HECDSS_API int hec_dss_tsGetSizes(dss_file* pdss, const char* pathname,
    const char* startDate, const char* startTime,
    const char* endDate, const char* endTime ) {

    zStructRecordSize* recordSize = zstructRecordSizeNew(pathname);
    zStructTimeSeries* tss = zstructTsNew(pathname);

    tss->startJulianDate = dateToJulian(startDate);
    tss->startTimeSeconds = timeStringToSeconds(startTime);
    tss->endJulianDate = dateToJulian(endDate);
    tss->endTimeSeconds = timeStringToSeconds(endTime);

    ztsGetSizes(pdss->ifltab, tss, recordSize);
    int rval = recordSize->numberValues;

    if( tss)
      zstructFree(tss);
    if( recordSize)
      zstructFree(recordSize);

    return rval;

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
