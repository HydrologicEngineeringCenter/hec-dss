#include <string.h>
#include "hecdss.h"
#include "heclib.h"

#include "zdssKeys.h"

/*
 hecdss.c contains code for a shared object/dll, providing an API to work with DSS files.

 This API is designed with perspective that the calling/client code is in charge of managing memory.
 The only exception is hec_dss_open(const char* filename, dss_file** dss).   hec_dss_open allocates 
 one internal structure that must be freed by calling hec_dss_close;

 For reading data: The client passes in pre-allocated arrays, with the size, then the API copies data 
 into those arrays
 

*/
// public declaration
typedef struct dss_file dss_file;

// private definition 
struct dss_file {
    long long ifltab[250];
};

HECDSS_API int hec_dss_test_modify_arg(int *value) {

    *value = 123;
    return 0;
}
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

HECDSS_API int hec_dss_test_list_of_string(char* data, int rows, int columns) {

  char buff[1024];
  for (int i = 0; i < rows; i++)
  {
    sprintf(buff, "/Basin/River/Flow//1Day/item %d", i);
    printf("buff='%s'", buff);
    char* s = data+i*columns;
    stringCopy(s, columns, buff, strlen(buff));
    printf("s='%s'", s);
  }
  
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
    if (status != 0)
      return status;
    int version = zgetVersion(f->ifltab);
    if (version != 7) {
        hec_dss_log_error("version %d is not supported.\nOnly version 7 DSS files are supported");
        zclose(f->ifltab);
        return -700;
    }
    *dss = f;
    return status;
}

HECDSS_API int hec_dss_close(dss_file *dss){
    int status = zclose(dss->ifltab);
    free(dss);
    dss = 0;
    return status;
}

HECDSS_API int hec_dss_version(dss_file* dss) {
    if (!dss)
        return 0;
    return zgetVersion(dss->ifltab);
}

/// <summary>
/// Returns number of records (includes aliases)
/// </summary>
/// <param name="dss"></param>
/// <returns>number of records</returns>
HECDSS_API int hec_dss_record_count(dss_file* dss) {
    if (!dss)
        return 0;

    long long nrec = zinquire(dss->ifltab, "nrec");
    
    return (int)nrec;
    }


/// <summary>
/// Used to read the catalog of a DSS file
/// </summary>
/// <param name="dss">pointer to DSS file</param>
/// <param name="pathBuffer">allocated buffer that is loaded with pathnames </param>
/// <param name="recordTypes">output array of record types corresponding to each path</param>
/// <param name="pathFilter">	Either null (for ignore) or a String that represents a pathname with wild characters represented
///  by a star(*) to match any string in the pathname part.Wild characters can only be at the beginning or end of a part,
///  not inside of a string.An example is a C part with "*Flow*", which
///  will match all pathnames that have "Flow" anywhere in the C part, such as "Flow", "Inflow", "Outflow-Reg", etc.
///  A part such as "Flow*Reg" is not supported. A null(//) will only match a null, where only a star (*) will match all. </param>
/// <param name="count">number of paths that can be stored in pathBuffer, and length of recordTypes array</param>
/// <param name="pathBufferItemSize">max allowable length of each pathname</param>
/// <returns></returns>
HECDSS_API int hec_dss_catalog(dss_file* dss, char* pathBuffer, int* recordTypes, const char* pathFilter,
                              const int count, const int pathBufferItemSize) {
 
  zStructCatalog* catStruct = zstructCatalogNew();
  int sorted = 0; // don't sort
  int status = zcatalog(dss->ifltab,pathFilter, catStruct, sorted);
  if (status < 0) {
    printf("Error during catalog.  Error code %d\n", status);
    return status;
  }
  int maxPaths = catStruct->numberPathnames > count ? count : catStruct->numberPathnames;
  for (int i = 0; i < maxPaths; i++)
  {
    if( catStruct->recordType) // for DSS6 this is null
      recordTypes[i] = catStruct->recordType[i];
    char* s = pathBuffer + i * pathBufferItemSize;
    stringCopy(s, pathBufferItemSize, catStruct->pathnameList[i], strlen(catStruct->pathnameList[i]));
  }
  zstructFree(catStruct);
  return maxPaths;
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
/// <summary>
/// Retrive an empty time series.
/// Used to get units and type for a time series
/// </summary>
/// <param name="pdss"></param>
/// <param name="pathname"></param>
/// <param name="units"></param>
/// <param name="unitsLength"></param>
/// <param name="type"></param>
/// <param name="typeLength"></param>
/// <returns></returns>
HECDSS_API int hec_dss_tsRetrieveInfo(dss_file* pdss, const char* pathname,char* units, 
                                       const int unitsLength, char* type, const int typeLength) {

    zStructTransfer* transfer = zstructTransferNew(pathname, 0);
    zStructTimeSeries* tss = zstructTsNew(pathname);
    transfer->internalHeaderMode = 1;
    int status = zread(pdss->ifltab, transfer);
    if (status == 0)
    {
        int intervalType = ztsProcessTimes(pdss->ifltab, tss, 0);
        status = ztsInternalHeaderUnpack(tss, transfer->internalHeader, transfer->internalHeaderNumber);
    }
    stringCopy(units, unitsLength, tss->units, strlen(tss->units));
    stringCopy(type, typeLength, tss->type, strlen(tss->type));
    
    zstructFree(tss);
    zstructFree(transfer);
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
HECDSS_API int hec_dss_locationRetrieve(dss_file* dss, const char* fullPath, 
                            double* x,double* y, double* z,
                            int* coordinateSystem, int* coordinateID, 
                            int* horizontalUnits,int* horizontalDatum,
                            int* verticalUnits, int* verticalDatum,
                            char* timeZoneName, const int timeZoneNameLength,
                            char* supplemental, const int supplementalLength){

  zStructLocation* loc = zstructLocationNew(fullPath);
  int status = zlocationRetrieve(dss->ifltab, loc);
  if (status == 0) {
    *x = loc->xOrdinate;
    *y = loc->yOrdinate;
    *z = loc->zOrdinate;
    *coordinateSystem = loc->coordinateSystem;
    *coordinateID = loc->coordinateID;
    *verticalUnits = loc->verticalUnits;
    *verticalDatum = loc->verticalDatum;

    if(loc->timeZoneName)
      stringCopy(timeZoneName, timeZoneNameLength, loc->timeZoneName, strlen(loc->timeZoneName));
    if( loc->supplemental)
      stringCopy(supplemental, supplementalLength, loc->supplemental, strlen(loc->supplemental));
  }
  return status;
  zstructFree(loc);
}

HECDSS_API int hec_dss_dateToYearMonthDay(const char* date,int*year, int* month, int* day) {
  return dateToYearMonthDay(date, year, month, day);
}

HECDSS_API int hec_dss_dateToJulian(const char* date){
  return dateToJulian(date);
}
