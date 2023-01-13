#include <string.h>
#include "hecdss.h"
#include "heclib.h"

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

void hec_dss_array_copy(double* destination, const long destinationSize,
  double* source, const size_t sourceSize) {
  size_t numberToCopy = sourceSize < destinationSize ? sourceSize: destinationSize;

  for (size_t i = 0; i < numberToCopy; i++)
  {
    destination[i] = source[i];
  }

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
    
    int status = hec_dss_zopen(f->ifltab,filename);
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

HECDSS_API int hec_dss_record_count(dss_file* dss) {
    if (!dss)
        return 0;

    long long nrec = zinquire(dss->ifltab, "nrec");// does this include aliases?
    
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
    recordTypes[i] = catStruct->recordType[i];
    char* s = pathBuffer + i * pathBufferItemSize;
    stringCopy(s, pathBufferItemSize, catStruct->pathnameList[i], strlen(catStruct->pathnameList[i]));
  }
  zstructFree(catStruct);
  return maxPaths;
}

HECDSS_API int  hec_dss_tsGetDateTimeRange(dss_file* dss, const char* pathname, const int boolFullSet,
                                            int* firstValidJulian, int* firstSeconds,
                                            int* lastValidJulian, int* lastSeconds) {

  int status = ztsGetDateTimeRange(dss->ifltab, pathname, boolFullSet, 
                                  firstValidJulian, firstSeconds, 
                                  lastValidJulian, lastSeconds);

  return status;
}

HECDSS_API int hec_dss_tsGetSizes(dss_file* dss, const char* pathname,
    const char* startDate, const char* startTime,
    const char* endDate, const char* endTime,
    int* numberValues) {
  //?? int ztsGetDateTimeRange(long long *ifltab, const char *pathname, int boolFullSet, *int * firstValidJulian, int* firstSeconds,*int * lastValidJulian, int* lastSeconds);
  
    zStructRecordSize* recordSize = zstructRecordSizeNew(pathname);
    zStructTimeSeries* tss = zstructTsNew(pathname);

    tss->startJulianDate = dateToJulian(startDate);
    tss->startTimeSeconds = timeStringToSeconds(startTime);
    tss->endJulianDate = dateToJulian(endDate);
    tss->endTimeSeconds = timeStringToSeconds(endTime);

    ztsProcessTimes(dss->ifltab, tss, 1);
    int status = ztsGetSizes(dss->ifltab, tss, recordSize);
    if( status ==0 )
    *numberValues = recordSize->logicalNumberValues;
    

    if( tss)
      zstructFree(tss);
    if( recordSize)
      zstructFree(recordSize);

    return status;

}

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


HECDSS_API int hec_dss_tsRetrieve(dss_file* dss, const char *pathname, 
                                  const char *startDate, const char *startTime, 
                                  const char* endDate,   const char *endTime,
                                  int *timeArray, double *valueArray, const int arraySize,
                                  int *numberValuesRead, int* julianBaseDate,int* timeGranularitySeconds,
                                  char* units,const int unitsLength, char* type,const int typeLength)
{
    *numberValuesRead = 0; 
    zStructTimeSeries* tss = zstructTsNew(pathname);

    tss->startJulianDate = dateToJulian(startDate);
    tss->startTimeSeconds = timeStringToSeconds(startTime);
    tss->endJulianDate = dateToJulian(endDate);
    tss->endTimeSeconds = timeStringToSeconds(endTime);
    
    const int retrieveDoublesFlag = 2; // get doubles
    const int boolRetrieveAnyQualityNotes = 1;
    /*int retrieveFlag
      * A flag indicating how to readand trim data.
      * For regular interval data :
    *retrieveFlag = 0 : Adhere to time window provided and generate the time array.
      * retrieveFlag = -1 : Trim data.Remove missing values at the beginning and end of data set(not inside),
      *andgenerate the time array.
      * retrieveFlag = -2 : Adhere to time window provided but do not include time array.
      * retrieveFlag = -3 : Trim data.Remove missing values at the beginning and end of data set(not inside),
      * no time array.
      * For irregular interval data :
    *retrieveFlag = 0 : Adhere to time window provided.
      * retrieveFlag = 1 : Retrieve one value previous to start of time window
      * retrieveFlag = 2 : Retrieve one value after end of time window
      * retrieveFlag = 3 : Retrieve one value before and one value after time window
      */
    const int retrieveUsingTimeWindowFlag = 0; // Adhere to time window provided and generate the time array

    int status = ztsRetrieve(dss->ifltab, tss, retrieveUsingTimeWindowFlag,retrieveDoublesFlag, boolRetrieveAnyQualityNotes);
    if (status == 0) {
        *julianBaseDate = tss->julianBaseDate;
        *timeGranularitySeconds = tss->timeGranularitySeconds;
        
        stringCopy(units, unitsLength, tss->units, strlen(tss->units));
        stringCopy(type, typeLength, tss->type, strlen(tss->type));
        int size = min(tss->numberValues, arraySize);
        size = max(0, size);
        *numberValuesRead = size;
        for (int i = 0; i < size; i++) {
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
  zstructFree(loc);
  return status;
 
}
/// <summary>
/// hec_dss_pdRetrieveInfo is used by client app to determine
/// required array sizes for calling hec_dss_pdRetrieve.
/// 
/// </summary>
/// <param name="dss">pointer to dss_file</param>
/// <param name="pathname">path of paired data</param>
/// <param name="numberOrdinates">number of rows for ordinates, and curves</param>
/// <param name="numberCurves">number of columns in the curve dataset</param>
/// <returns></returns>
HECDSS_API int hec_dss_pdRetrieveInfo(dss_file* dss, const char* pathname,
                                    int* numberOrdinates, int* numberCurves, 
                                    char* unitsIndependent, const int unitsIndependentLength,
                                    char* unitsDependent, const int unitsDependentLength,
                                    char* typeIndependent, const int typeIndependentLength,
                                    char* typeDependent, const int typeDependentLength,
                                    int* labelsLength){
  zStructPairedData* pds = zstructPdNew(pathname);
  pds->endingOrdinate = 1;// hack to retrive meta data, with minimal actual data.
  int returnDoubles = 2;
  int status = zpdRetrieve(dss->ifltab, pds, returnDoubles);

  *numberOrdinates = pds->numberOrdinates;
  *numberCurves = pds->numberCurves;
  if (pds->unitsIndependent)
    stringCopy(unitsIndependent, unitsIndependentLength, pds->unitsIndependent, strlen(pds->unitsIndependent));
  if (pds->unitsDependent)
    stringCopy(unitsDependent, unitsDependentLength, pds->unitsDependent, strlen(pds->unitsDependent));
  if (pds->typeIndependent)
    stringCopy(typeIndependent, typeIndependentLength, pds->typeIndependent, strlen(pds->typeIndependent));
  if (pds->typeDependent)
    stringCopy(typeDependent, typeDependentLength, pds->typeDependent, strlen(pds->typeDependent));
  if (pds->labels)
    *labelsLength = pds->labelsLength;

  zstructFree(pds);
  return status;

}

HECDSS_API int hec_dss_dataType(dss_file* dss, const char* pathname) {

  return zdataType(dss->ifltab, pathname);
}


HECDSS_API int hec_dss_pdRetrieve(dss_file* dss, const char* pathname,
  double* doubleOrdinates, const int  doubleOrdinatesLength,
  double* doubleValues, const int doubleValuesLength,
  int* numberOrdinates, int* numberCurves,
  char* unitsIndependent, const int unitsIndependentLength,
  char* typeIndependent, const int typeIndependentLength,
  char* unitsDependent, const int unitsDependentLength,
  char* typeDependent, const int typeDependentLength,
  char* labels, const int labelsLength)
{
  zStructPairedData* pds = zstructPdNew(pathname);
  int returnDoubles = 2;
  int status = zpdRetrieve(dss->ifltab, pds, returnDoubles);

  if (pds->numberOrdinates != doubleOrdinatesLength && status == 0)
  {
    hec_dss_log_error("in hec_dss_pdRetrieve the doubleOrdinatesLength argument does not match what was read from disk.");
    status = -1;
  }
  if (pds->numberCurves * pds->numberOrdinates != doubleValuesLength && status == 0)
  {
    hec_dss_log_error("in hec_dss_pdRetrieve the doubleValuesLength argument does not match what was read from disk.");
    status = -1;
  }

  if (status == 0) {
  
    *numberOrdinates = pds->numberOrdinates;
    *numberCurves = pds->numberCurves;
    /// -- leaving these meta-data below out for initial version.
    //*boolIndependentIsXaxis = pds->boolIndependentIsXaxis; 
    //*xprecision = pds->xprecision;
    // *yprecision = pds->yprecision;


    if (pds->unitsIndependent)
      stringCopy(unitsIndependent, unitsIndependentLength, pds->unitsIndependent, strlen(pds->unitsIndependent));
    if (pds->unitsDependent)
      stringCopy(unitsDependent, unitsDependentLength, pds->unitsDependent, strlen(pds->unitsDependent));
    if (pds->typeIndependent)
      stringCopy(typeIndependent, typeIndependentLength, pds->typeIndependent, strlen(pds->typeIndependent));
    if (pds->typeDependent)
      stringCopy(typeDependent, typeDependentLength, pds->typeDependent, strlen(pds->typeDependent));

    hec_dss_array_copy(doubleOrdinates, doubleOrdinatesLength, pds->doubleOrdinates, pds->numberOrdinates);
    hec_dss_array_copy(doubleValues, doubleValuesLength, pds->doubleValues, pds->numberOrdinates* pds->numberCurves);

  }


    zstructFree(pds);
    return status;
}


HECDSS_API int hec_dss_dateToYearMonthDay(const char* date,int*year, int* month, int* day) {
  return dateToYearMonthDay(date, year, month, day);
}

HECDSS_API int hec_dss_dateToJulian(const char* date){
  return dateToJulian(date);
}

HECDSS_API void hec_dss_julianToYearMonthDay(const int julian, int* year, int* month,int* day){

  julianToYearMonthDay(julian, year, month, day);
}