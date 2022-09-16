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

HECDSS_API int hec_dss_test_modify_arg(int *value) {

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

HECDSS_API int hec_dss_tsRetrieve(dss_file* pdss, const char *pathname, 
                                  const char *startDateTime, const char* endDateTime,
                                  int *timeArray, double *valueArray, const int arraySize,
                                  int *numberValuesRead, int* julianBaseDate,
                                  char* units, char* type)
{
    zStructTimeSeries* tss;
    tss = zstructTsNew(pathname);
    printf("\npathname='%s'", pathname);
    
   //tss->startJulianDate = *startJulian;
   //int tss->startTimeSeconds = *startTimeMinutes * 60;
   //int tss->startJulianDate = *endJulian;
   //int tss->startTimeSeconds = *endTimeMinutes * 60;
   //int tss->numberValues = *maxNumberValuestatus;
    
    int status = ztsRetrieve(pdss->ifltab, tss, 1, 1, 0);
    *numberValuesRead = tss->numberValues;
    *julianBaseDate = tss->julianBaseDate;
//    strncpy(units,tss->units, 29);
 //   strncpy(type, tss->type, 29);
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
                                    char* typeDependent, const int typeDependentLength){
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



  zstructFree(pds);
  return status;

}

HECDSS_API int hec_dss_dataType(dss_file* dss, const char* pathname) {

  return zdataType(dss->ifltab, pathname);
}


HECDSS_API int hec_dss_pdRetrieve(dss_file* dss, const char* pathname,
  double* doubleOrdinates, double* valdoubleValues, const int arraySize,
  int* numberOrdinates, int* numberCurves,
  char* unitsIndependent, const int unitsIndependentLength,
  char* typeIndependent, const int typeIndependentLength,
  char* unitsDependent, const int unitsDependentLength,
  char* typeDependent, const int typeDependentLength,
  int* boolIndependentIsXaxis,
  int* precision,
  char* labels, const int labelsLength)
{
  zStructPairedData* pds = zstructPdNew(pathname);
  int returnDoubles = 2;
  int status = zpdRetrieve(dss->ifltab, pds, returnDoubles);

    zstructFree(pds);
    return status;
}
/*
void zrits7_(long long* ifltab, const char* path,
    int* startJulian, int* startTimeMinutes,
    int* endJulian, int* endTimeMinutes,
    int* timeArray, float* values,
    int* maxNumberValues, int* numberValuesRead,
    int* julianBaseDate,
    char* units, char* type, int* status,
    size_t pathLen, size_t unitsLen, size_t typeLen)
    */