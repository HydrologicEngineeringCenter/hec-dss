#include <string.h>
#include "hecdss.h"
#include "heclib.h"
// public definition
typedef struct dss_file dss_file;

struct dss_file {
    long long ifltab[250];
};


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