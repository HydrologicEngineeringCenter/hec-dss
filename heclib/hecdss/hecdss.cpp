// hecdss.cpp : Defines the exported functions for the DLL.
//

#include "pch.h"
#include "framework.h"
#include "hecdss.h"


#pragma region exampleRegion



// This is an example of an exported variable
HECDSS_API int nhecdss=0;

// This is an example of an exported function.
HECDSS_API int fnhecdss(void)
{
    return 0;
}

// This is the constructor of a class that has been exported.
Checdss::Checdss()
{
    return;
}
#pragma endregion

// public definition
typedef struct dss_file dss_file;

struct dss_file {
    long long ifltab[250];
};


HECDSS_API int hec_dss_open(dss_file** pdss)
{//	status = ztsRetrieve((long long*)ifltab, tss, retrieveFlag, retrieveDoublesFlag, 1);
    struct dss_file f1;
    *pdss = &f1;
    return 0;
}

HECDSS_API int hec_dss_ztsRetrieve(dss_file** pdss, zStructTimeSeries* tss)
{//	status = ztsRetrieve((long long*)ifltab, tss, retrieveFlag, retrieveDoublesFlag, 1);
    struct dss_file f1;
    *pdss = &f1;
    return 0;
}