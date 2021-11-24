#include "pch.h"
#include "NativePairedDataWrapper.h"

double* GetDoubleOrdinates(zStructPairedData* pd)
{
    return pd->doubleOrdinates;
}

float* GetFloatValues(zStructPairedData* pd)
{
    return pd->floatValues;
}

int GetNumberCurves(zStructPairedData* pd)
{
    return pd->numberCurves;
}

int GetNumberOrdinates(zStructPairedData* pd)
{
    return pd->numberOrdinates;
}

double* GetPdDoubleValues(zStructPairedData* pd)
{
    return pd->doubleValues;
}

char* GetLabels(zStructPairedData* pd)
{
    return pd->labels;
}

int GetLabelsLength(zStructPairedData* pd)
{
    return pd->labelsLength;
}

BSTR GetTypeDependent(zStructPairedData* pd)
{
    return ANSItoBSTR(pd->typeDependent);
}

BSTR GetTypeIndependent(zStructPairedData* pd)
{
    return ANSItoBSTR(pd->typeIndependent);
}

BSTR GetUnitsDependent(zStructPairedData* pd)
{
    return ANSItoBSTR(pd->unitsDependent);
}

BSTR GetUnitsIndependent(zStructPairedData* pd)
{
    return ANSItoBSTR(pd->unitsIndependent);
}

zStructLocation* GetPdLocation(zStructPairedData* pd)
{
    return pd->locationStruct;
}

int GetPdNumberValues(zStructPairedData* pd)
{
    return pd->numberOrdinatesInStruct * pd->numberCurves;
}
