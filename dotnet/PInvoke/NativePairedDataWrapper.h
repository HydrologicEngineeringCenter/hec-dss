#pragma once

#include "DSSGrid.h"
#include "pinvokeTools.h"

extern "C" {
	__declspec(dllexport) double* GetDoubleOrdinates(zStructPairedData* pd);
	__declspec(dllexport) float* GetPdFloatValues(zStructPairedData* pd);
	__declspec(dllexport) int GetNumberCurves(zStructPairedData* pd);
	__declspec(dllexport) int GetNumberOrdinates(zStructPairedData* pd);
	__declspec(dllexport) double* GetPdDoubleValues(zStructPairedData* pd);
	__declspec(dllexport) char* GetLabels(zStructPairedData* pd);
	__declspec(dllexport) int GetLabelsLength(zStructPairedData* pd);
	__declspec(dllexport) BSTR GetTypeDependent(zStructPairedData* pd);
	__declspec(dllexport) BSTR GetTypeIndependent(zStructPairedData* pd);
	__declspec(dllexport) BSTR GetUnitsDependent(zStructPairedData* pd);
	__declspec(dllexport) BSTR GetUnitsIndependent(zStructPairedData* pd);
	__declspec(dllexport) zStructLocation* GetPdLocation(zStructPairedData* pd);
	__declspec(dllexport) int GetPdNumberValues(zStructPairedData* pd);
}