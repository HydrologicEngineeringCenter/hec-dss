#include "managedToUnmanaged.h"

char * managedToUnmanagedCharArr(array<char> ^ arr)
{
	pin_ptr<char> valPinned = &arr[0];
	char * mallocVal = (char *)malloc(sizeof(char) * arr->Length);
	memcpy(mallocVal, valPinned, sizeof(char) * arr->Length);
	return mallocVal;
}

char * managedToUnmanagedString(String ^ str)
{
	IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(str);
	char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
	char * mallocPtr = (char*)malloc(strlen(strPtr) + 1);
	strcpy(mallocPtr, strPtr);
	Marshal::FreeHGlobal(marshallToCharStar);
	return mallocPtr;
}

int * managedToUnmanagedIntArr(array<int> ^ arr)
{
	pin_ptr<int> valPinned = &arr[0];
	int * mallocVal = (int *)malloc(sizeof(int) * arr->Length);
	memcpy(mallocVal, valPinned, sizeof(int) * arr->Length);
	return mallocVal;
}

float * managedToUnmanagedFloatArr(array<float> ^ arr)
{
	pin_ptr<float> valPinned = &arr[0];
	float * mallocVal = (float *)malloc(sizeof(float) * arr->Length);
	memcpy(mallocVal, valPinned, sizeof(float) * arr->Length);
	return mallocVal;
}

double * managedToUnmanagedDoubleArr(array<double> ^ arr)
{
	pin_ptr<double> valPinned = &arr[0];
	double * mallocVal = (double *)malloc(sizeof(double) * arr->Length);
	memcpy(mallocVal, valPinned, sizeof(double) * arr->Length);
	return mallocVal;
}