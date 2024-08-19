#pragma once
#include <stdio.h>
// some checks used by testing Code C and Fortran
// converted from checks.f

void checkdoubles_(double* dataOrig, double* dataRead, int* number, char* mess, int* status, size_t dummy);
void checkfloats_(float* dataOrig, float* dataRead, int* number, char* mess, int* status, size_t dummy);
void checknumbers_(int* numberOrig, int* numberRead, const char* mess, int* status, size_t dummy);
void checkints_(int* dataOrig, int* dataRead, int* length, int* number, char* mess, int* status, size_t mess_len);