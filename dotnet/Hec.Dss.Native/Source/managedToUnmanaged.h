#pragma once
#include <stdio.h>
#include <string.h>
#include <string>
extern "C"
{
#include "heclib.h"
#include "zStructSpatialGrid.h"
}

#include "DSSGrid.h"

using namespace System;
using namespace System::Runtime::InteropServices;
using namespace System::Diagnostics;

char * managedToUnmanagedCharArr(array<char> ^ arr);
char * managedToUnmanagedString(String ^ str);
int * managedToUnmanagedIntArr(array<int> ^ arr);
float * managedToUnmanagedFloatArr(array<float> ^ arr);
double * managedToUnmanagedDoubleArr(array<double> ^ arr);