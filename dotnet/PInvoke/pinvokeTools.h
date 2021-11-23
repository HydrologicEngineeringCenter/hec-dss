#pragma once
#include <wtypes.h>
#include <stdio.h>
#include <OleAuto.h>


BSTR ANSItoBSTR(const char* input);
char* getStringFromHeader(int* header);