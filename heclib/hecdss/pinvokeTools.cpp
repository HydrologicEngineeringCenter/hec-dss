#include "pch.h"
#include "DSSGrid.h"
#include "pinvokeTools.h"

BSTR ANSItoBSTR(const char* input) {
    BSTR result = NULL;
    int lenA = lstrlenA(input);
    int lenW = MultiByteToWideChar(CP_ACP, 0, input, lenA, NULL, 0);
    if (lenW > 0)
    {
        result = SysAllocStringLen(0, lenW);
        MultiByteToWideChar(CP_ACP, 0, input, lenA, result, lenW);
    }
    return result;
}

char* getStringFromHeader(int* header)
{
	int start = 1;
	int start2 = 1;
	int max_len = 8;
	char buffer[10];
	memset(buffer, '\0', 10);
	holchr_(header, &start, &max_len, buffer, &start2, sizeof(buffer) - 1);
	return mallocAndCopyTrim(buffer);
}