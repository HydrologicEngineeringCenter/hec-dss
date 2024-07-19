
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "heclib.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdss7.h"
#include "hecdssInternal.h"

int zinquireChar(long long *ifltab, const char *request, char *creturn, size_t creturnSize, int *number)
{
	int len;
	char requestlc[5];
	char ctemp[50];
	int i;
	const char *ctype;
	long long *info;
	int numberInfo;
	int boolPathIncreturn;
	int dummy;
	int status;


	if (!request) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zinquire_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "request is null");
	}

	if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	 }
	len = (int)strlen(request);
	if (len > sizeof(requestlc)-1) {
		len = sizeof(requestlc)-1;
	}
	for (i=0; i<len; i++) {
		requestlc[i] = tolower(request[i]);
	}
	requestlc[len] = '\0';

	boolPathIncreturn = 0;
	if ((creturnSize > 0) && (creturn)) {
		if (creturn[0] == '/') {
			boolPathIncreturn = 1;
		}
		else {
			//  "blank" out return string
			creturn[0] = '\0';
		}
	}


	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zinquire_ID, "Request: ", requestlc);
	}


	if (!strcmp(requestlc, "erro")) {
		if ((zgetVersion(ifltab) == 7) && (ifltab[zdssKeys.kerrorCode] != 0)) {
			number[0] = (int)ifltab[zdssKeys.kerrorCode];
			if (ifltab[zdssKeys.kerrorCondition] != 0) {
				stringCopy(creturn, (size_t)creturnSize, zdssVals.globalErrorMess, strlen(zdssVals.globalErrorMess));
			}
		}
		else {
			number[0] = zdssVals.globalErrorFlag;
			stringCopy(creturn, (size_t)creturnSize, zdssVals.globalErrorMess, strlen(zdssVals.globalErrorMess));
		}
	}
	else  if (!strcmp(requestlc, "fver")) {
		long long* fileHeader = (long long*)ifltab[zdssKeys.kfileHeader];
		charInt((void *)&fileHeader[zdssFileKeys.kversion], ctemp, 4, sizeof(ctemp), 0, 1, 0);
		ctemp[4] = '\0';
		stringCopy(creturn, (size_t)creturnSize, ctemp, (size_t)5);
		number[0] = 10000 * (ctemp[0] - '0') +
		              100 * (ctemp[2] - (ctemp[2] < '[' ? '@' : '`')) +
		                    (ctemp[3] - (ctemp[3] < '[' ? '@' : '`'));
	}
	else if (!strcmp(requestlc, "name")) {
		charLong((void *)ifltab[zdssKeys.kfullFilename], creturn, 0, (int)creturnSize, 0, 1);
	}

	else if (!strcmp(requestlc, "read")) {
		if (ifltab[zdssKeys.kopenStatus] == OPEN_STAT_READ_ONLY) {
			number[0] = 1;
			stringCopy(creturn, (size_t)creturnSize, "ON", _TRUNCATE);
		}
		else {
			number[0] = 0;
			stringCopy(creturn, (size_t)creturnSize, "OFF", _TRUNCATE);
		}
	}

	else if (!strcmp(requestlc, "vers")) {
		//  Software version
			number[0] = 7;
			stringCopy(creturn, (size_t)creturnSize, zdssVals.czVersion, strlen(zdssVals.czVersion));
	}
	else if (!strcmp(requestlc, "rver")) {
		//  Record version
		if (boolPathIncreturn) {
			info = (long long *)calloc(zdssVals.maxInfoSize, 8);
			if (!info) return -1;
			status = zreadInfoBlock(ifltab, (const char*)creturn, 0, info, zdssVals.maxInfoSize, &numberInfo);
			if (status == STATUS_OKAY) {
				i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &dummy, number);
			}
			else {
				number[0] = status;
			}
			free(info);
		}
		else {
			info = (long long *)ifltab[zdssKeys.kinfo];
			i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &dummy, number);
		}
	}
	else if (!strcmp(requestlc, "type")) {
		if (boolPathIncreturn) {
			status = zcheckInternal(ifltab, (const char*)creturn, 0);
			if (status == STATUS_OKAY) {
				number[0] = (int)ifltab[zdssKeys.klastType];
			}
			else {
				number[0] = status;
			}
		}
		else {
			number[0] = (int)ifltab[zdssKeys.klastType];
			ctype = ztypeName(number[0], 1);
			stringCopy(creturn, (size_t)creturnSize, ctype, strlen(ctype));
		}
	}
	else if (!strncmp(requestlc, "prog", 4)) {
		stringCopy(creturn, (size_t)creturnSize, zdssVals.cprogramName, strlen(zdssVals.cprogramName));
	}
	else if (!strncmp(requestlc, "vdtm", 4)) {
		number[0] = zdssVals.iverticalDatum;
		stringCopy(creturn, (size_t)creturnSize, zdssVals.cverticalDatum, strlen(zdssVals.cverticalDatum));
	}
	else if (!strncmp(requestlc, "vdow", 4)) {
		number[0] = zdssVals.icanOverwriteLocationVerticalDatum;
	}

	else {
		number[0] = (int)zinquire(ifltab, requestlc);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zinquire_ID, "creturn: ", creturn);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zinquire_ID, "number: ", number[0]);
	}

	return number[0];

}

int zinquirechar_ (long long *ifltab, const char *parameter, char *charVal, int *lenCharValIn, int *integerVal,
					size_t lenParameter, size_t lenCharVal2)
{
	int istat;
	int lenCharVal;
	char *param;
	char *cval;

	lenCharVal = *lenCharValIn;
	param = stringFortToC(parameter, lenParameter);
	cval = (char *)calloc((size_t)lenCharVal + 1, CHAR_SIZE);

	istat = zinquireChar(ifltab, parameter, cval, (size_t)lenCharVal, integerVal);
	if (istat == -1) {
		*integerVal = -1;
		if (lenCharVal > 1) {
			stringFill(charVal, ' ', lenCharVal);
		}
	}
	else {
		stringCToFort(charVal, (size_t)lenCharVal,  cval);
	}
	free(param);
	free(cval);
	return istat;
}
