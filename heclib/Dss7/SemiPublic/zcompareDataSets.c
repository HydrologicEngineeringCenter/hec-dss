#include <string.h>
#include <stdio.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"

/**
*  Function:	zcompareDataSets
*
*  Use:			semi-private
*
*  Description:	A set of compare functions primarily used for unit testing.  These functions are not accessed during normal use.
*
*  Declaration: int zcompareDataSets(long long *ifltab, void *struct1, void *struct2, int verboseLevel, int boolExact, const char *message);
*
*  Parameters:	long long *ifltab:  the ifltab, used for messaging.  If a DSS file is not opened, make this an array with
*									0 (zero) as the first element.  The rest will not be used.
*
*				void *struct1:		The first struct (original) to compare.  This has to be one of the primary structs, such as zStructTransfer.
*
*				void *struct2:		The second struct (copy) to compare.
*
*				verboseLevel:
* 					0:  None, just returns status flag
*					1:  Normal.  Prints when test fails + message passed in
*					2:  Verbose. Prints when test passed + message passed in
*
*				int boolExact:
*					A boolean flag to indicate if everything should match, including last write time, program name, etc.
*					Usually set to false (0) for normal data sets.
*
*				const char *message
*					The message to print on failure
*
*  Returns:		int statis (boolean):
*					0 (zero) if the struct match
*					1 (one)  if the struct are not the same.
*
*  Remarks:		 Does not work for anything except standard DSS structs
*
*
*  Deviates:
*		int zcheckStatus(long long *ifltab, int status, int verbose, const char *message);
*		int zcompareTimeArrays(long long *ifltab, zStructTimeSeries *struct1, zStructTimeSeries *struct2, int verboseLevel, const char *message);
*		int zcompareInts(long long *ifltab, int number1, int number2, int verbose, const char *message);
*		int zcompareStrings(long long *ifltab, const char *string1, const char *string2, int totalLength, int boolIgnoreCase, int verbose, const char *message);
*		int zcompareLongs(long long *ifltab, long long number1, long long number2, int verbose, const char *message);
*		int zcompareFloats(long long *ifltab, float number1, float number2, int verbose, const char *message);
*		int zcompareDoubles(long long *ifltab, double number1, double number2, int verbose, const char *message);
*		int zcompareIntArrays(long long *ifltab, int *array1, int *array2, int numberArray, int verbose, const char *message);
*		int zcompareFloatArrays(long long *ifltab, float *array1, float *array2, int numberArray, int verbose, const char *message);
*		int zcompareDoubleArrays(long long *ifltab, double *array1, double *array2, int numberArray, int verbose, const char *message);
*		int zcompareStringArrays(long long *ifltab, const char *string1, const char *string2, int totalBytes, int boolIgnoreCase, int verbose, const char *message);
*		void zprintFailMessage(long long *ifltab, int verboseLevel, const char *name, const char *pathname, const char *message);
*			Private - use zcompareDataSets instead
*		int zcompare_ztransferStruct(long long *ifltab, zStructTransfer *struct1, zStructTransfer *struct2, int verboseLevel, int boolExact, const char *message);
*		int zcompare_zStructRecordSize(long long *ifltab, zStructRecordSize *struct1, zStructRecordSize *struct2, int verboseLevel, const char *message);
*		int zcompare_zStructTsRecordSizes(long long *ifltab, zStructRecordSize *struct1, zStructRecordSize *struct2, int verboseLevel, const char *message);
*		int zcompare_zStructRecordInfo(long long *ifltab, zStructRecordAddresses *struct1, zStructRecordAddresses *struct2, int verboseLevel, const char *message);
*		int zcompare_zStructLocation(long long *ifltab, zStructLocation *struct1, zStructLocation *struct2, int verboseLevel, const char *message);
*		int zcompare_zStructTimeSeries(long long *ifltab, zStructTimeSeries *struct1, zStructTimeSeries *struct2, int verboseLevel, int boolExact, const char *message);
*		int zcompare_zStructArray(long long *ifltab, zStructArray *struct1, zStructArray *struct2, int verboseLevel, int boolExact, const char *message);

*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zcompareDataSets(long long *ifltab, void *struct1, void *struct2, int verboseLevel, int boolExact, const char *pathname, const char *message)
{

	int dataType = -1;
	int structType;

	if (!struct1) {
		if (verboseLevel > 0) {
			zmessage2(ifltab, "\nFirst struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!struct2) {
		if (verboseLevel > 0) {
			zmessage2(ifltab, "\nSecond struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}

	structType = zstructGetType(struct1);


	if (structType == STRUCT_TYPE_TRANSFER) {
		//  zStructTransfer
		return zcompare_ztransferStruct(ifltab, (zStructTransfer *)struct1, (zStructTransfer *)struct2, verboseLevel, boolExact, message);
	}
	else if (structType == STRUCT_TYPE_RECORD_SIZES) {
		//  zStructRecordSize
		return zcompare_zStructRecordSize(ifltab, (zStructRecordSize *)struct1, (zStructRecordSize *)struct2, verboseLevel, message);
	}
	else if (structType == STRUCT_TYPE_RECORD_BASICS) {
		//return zcompare_zStructRecordSize(ifltab, (zStructRecordSize *)struct1, (zStructRecordSize *)struct2, verboseLevel, message);
	}
	else if (structType == STRUCT_TYPE_RECORD_ADDRESSES) {
		//  zStructRecordAddresses
		return zcompare_zStructRecordAddresses(ifltab, (zStructRecordAddresses *)struct1, (zStructRecordAddresses *)struct2, verboseLevel, pathname, message);
	}
	else if (structType == STRUCT_TYPE_CATALOG) {

	}
	else if (structType == DATA_TYPE_LOCATION) {
		//  zStructLocation   Location struct
		return zcompare_zStructLocation(ifltab, (zStructLocation *)struct1, (zStructLocation *)struct2, verboseLevel, message);
	}
	else if (structType == STRUCT_TYPE_ARRAY) {
		//  zStructArray
		return zcompare_zStructArray(ifltab, (zStructArray *)struct1, (zStructArray *)struct2, verboseLevel, boolExact, message);
	}
	else if (structType == DATA_TYPE_RTS) {
		//  zStructTimeSeries  (all time series)
		return zcompare_zStructTimeSeries(ifltab, (zStructTimeSeries *)struct1, (zStructTimeSeries *)struct2, verboseLevel, boolExact, message);
	}
	else if (structType == DATA_TYPE_PD) {
		//  zStructPairedData  (Paired Data)
	}
	else if (structType == DATA_TYPE_TEXT_TABLE) {
		//  zStructTextTable
		//return zcompare_zTxTableStruct(ifltab, struct1, struct2, verboseLevel, message);
	}
	else {
		zmessage(ifltab, "zcompareDataSets: Unsupported Data Type");
		zmessage2(ifltab, "\nStatus FAIL: ", message);
	}

	return STATUS_NOT_OKAY;
}


int zcheckStatus(long long *ifltab, int status, int verbose, const char *message)
{
	char messageString[120];

	if (zisError(status)) {
		if (verbose) {
			zmessage2(ifltab, "\nStatus FAIL: ", message);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Status: %d", status);
			zmessage(ifltab, messageString);
		}
		return STATUS_NOT_OKAY;
	}
	if (status == STATUS_RECORD_NOT_FOUND) {
		if (verbose) {
			zmessage2(ifltab, "Record NOT found; ", message);
		}
		return status;
	}
	if (verbose > 1) {
		zmessage2(ifltab, "Status Okay; ", message);
	}
	return STATUS_OKAY;
}


int zcompareInts(long long *ifltab, int number1, int number2, int verbose, const char *message)
{
	char messageString[120];

	if (number1 != number2) {
		if (verbose) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "First number: %d, second number: %d",
				number1, number2);
			zmessage2(ifltab, "\nNumbers do not match; ", messageString);
			zmessage(ifltab, message);
		}
		return STATUS_NOT_OKAY;
	}
	if (verbose > 1) {
		zmessage2(ifltab, "Passed number compare; ", message);
	}
	return STATUS_OKAY;
}

int zcompareStrings(long long *ifltab, const char *string1, const char *string2, int totalLength, int boolIgnoreCase, int verbose, const char *message)
{
	char messageString[120];
	int i;
	int len1, len2;
	char ichar1, ichar2;
	char s1[2], s2[2];


	//  It's okay if both strings are null, just not one.
	if (!string1 && !string2) {
		return STATUS_OKAY;
	}

	if (!string1) {
		//  Okay to have one null and the other zero length
		if (strlen(string2) == 0) return STATUS_OKAY;
		if (verbose) {
			zmessage2(ifltab, "\nFirst String to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!string2) {
		//  Okay to have one null and the other zero length
		if (strlen(string1) == 0) return STATUS_OKAY;
		if (verbose) {
			zmessage2(ifltab, "\nSecond String to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}

	len1 = (int)strlen(string1);
	len2 = (int)strlen(string2);

	if (len1 != len2) {
		if (verbose) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "First length: %d, second length: %d",
				len1, len2);
			zmessage2(ifltab, "\nString lengths do not match; ", messageString);
			zmessage2(ifltab, "First String:  ", string1);
			zmessage2(ifltab, "Second String: ", string2);
			zmessage(ifltab, message);
		}
		return STATUS_NOT_OKAY;
	}

	if (totalLength > 0) {
		len1 = totalLength;
	}

	for (i=0; i<len1; i++) {
		ichar1 = string1[i];
		ichar2 = string2[i];
		if (boolIgnoreCase) {
			if (ichar1 > 96) ichar1 -= 32;
			if (ichar2 > 96) ichar2 -= 32;
		}
		if (ichar1 != ichar2) {
			if (verbose) {
				s1[0] = string1[i];
				s1[1] = '\0';
				s2[0] = string2[i];
				s2[1] = '\0';
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,  ==>%s<==  ==>%s<==", i, s1, s2);
				zmessage2(ifltab, "\nStrings do not match at character ", messageString);
				zmessage2(ifltab, "First String:  ", string1);
				zmessage2(ifltab, "Second String: ", string2);
				zmessage(ifltab, message);
			}
			return STATUS_NOT_OKAY;
		}
	}

	if (verbose > 1) {
		zmessage2(ifltab, "Passed string compare; ", message);
	}

	return STATUS_OKAY;
}

int zcompareLongs(long long *ifltab, long long number1, long long number2, int verbose, const char *message)
{
	char messageString[120];

	if (number1 != number2) {
		if (verbose) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "First number: %lld, second number: %lld",
				number1, number2);
			zmessage2(ifltab, "\nLongs do not match; ", messageString);
			zmessage(ifltab, message);
		}
		return STATUS_NOT_OKAY;
	}
	if (verbose > 1) {
		zmessage2(ifltab, "Passed long compare; ", message);
	}
	return STATUS_OKAY;
}

int zcompareFloats(long long *ifltab, float number1, float number2, int verbose, const char *message)
{
	char messageString[120];

	if (number1 != number2) {
		if (verbose) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "First float: %f, second float: %f",
				number1, number2);
			zmessage2(ifltab, "\n floats do not match; ", messageString);
			zmessage(ifltab, message);
		}
		return STATUS_NOT_OKAY;
	}
	if (verbose > 1) {
		zmessage2(ifltab, "Passed float compare; ", message);
	}
	return STATUS_OKAY;
}


int zcompareDoubles(long long *ifltab, double number1, double number2, int verbose, const char *message)
{
	char messageString[120];

	if (number1 != number2) {
		if (verbose) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "First double: %f, second double: %f",
				number1, number2);
			zmessage2(ifltab, "\n doubles do not match; ", messageString);
			zmessage(ifltab, message);
		}
		return STATUS_NOT_OKAY;
	}
	if (verbose > 1) {
		zmessage2(ifltab, "Passed double compare; ", message);
	}
	return STATUS_OKAY;
}

int zcompareIntArrays(long long *ifltab, int *array1, int *array2, int numberArray, int verbose, const char *message)
{
	int i;
	char messageString[120];


	//  It's okay if both arrays are null, just not one.
	if (!array1 && !array2) {
		return STATUS_OKAY;
	}
	if (!array1) {
		if (verbose) {
			zmessage2(ifltab, "\nFirst int array to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!array2) {
		if (verbose) {
			zmessage2(ifltab, "\nSecond int array to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}

	for (i=0; i<numberArray; i++) {
		if (array1[i] != array2[i]) {
			if (verbose) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d;  First value: %d, second value: %d",
					i, array1[i], array2[i]);
				zmessage2(ifltab, "\nInts do not match at element ", messageString);
				zmessage(ifltab, message);
			}
			return STATUS_NOT_OKAY;
		}
	}
	if (verbose > 1) {
		zmessage2(ifltab, "Passed Int array compare; ", message);
	}
	return STATUS_OKAY;
}

int zcompareFloatArrays(long long *ifltab, float *array1, float *array2, int numberArray, int verbose, const char *message)
{
	int i;
	char messageString[120];


	//  It's okay if both arrays are null, just not one.
	if (!array1 && !array2) {
		return STATUS_OKAY;
	}
	if (!array1) {
		if (verbose) {
			zmessage2(ifltab, "\nFirst float array to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!array2) {
		if (verbose) {
			zmessage2(ifltab, "\nSecond float array to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}

	for (i=0; i<numberArray; i++) {
		if (array1[i] != array2[i]) {
			if (verbose) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d;  First value: %f, second value: %f",
					i, array1[i], array2[i]);
				zmessage2(ifltab, "\nFloats do not match at element ", messageString);
				zmessage(ifltab, message);
			}
			return STATUS_NOT_OKAY;
		}
	}
	if (verbose > 1) {
		zmessage2(ifltab, "Passed Float array compare; ", message);
	}
	return STATUS_OKAY;
}


int zcompareDoubleArrays(long long *ifltab, double *array1, double *array2, int numberArray, int verbose, const char *message)
{
	int i;
	char messageString[120];


	//  It's okay if both arrays are null, just not one.
	if (!array1 && !array2) {
		return STATUS_OKAY;
	}
	if (!array1) {
		if (verbose) {
			zmessage2(ifltab, "\nFirst double array to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!array2) {
		if (verbose) {
			zmessage2(ifltab, "\nSecond double array to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}

	for (i=0; i<numberArray; i++) {
		if (array1[i] != array2[i]) {
			if (verbose) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d;  First value: %f, second value: %f",
					i, array1[i], array2[i]);
				zmessage2(ifltab, "\nDoubles do not match at element ", messageString);
				zmessage(ifltab, message);
			}
			return STATUS_NOT_OKAY;
		}
	}
	if (verbose > 1) {
		zmessage2(ifltab, "Passed Double array compare; ", message);
	}
	return STATUS_OKAY;
}

int zcompareStringArrays(long long *ifltab, const char *string1, const char *string2, int totalBytes, int boolIgnoreCase, int verbose, const char *message)
{
	char messageString[120];
	int lineNumber, ipos, jpos;
	int i;
	char ichar1, ichar2;
	char s1[2], s2[2];


		//  It's okay if both strings are null, just not one.
	if (!string1 && !string2) {
		return STATUS_OKAY;
	}
	if (!string1) {
		if (verbose) {
			zmessage2(ifltab, "\nFirst String to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!string2) {
		if (verbose) {
			zmessage2(ifltab, "\nSecond String to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}


	lineNumber = 0;
	ipos = 0;
	jpos = 0;
	for (i=0; i<totalBytes; i++) {
		ichar1 = string1[i];
		ichar2 = string2[i];
		if (boolIgnoreCase) {
			if (ichar1 > 96) ichar1 -= 32;
			if (ichar2 > 96) ichar2 -= 32;
		}
		if (ichar1 != ichar2) {
			if (verbose) {
				s1[0] = string1[i];
				s1[1] = '\0';
				s2[0] = string2[i];
				s2[1] = '\0';
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, array position %d", lineNumber, i);
				zmessage2(ifltab, "\nStrings do not match at line number ", messageString);
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,  ==>%s<==  ==>%s<==", ipos, s1, s2);
				zmessage2(ifltab, "\nString position ", messageString);
				zmessage2(ifltab, "First String:  ", &string1[jpos]);
				zmessage2(ifltab, "Second String: ", &string2[jpos]);
				zmessage(ifltab, message);
			}
			return STATUS_NOT_OKAY;
		}
		ipos++;
		if (ichar1 == '\0') {
			lineNumber++;
			ipos = 0;
			jpos = i + 1;
		}
	}

	if (verbose > 1) {
		zmessage2(ifltab, "Passed string array compare; ", message);
	}

	return STATUS_OKAY;
}

void zprintFailMessage(long long *ifltab, int verboseLevel, const char *name, const char *pathname, const char *message)
{
	if (verboseLevel) {
		zmessage2(ifltab, name, " struct compare failed");
		if ((int)strlen(pathname) > 1) {
			zmessage2(ifltab, "Pathname: ", pathname);
		}
		zmessage2(ifltab, message, "\n\n");
	}
}



////////////  zcompare_zStructTsRecordSizes

int zcompare_zStructTsRecordSizes(long long *ifltab, zStructRecordSize *struct1, zStructRecordSize *struct2, int verboseLevel, const char *pathname, const char *message)
{
	int status;
	char name[] = "zStructRecordSize";


	if (!struct1) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nFirst zStructRecordSize struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!struct2) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nSecond zStructRecordSize struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}



	status = zcompareInts(ifltab, struct1->dataType, struct2->dataType, verboseLevel, " dataType differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->version, struct2->version, verboseLevel, " version differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->numberValues, struct2->numberValues, verboseLevel, " numberValues differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->logicalNumberValues, struct2->logicalNumberValues, verboseLevel, " logicalNumberValues differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->tsValueSize, struct2->tsValueSize, verboseLevel, " valueSize differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->tsQualityElementSize, struct2->tsQualityElementSize, verboseLevel, " qualityElementSize differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->tsInotesElementSize, struct2->tsInotesElementSize, verboseLevel, " inotesElementSize differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->tsCnotesLength, struct2->tsCnotesLength, verboseLevel, " cnotesLength differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->tsProfileDepthsNumber, struct2->tsProfileDepthsNumber, verboseLevel, " profileDepthsNumber differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->tsPrecision, struct2->tsPrecision, verboseLevel, " tsPrecision differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->lastWriteTimeMillis, struct2->lastWriteTimeMillis, verboseLevel, " lastWriteTimeMillis differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->values1Number, struct2->values1Number, verboseLevel, " values1Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->values2Number, struct2->values2Number, verboseLevel, " values2Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->internalHeaderNumber, struct2->internalHeaderNumber, verboseLevel, " internalHeaderNumber differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->userHeaderNumber, struct2->userHeaderNumber, verboseLevel, " userHeaderNumber differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->header2Number, struct2->header2Number, verboseLevel, " header2Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	if (verboseLevel > 1) {
		zmessage(ifltab, "zStructRecordSize comparison passed.");
	}

	return STATUS_OKAY;
}


////////////  zcompare_zStructRecordSize

int zcompare_zStructRecordSize(long long *ifltab, zStructRecordSize *struct1, zStructRecordSize *struct2, int verboseLevel, const char *message)
{
	int status;
	char name[] = "zStructRecordSize";
	char *pathname;



	if (!struct1) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nFirst zStructRecordSize struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!struct2) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nSecond zStructRecordSize struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}

	pathname = struct1->pathname;

	status = zcompareInts(ifltab, struct1->dataType, struct2->dataType, verboseLevel, " dataType differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->version, struct2->version, verboseLevel, " version differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->numberValues, struct2->numberValues, verboseLevel, " numberValues differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->logicalNumberValues, struct2->logicalNumberValues, verboseLevel, " logicalNumberValues differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->lastWriteTimeMillis, struct2->lastWriteTimeMillis, verboseLevel, " lastWriteTimeMillis differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareStrings(ifltab, struct1->programLastWrite, struct2->programLastWrite, 0, 0, verboseLevel, " programLastWrite differ");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->values1Number, struct2->values1Number, verboseLevel, " values1Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->values2Number, struct2->values2Number, verboseLevel, " values2Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->values3Number, struct2->values3Number, verboseLevel, " values3Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->internalHeaderNumber, struct2->internalHeaderNumber, verboseLevel, " internalHeaderNumber differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->userHeaderNumber, struct2->userHeaderNumber, verboseLevel, " userHeaderNumber differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->header2Number, struct2->header2Number, verboseLevel, " header2Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}


	if (verboseLevel > 1) {
		zmessage(ifltab, "zStructRecordSize comparison passed.");
	}

	return STATUS_OKAY;
}


///////////  zcompare_zStructRecordAddresses

int zcompare_zStructRecordAddresses(long long *ifltab, zStructRecordAddresses *struct1, zStructRecordAddresses *struct2, int verboseLevel, const char *pathname, const char *message)
{
	int status;
	char name[] = "zStructRecordAddresses";



	//  Internals
	status = zcompareInts(ifltab, struct1->binSize, struct2->binSize, verboseLevel, " binSize differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->infoLength, struct2->infoLength, verboseLevel, " infoLength differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}


	status = zcompareLongs(ifltab, struct1->tableHash, struct2->tableHash, verboseLevel, " tableHash differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->hashCode, struct2->hashCode, verboseLevel, " hashCode differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->totalAllocatedSize, struct2->totalAllocatedSize, verboseLevel, " totalAllocatedSize differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->hashAddress, struct2->hashAddress, verboseLevel, " hashAddress differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->binAddress, struct2->binAddress, verboseLevel, " binAddress differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->infoAddress, struct2->infoAddress, verboseLevel, " infoAddress differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->internalHeaderAddress, struct2->internalHeaderAddress, verboseLevel, " internalHeaderAddress differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->header2Address, struct2->header2Address, verboseLevel, " header2Address differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->userHeaderAddress, struct2->userHeaderAddress, verboseLevel, " userHeaderAddress differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->values1Address, struct2->values1Address, verboseLevel, " values1Address differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->values2Address, struct2->values2Address, verboseLevel, " values2Address differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}

	status = zcompareLongs(ifltab, struct1->values3Address, struct2->values3Address, verboseLevel, " values3Address differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, pathname, message);
		return status;
	}


	if (verboseLevel > 1) {
		zmessage(ifltab, "zStructRecordAddresses comparison passed.");
	}

	return STATUS_OKAY;
}



////////////  zcompare_ztransferStruct

int zcompare_ztransferStruct(long long *ifltab, zStructTransfer *struct1, zStructTransfer *struct2, int verboseLevel, int boolExact, const char *message)
{
	int status;
	char name[] = "zStructTransfer";



	if (!struct1) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nFirst zStructTransfer struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!struct2) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nSecond zStructTransfer struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}

	if (boolExact) {
		status = zcompareStrings(ifltab, struct1->pathname, struct2->pathname, 0, 1, verboseLevel, " pathnames differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareInts(ifltab, struct1->pathnameLength, struct2->pathnameLength, verboseLevel, " pathnameLength differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	status = zcompareInts(ifltab, struct1->dataType, struct2->dataType, verboseLevel, " dataType differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	//  Headers
	//  internalHeader
	status = zcompareInts(ifltab, struct1->internalHeaderNumber, struct2->internalHeaderNumber, verboseLevel, " internalHeaderNumber differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	if (boolExact) {
		status = zcompareInts(ifltab, struct1->internalHeaderMode, struct2->internalHeaderMode, verboseLevel, " internalHeaderMode differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	status = zcompareIntArrays(ifltab, struct1->internalHeader, struct2->internalHeader, struct2->internalHeaderNumber, verboseLevel, " internalHeader differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}


	//  header2
	status = zcompareInts(ifltab, struct1->header2Number, struct2->header2Number, verboseLevel, " header2Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	if (boolExact) {
		status = zcompareInts(ifltab, struct1->header2Mode, struct2->header2Mode, verboseLevel, " header2Mode differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	status = zcompareIntArrays(ifltab, struct1->header2, struct2->header2, struct2->header2Number, verboseLevel, " header2 differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}


	//  values3
	status = zcompareInts(ifltab, struct1->values3Number, struct2->values3Number, verboseLevel, " values3Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	if (boolExact) {
		status = zcompareInts(ifltab, struct1->values3Mode, struct2->values3Mode, verboseLevel, " values3Mode differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	status = zcompareIntArrays(ifltab, struct1->values3, struct2->values3, struct2->values3Number, verboseLevel, " values3 differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}


	//  userHeader
	status = zcompareInts(ifltab, struct1->userHeaderNumber, struct2->userHeaderNumber, verboseLevel, " userHeaderNumber differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	if (boolExact) {
		status = zcompareInts(ifltab, struct1->userHeaderMode, struct2->userHeaderMode, verboseLevel, " userHeaderMode differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	status = zcompareIntArrays(ifltab, struct1->userHeader, struct2->userHeader, struct2->userHeaderNumber, verboseLevel, " userHeader differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}


	//  values1
	status = zcompareInts(ifltab, struct1->values1Number, struct2->values1Number, verboseLevel, " values1Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	if (boolExact) {
		status = zcompareInts(ifltab, struct1->values1Mode, struct2->values1Mode, verboseLevel, " values1Mode differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	status = zcompareIntArrays(ifltab, struct1->values1, struct2->values1, struct1->values1Number, verboseLevel, " values1 differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}


	//  values2
	status = zcompareInts(ifltab, struct1->values2Number, struct2->values2Number, verboseLevel, " values2Number differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	if (boolExact) {
		status = zcompareInts(ifltab, struct1->values2Mode, struct2->values2Mode, verboseLevel, " values2Mode differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	status = zcompareIntArrays(ifltab, struct1->values2, struct2->values2, struct1->values2Number, verboseLevel, " values2 differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}


	//  Other
	status = zcompareInts(ifltab, struct1->numberValues, struct2->numberValues, verboseLevel, " numberValues differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->logicalNumberValues, struct2->logicalNumberValues, verboseLevel, " logicalNumberValues differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	if (boolExact) {
		status = zcompareInts(ifltab, struct1->totalAllocatedSize, struct2->totalAllocatedSize, verboseLevel, " totalAllocatedSize differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareInts(ifltab, struct1->totalExpandedSize, struct2->totalExpandedSize, verboseLevel, " totalExpandedSize differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareInts(ifltab, struct1->version, struct2->version, verboseLevel, " version differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareInts(ifltab, struct1->insufficientSpace, struct2->insufficientSpace, verboseLevel, " insufficientSpace differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareLongs(ifltab, struct1->lastWrittenTime, struct2->lastWrittenTime, verboseLevel, " lastWrittenTime differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareStrings(ifltab, struct1->programName, struct2->programName, 0, 0, verboseLevel, " programName differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}



	if (verboseLevel > 1) {
		zmessage(ifltab, "zStructTransfer comparison passed.");
	}

	return STATUS_OKAY;
}


////////////  zcompare_zStructLocation

int zcompare_zStructLocation(long long *ifltab, zStructLocation *struct1, zStructLocation *struct2, int verboseLevel, const char *message)
{
	int status;
	char name[] = "zStructLocation";



	if (!struct1) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nFirst zStructLocation struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!struct2) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nSecond zStructLocation struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}


	status = zcompareStrings(ifltab, struct1->pathname, struct2->pathname, 0, 1, verboseLevel, " pathnames differ");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}



	status = zcompareStrings(ifltab, struct1->timeZoneName, struct2->timeZoneName, 0, 1, verboseLevel, " timeZoneName differ");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}


	status = zcompareDoubles(ifltab, struct1->xOrdinate, struct2->xOrdinate, verboseLevel, " xOrdinate differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	status = zcompareDoubles(ifltab, struct1->yOrdinate, struct2->yOrdinate, verboseLevel, " yOrdinate differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	status = zcompareDoubles(ifltab, struct1->zOrdinate, struct2->zOrdinate, verboseLevel, " zOrdinate differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}


	status = zcompareInts(ifltab, struct1->coordinateSystem, struct2->coordinateSystem, verboseLevel, " coordinateSystem differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->coordinateID, struct2->coordinateID, verboseLevel, " coordinateID differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->horizontalUnits, struct2->horizontalUnits, verboseLevel, " horizontalUnits differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->verticalUnits, struct2->verticalUnits, verboseLevel, " verticalUnits differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->verticalDatum, struct2->verticalDatum, verboseLevel, " verticalDatum differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->horizontalDatum, struct2->horizontalDatum, verboseLevel, " horizontalDatum differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}


	if ((!struct1->supplemental) && (!struct2->supplemental)) {
		//  Both null is fine
	}
	else if ((struct1->supplemental) && (struct2->supplemental)) {
		status = zcompareStrings(ifltab, struct1->supplemental, struct2->supplemental, 0, 1, verboseLevel, " supplemental differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains other info, the other does not.");
		return -1;
	}



	if (verboseLevel > 1) {
		zmessage(ifltab, "zStructLocation comparison passed.");
	}

	return STATUS_OKAY;
}



int zcompare_zStructArray(long long *ifltab, zStructArray *struct1, zStructArray *struct2, int verboseLevel, int boolExact, const char *message)
{
	int i;
	int count;
	int status;
	char name[] = "zStructArray";
	int dataType1[3];
	int dataType2[3];


	if (!struct1) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nFirst zStructArray struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!struct2) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nSecond zStructArray struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}


	status = zcompareStrings(ifltab, struct1->pathname, struct2->pathname, 0, 1, verboseLevel, " pathnames differ");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	for (i=0; i<3; i++) {
		dataType1[i] = 0;
		dataType2[i] = 0;
	}

	if (struct1->intArray) dataType1[0] = 1;
	if (struct1->floatArray) dataType1[1] = 1;
	if (struct1->doubleArray) dataType1[2] = 1;
	if (struct2->intArray) dataType2[0] = 1;
	if (struct2->floatArray) dataType2[1] = 1;
	if (struct2->doubleArray) dataType2[2] = 1;

	count = 0;
	for (i=0; i<3; i++) {
		if (dataType1[i]) count++;
	}
	if (count != 1) {
		if (count == 0) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "No array used for first array struct");
			return -1;
		}
		else {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "Multiple arrays used for first array struct");
			return -1;
		}
	}

	count = 0;
	for (i=0; i<3; i++) {
		if (dataType2[i]) count++;
	}
	if (count != 1) {
		if (count == 0) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "No array used for second array struct");
			return -1;
		}
		else {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "Multiple arrays used for second array struct");
			return -1;
		}
	}

	for (i=0; i<3; i++) {
		status = zcompareInts(ifltab, dataType1[i], dataType2[i], verboseLevel, "Array type miss-match for array struct");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	if (struct1->intArray) {
		status = zcompareInts(ifltab, struct1->numberIntArray, struct2->numberIntArray, verboseLevel, " length of int arrays differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
		status = zcompareIntArrays(ifltab, struct1->intArray, struct2->intArray, struct1->numberIntArray, verboseLevel, " int arrays differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	if (struct1->floatArray) {
		status = zcompareInts(ifltab, struct1->numberFloatArray, struct2->numberFloatArray, verboseLevel, " length of float arrays differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
		status = zcompareFloatArrays(ifltab, struct1->floatArray, struct2->floatArray, struct1->numberFloatArray, verboseLevel, " float arrays differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	if (struct1->doubleArray) {
		status = zcompareInts(ifltab, struct1->numberDoubleArray, struct2->numberDoubleArray, verboseLevel, " length of double arrays differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
		status = zcompareDoubleArrays(ifltab, struct1->doubleArray, struct2->doubleArray, struct1->numberDoubleArray, verboseLevel, " double arrays differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}




	if (verboseLevel > 1) {
		zmessage(ifltab, "zStructArray comparison passed.");
	}

	return STATUS_OKAY;
}





////////////  zcompare_zStructTimeSeries

int zcompare_zStructTimeSeries(long long *ifltab, zStructTimeSeries *struct1, zStructTimeSeries *struct2, int verboseLevel, int boolExact, const char *message)
{
	int status;
	int number;
	char name[] = "zStructTimeSeries";



	if (!struct1) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nFirst zStructTimeSeries struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!struct2) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nSecond zStructTimeSeries struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}

	if (boolExact) {
		status = zcompareStrings(ifltab, struct1->pathname, struct2->pathname, 0, 1, verboseLevel, " pathnames differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}

	status = zcompareInts(ifltab, struct1->boolPattern, struct2->boolPattern, verboseLevel, " One is pattern data, the other is not");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	status = zcompareInts(ifltab, struct1->dataType, struct2->dataType, verboseLevel, " dataType differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	if (!struct1->boolPattern) {
		status = zcompareTimeArrays(ifltab, struct1, struct2, verboseLevel, boolExact, " TimeArrays differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}


	///////////////////
	//  Data
	status = zcompareInts(ifltab, struct1->numberValues, struct2->numberValues, verboseLevel, " numberValues differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}
	if (boolExact) {
		status = zcompareInts(ifltab, struct1->sizeEachValueRead, struct2->sizeEachValueRead, verboseLevel, " sizeEachValueRead differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	status = zcompareInts(ifltab, struct1->precision, struct2->precision, verboseLevel, " precision differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	if ((!struct1->floatValues) && (!struct2->floatValues)) {
		//  Both null is fine
	}
	else if ((struct1->floatValues) && (struct2->floatValues)) {
		status = zcompareFloatArrays(ifltab, struct1->floatValues, struct2->floatValues, struct1->numberValues, verboseLevel, " floatValues differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains floatValues, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}

	if ((!struct1->doubleValues) && (!struct2->doubleValues)) {
		//  Both null is fine
	}
	else if ((struct1->doubleValues) && (struct2->doubleValues)) {
		status = zcompareDoubleArrays(ifltab, struct1->doubleValues, struct2->doubleValues, struct1->numberValues, verboseLevel, " doubleValues differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains doubleValues, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}

	if ((!struct1->units) && (!struct2->units)) {
		//  Both null is fine
	}
	else if ((struct1->units) && (struct2->units)) {
		status = zcompareStrings(ifltab, struct1->units, struct2->units, 0, 1, verboseLevel, " units differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains units, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}

	if ((!struct1->type) && (!struct2->type)) {
		//  Both null is fine
	}
	else if ((struct1->type) && (struct2->type)) {
		status = zcompareStrings(ifltab, struct1->type, struct2->type, 0, 1, verboseLevel, " type differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains type, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}



	/////////////
	//  Profile data
	////////////

	status = zcompareInts(ifltab, struct1->profileDepthsNumber, struct2->profileDepthsNumber, verboseLevel, " profileDepthsNumber differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}

	//  floats

	if ((!struct1->floatProfileDepths) && (!struct2->floatProfileDepths)) {
		//  Both null is fine
	}
	else if ((struct1->floatProfileDepths) && (struct2->floatProfileDepths)) {
		status = zcompareFloatArrays(ifltab, struct1->floatProfileDepths, struct2->floatProfileDepths, struct1->profileDepthsNumber, verboseLevel, " floatProfileDepths differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains floatProfileDepths, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}

	if ((!struct1->floatProfileValues) && (!struct2->floatProfileValues)) {
		//  Both null is fine
	}
	else if ((struct1->floatProfileValues) && (struct2->floatProfileValues)) {
		number = struct2->profileDepthsNumber * struct2->numberValues;
		status = zcompareFloatArrays(ifltab, struct1->floatProfileValues, struct2->floatProfileValues, number, verboseLevel, " floatProfileValues differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains floatValues, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}


	//  doubles

	if ((!struct1->doubleProfileDepths) && (!struct2->doubleProfileDepths)) {
		//  Both null is fine
	}
	else if ((struct1->doubleProfileDepths) && (struct2->doubleProfileDepths)) {
		status = zcompareDoubleArrays(ifltab, struct1->doubleProfileDepths, struct2->doubleProfileDepths, struct1->profileDepthsNumber, verboseLevel, " doubleProfileDepths differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains doubleProfileDepths, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}

	if ((!struct1->doubleProfileValues) && (!struct2->doubleProfileValues)) {
		//  Both null is fine
	}
	else if ((struct1->doubleProfileValues) && (struct2->doubleProfileValues)) {
		number = struct2->profileDepthsNumber * struct2->numberValues;
		status = zcompareDoubleArrays(ifltab, struct1->doubleProfileValues, struct2->doubleProfileValues, number, verboseLevel, " doubleProfileValues differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains doubleProfileValues, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}



	if ((!struct1->unitsProfileDepths) && (!struct2->unitsProfileDepths)) {
		//  Both null is fine
	}
	else if ((struct1->unitsProfileDepths) && (struct2->unitsProfileDepths)) {
		status = zcompareStrings(ifltab, struct1->unitsProfileDepths, struct2->unitsProfileDepths, 0, 1, verboseLevel, " unitsProfileDepths differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains unitsProfileDepths, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}

	if ((!struct1->unitsProfileValues) && (!struct2->unitsProfileValues)) {
		//  Both null is fine
	}
	else if ((struct1->unitsProfileValues) && (struct2->unitsProfileValues)) {
		status = zcompareStrings(ifltab, struct1->unitsProfileValues, struct2->unitsProfileValues, 0, 1, verboseLevel, " unitsProfileValues differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains unitsProfileValues, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}


	//  Other stuff
	if ((!struct1->timeZoneName) && (!struct2->timeZoneName)) {
	}
	else {
		status = zcompareStrings(ifltab, struct1->timeZoneName, struct2->timeZoneName, 0, 1, verboseLevel, " timeZoneName differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}



	//////////////////////////
	///  Quality
	//  Array size is usually used only for read
	if (boolExact) {
		status = zcompareInts(ifltab, struct1->qualityArraySize, struct2->qualityArraySize, verboseLevel, " qualityArraySize differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	status = zcompareInts(ifltab, struct1->qualityElementSize, struct2->qualityElementSize, verboseLevel, " qualityElementSize differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}
	if ((!struct1->quality) && (!struct2->quality)) {
	}
	else if ((struct1->quality) && (struct2->quality)) {
		number = struct1->qualityElementSize * struct1->numberValues;
		status = zcompareIntArrays(ifltab, struct1->quality, struct2->quality, number, verboseLevel, " quality differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains quality, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}


	//////////////////////////
	///  int notes
	if (boolExact) {
		status = zcompareInts(ifltab, struct1->inotesArraySize, struct2->inotesArraySize, verboseLevel, " inotesArraySize differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	status = zcompareInts(ifltab, struct1->inoteElementSize, struct2->inoteElementSize, verboseLevel, " inoteElementSize differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}
	if ((!struct1->inotes) && (!struct2->inotes)) {
	}
	else if ((struct1->inotes) && (struct2->inotes)) {
		number = struct1->inoteElementSize * struct1->numberValues;
		status = zcompareIntArrays(ifltab, struct1->inotes, struct2->inotes, number, verboseLevel, " inotes differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains inotes, the other does not.");
		return -1;
	}


	//////////////////////////
	///  character notes
	status = zcompareInts(ifltab, struct1->cnotesLengthTotal, struct2->cnotesLengthTotal, verboseLevel, " cnotesLengthTotal differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}
	//  We don't check size of char array
	if ((!struct1->cnotes) && (!struct2->cnotes)) {
	}
	else if ((struct1->cnotes) && (struct2->cnotes)) {
		//  FIX MWE this does not work for string with imbedded nulls
		status = zcompareStrings(ifltab, struct1->cnotes, struct2->cnotes, struct1->cnotesLengthTotal, 0, verboseLevel, " cnotes differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}
	else {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains cnotes, the other does not.");
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return -1;
	}



	//  Headers
	//  internalHeader

	//  userHeader
	status = zcompareInts(ifltab, struct1->userHeaderNumber, struct2->userHeaderNumber, verboseLevel, " userHeaderNumber differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}
	status = zcompareIntArrays(ifltab, struct1->userHeader, struct2->userHeader, struct2->userHeaderNumber, verboseLevel, " userHeader differs");
	if (status) {
		zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
		return status;
	}


	//  Misc stuff
	if (boolExact) {
		status = zcompareLongs(ifltab, struct1->lastWrittenTime, struct2->lastWrittenTime, verboseLevel, " lastWrittenTime differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareStrings(ifltab, struct1->programName, struct2->programName, 0, 0, verboseLevel, " programName differ");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}
	}



	if (verboseLevel > 1) {
		zmessage(ifltab, "zStructTimeSeries comparison passed.");
	}

	return STATUS_OKAY;
}

int zcompareTimeArrays(long long *ifltab, zStructTimeSeries *struct1, zStructTimeSeries *struct2, int verboseLevel, int boolExact, const char *message)
{
	int status;
	int i;
	int itime;
	int julian;

	char name[] = "zcompareTimeArrays";


	if (!struct1) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nFirst zcompareTimeArrays struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}
	if (!struct2) {
		if (verboseLevel) {
			zmessage2(ifltab, "\nSecond zcompareTimeArrays struct to compare is null; ", message);
		}
		return STATUS_NOT_OKAY;
	}

	if (boolExact) {
		if (struct1->timeGranularitySeconds && struct2->timeGranularitySeconds) {
			status = zcompareInts(ifltab, struct1->timeGranularitySeconds, struct2->timeGranularitySeconds, verboseLevel, " timeGranularitySeconds differs");
			if (status) {
				zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
				return status;
			}
		}
	}


	if ((!struct1->times) && (!struct2->times)) {

		//   Time Info

		if (boolExact) {
			status = zcompareInts(ifltab, struct1->julianBaseDate, struct2->julianBaseDate, verboseLevel, " julianBaseDate differs");
			if (status) {
				zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
				return status;
			}
		}

		status = zcompareInts(ifltab, struct1->startJulianDate, struct2->startJulianDate, verboseLevel, " startJulianDate differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareInts(ifltab, struct1->startTimeSeconds, struct2->startTimeSeconds, verboseLevel, " startTimeSeconds differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareInts(ifltab, struct1->endJulianDate, struct2->endJulianDate, verboseLevel, " endJulianDate differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareInts(ifltab, struct1->endTimeSeconds, struct2->endTimeSeconds, verboseLevel, " endTimeSeconds differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

		status = zcompareInts(ifltab, struct1->timeOffsetSeconds, struct2->timeOffsetSeconds, verboseLevel, " timeOffset differs");
		if (status) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, message);
			return status;
		}

	}
	else if ((struct1->times) && (struct2->times)) {

		for (i = 0; i<struct1->numberValues; i++) {

			status = compareTimes(0, struct1->times[i], struct1->julianBaseDate, struct1->timeGranularitySeconds, 
								  0, struct2->times[i], struct2->julianBaseDate, struct2->timeGranularitySeconds);
			if (status) {
				if (verboseLevel > 0) {
					zmessageInt(ifltab, "Dates / times do not match at element ", i);
					julian = struct1->julianBaseDate;
					itime = struct1->times[i];
					cleanTime(&julian, &itime, struct1->timeGranularitySeconds);
					itime *= struct1->timeGranularitySeconds;
					ztsDateMessage(ifltab, 0, "First date / time: ", julian, itime);
					julian = struct2->julianBaseDate;
					itime = struct2->times[i];
					cleanTime(&julian, &itime, struct2->timeGranularitySeconds);
					itime *= struct2->timeGranularitySeconds;
					ztsDateMessage(ifltab, 0, "Second date / time: ", julian, itime);
				}
				return STATUS_NOT_OKAY;
			}
		}

	}
	else {
		if (boolExact || (struct1->timeIntervalSeconds < 1)) {
			zprintFailMessage(ifltab, verboseLevel, name, struct1->pathname, "One struct contains times, the other does not.");
			return -1;
		}
		else {
			zmessage2(ifltab, "One struct has a time array and the other doesn't (not necessarily an error): ", struct1->pathname);
		}
	}


	if (verboseLevel > 1) {
		zmessage2(ifltab, "Passed Time array compare; ", message);
	}
	return STATUS_OKAY;
}



