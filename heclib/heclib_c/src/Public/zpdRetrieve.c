#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "heclib.h"
#include "zerrorCodes.h"
#include "verticalDatum.h"


/**
*  Function:	zpdRetrieve
*
*  Use:			Public
*
*  Description:	Primary function to retrieve paired data
*
*  Declaration: int zpdRetrieve(long long *ifltab, zStructPairedData *pds, int retrieveFlag);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*					See zopen() for more information.
*
*				zStructPairedData *pds
*					A struct that will contain the data and information from the record.
*					This struct is created by the following method:
*						zStructPairedData* zstructPdNew(const char* pathname);
*					When you are done, the struct must be freed by a call to
*						void zstructFree(zStructPairedData *pds)
*					NEVER REUSE A zStructPairedData, always free and create a new on.
*
*				int retrieveFlag
*					A flag indicating if floats or doubles should be returned.  This is independent of
*					what is actually stored on disk  Values will be converted to the requested type.
*						0:  Return data as stored.
*						1:  Return floats
*						2:  Return doubles
*
*
*	Returns:	int status
*					STATUS_OKAY	(same as STATUS_RECORD_FOUND)
*					STATUS_RECORD_NOT_FOUND
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*
*	Remarks:
*				Parameters within zStructPairedData allow you to retrieve just one or a set of curves,
*					a set of rows, or a block of data (rows/curves). See struct description below
*				The "independent variable" is the name of the parameter for the ordinate set.
*					The "dependent variable" is the name of the parameter for the curve(s).
*					For example, for a "Stage-Damage" function, the independent variable is "Stage" and the ordinates
*					are the heights where damages are given.  The dependent variable is "Damage", and may include several curves,
*					such as "Total", "Residential", "Commercial", etc.
*				Curves and columns are synonymous, as well as ordinates and rows.  We store curves, but often display in columns.
*				The first curve is "1", not "0".  The first ordinate is "1", not "0".  (by convention)
*				Paired data is stored in columns (not rows).  Column 1 is followed by all of column 2
*
*
*  zStructPairedData parameters used in this call:
*
*	Required:
*
*				const char* pathname
*					The pathname of the record to retrieve.
*
*
*	Optional - For retrieving part of a data set:
*
*				You can retrieve part of a data set, such as a curve, a set of curves, a row or set of rows,
*				a block or an individual value.  For example, if you wanted to retrieve just an individual value, set
*				startingOrdinate = endingOrdinate, and startingCurve = endingCurve (remember, the first row is "row 1", not "row 0")
*				The oridnate array will correspond to the rows returned
*
*				int startingCurve
*					The starting curve number (starting at 1) for the data you want.
*					To ignore (.e.g, return an entire row), set to zero (the default)
*
*				int endingCurve
*					The ending curve number for the data you want.  Set to "0" to ignore.
*
*				int startingOrdinate
*					The starting row (starting at 1) for the data you want.  Set to "0" to ignore.
*
*				int endingOrdinate
*					The ending row for the data you want.  Set to "0" to ignore.
*
*
*	Returned
*				float *floatOrdinates
*					The float ordinate array containing the "independent variable" data, if retrieving floats (otherwise zero)
*			or
*				double *doubleOrdinates
*					The double ordinate array containing the "independent variable" data, if retrieving doubles (otherwise zero)
*
*				float *floatValues
*					A float array containing the "dependent variable" data.  If a family of curves is stored in this record,
*					then the second curve follows the first curve (not first row, then second row).  Must match ordinate type.
*			or
*				double *doubleValues
*					A single array containing the "dependent variable" data.  If a family of curves is stored in this record,
*					then the second curve follows the first curve (not first row, then second row).  Must match ordinate type.
*
*					The dimensions of the values will must be consistent with numberCurves; i.e., doubleValues[numberCurves][numberOrdinates]
*
*				int numberOrdinates
*					Number of ordinates in the data set.  This can also be thought of as the number of rows.
*
*				int numberCurves
*					Number of curves or columns, often one.  If the function was a "Stage-Damage" curve, and the curves
*					were "Total", "Residential" and "Commerical", then this would be "3", and the residential values would
*					follow (all of) the total values, and the commerical curve would follow the residental curve.
*
*				int numberOrdinatesInStruct
*					The number of ordinates in the ordinate array in this struct.  If retrieving all data, this will
*					equal numberOrdinates.  If retrieving part of the record, then this will be the number retrieved.
*					(Usually numberOrdinatesInStruct = endingOrdinate - startingOrdinate + 1)
*
*				int numberCurvesInStruct
*					The number of curves returned in this struct.  If retrieving all data, this will
*					equal numberCurves.  If retrieving part of the record, then this will be the number retrieved.
*					(Usually numberCurvesInStruct = endingCurve - startingCurve + 1)
*
*				int startingCurve
*					The starting curve number (starting at 1) of the data set.
*
*				int endingCurve
*					The ending curve number of the data set.
*
*				int startingOrdinate
*					The starting row (starting at 1) of the data set.
*
*				int endingOrdinate
*					The ending row of the data set.
*
*				const char *unitsIndependent
*					The units of the independent data (ordinates), such as "Feet".
*
*				const char *typeIndependent
*					Defines the ordinates type and can be "Linear", "Log", or "Probability".
*
*				const char *unitsDependent
*					The units of the dependent data (curves), such as "Dollars".
*
*				const char *typeDependentt
*					The curve type.  The type can be "Linear", "Log", or "Probability".
*
*				int boolIndependentIsXaxis
*					A 0/1 flag indicating if the independent variable is to be plotted on the X axis.
*
*				int precision
*					An 2 digit int that represents the precision, or number of decimal places, for values.
*					The precision is the number of digits past the decimal.
*					For example, 0 gives a number like 123.  1 gives a number like 123.4, 2 1234.56, etc.
*					The first digit is for the independent variable, the second for the dependent.
*					For example, "21" provides a pair such as 12.34, 123.4
*					-1 indicates that this has not been set.
*
*				char *labels
*					A character string the contains a label for each curve, if used.
*					Each label must be null terminated.  The total length of labels is given in labelsLength.
*					For example, in a "Stage-Damage" curve, this might be "Total\0Residential\0Commerical\0"
*
*				int labelsLength
*					The number of characters in labels.  If zero, no labels were stored.
*
*				char *timeZoneName
*					The time zone name for this record, if set.  This is not necessarily the same as the location time zone,
*					although it usually is.  The name is generally that used in Java and includes a variety of types.
*					For example, you might have a data set recorded with GTM, although the location is PST.
*
*				int *userHeader
*					An int array to hold any additional information for the record, usually from the user.
*
*				int userHeaderNumber
*					The number of int words to be stored in the userHeader
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zpdRetrieve(long long *ifltab, zStructPairedData *pds, int retrieveSizeFlag)
{
	int i;
	int ipos;
	int numberValues;
	int number;
	int status;
	int boolRetrieveDoubles;
	int startOrdinate;
	int endOrdinate;
	int startCurve;
	int endCurve;
	int boolReadEntire;
	char *clabels;
	int count;
	int start;
	int len;
	int offset;
	int numberToRead;
	int wordSize;
	long long address;
	int buffer[1]={0}; long long bufferControl[4] ={0,0,0,0};
	char messageString[100];
	zStructTransfer* ztransfer;


	if (!pds) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "zStructPairedData is null");
	}
	if (!pds->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "zStructPairedData pathname is null");
	}
	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zpdRetrieve6(ifltab, pds, retrieveSizeFlag);
	}

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Handle: ", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Pathname: ", pds->pathname);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d; Ending Curve: %d",
			pds->startingCurve, pds->endingCurve);
		zmessageDebug(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Starting Curve: ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d; Ending Row: %d",
			pds->startingOrdinate, pds->endingOrdinate);
		zmessageDebug(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Starting Row: ", messageString);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Retrieve size flag (0: as stored, or size 1, 2): ", retrieveSizeFlag);
	}

	status = STATUS_OKAY;
	ztransfer = zstructTransferNew(pds->pathname, 1);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating ztransfer struct");
	}


	//  If we are reading only a portion of the dataset, do not read the entire dataset
	boolReadEntire = 1;  //  Typical
	if (pds->startingCurve > 1) boolReadEntire = 0;
	if (pds->startingOrdinate > 1) boolReadEntire = 0;
	if ((pds->endingOrdinate != 0) && (pds->endingOrdinate != pds->numberOrdinates)) boolReadEntire = 0;
	if ((pds->endingCurve != 0) && (pds->endingCurve != pds->numberCurves)) boolReadEntire = 0;

	if (!boolReadEntire) {
		//  First determine what we have
		ztransfer->values3Mode = 0;
		ztransfer->userHeaderMode = 0;
		ztransfer->values1Mode = 0;
		ztransfer->values2Mode = 0;
	}
	else {
		//  Default ztransfer set to read all.
	}

	//
	status = zread(ifltab, ztransfer);
	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zpdRetrieve_ID);
		zstructFree(ztransfer);
		return status;
	}
	if (status != STATUS_RECORD_FOUND) {
		pds->numberCurves = 0;
		pds->numberCurvesInStruct = 0;
		pds->numberOrdinates = 0;
		pds->numberOrdinatesInStruct = 0;
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_GENERAL)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_NOT_EXIST, zhandle(ifltab));
			zmessage2(ifltab, messageString, pds->pathname);
		}
		zstructFree(ztransfer);
		return status;
	}
	if ((ztransfer->dataType < DATA_TYPE_PD) || (ztransfer->dataType >= DATA_TYPE_TEXT)  ||
		(ztransfer->internalHeaderNumber < INT_HEAD_pdUnits)) {
		status = zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
			zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_PD,
			(long long)ztransfer->dataType, zdssErrorSeverity.WARNING, pds->pathname, "");
		zstructFree(ztransfer);
		return status;
	}

	if (getEndian()) {
		zswitchInts(ztransfer->internalHeader, INT_HEAD_pdUnits);
		if (ztransfer->dataType == DATA_TYPE_PD) {
			if (ztransfer->values1Number > 0) {
				zswitchInts(ztransfer->values1, ztransfer->values1Number);
			}
			if (ztransfer->values2Number > 0) {
				zswitchInts(ztransfer->values2, ztransfer->values2Number);
			}
			if (ztransfer->values3Number > 0) {
				zswitchInts(ztransfer->values3, ztransfer->values3Number);
			}
		}
	}

	//  Determine if we need to convert the data size
	if (retrieveSizeFlag == 0) {
		if (ztransfer->dataType == DATA_TYPE_PD) {
			boolRetrieveDoubles = 0;
		}
		else {
			boolRetrieveDoubles = 1;
		}
	}
	else if (retrieveSizeFlag == 1) {
		boolRetrieveDoubles = 0;
	}
	else if (retrieveSizeFlag == 2) {
		boolRetrieveDoubles = 1;
	}
	//  Fill out the pd struct with what we know so far
	pds->dataType = ztransfer->dataType;
	pds->numberOrdinates = ztransfer->internalHeader[INT_HEAD_pdNumberOrdinates];
	pds->numberCurves = ztransfer->internalHeader[INT_HEAD_pdNumberCurves];
	pds->labelsLength = ztransfer->internalHeader[INT_HEAD_pdLabelsLength];
	pds->boolIndependentIsXaxis = ztransfer->internalHeader[INT_HEAD_pdBoolIndependentIsXaxis];
	stringCopy(pds->programName,  sizeof(pds->programName), ztransfer->programName, strlen(ztransfer->programName));
	pds->lastWrittenTime = ztransfer->lastWrittenTime;
	pds->fileLastWrittenTime = ztransfer->fileLastWrittenTime;
	if (ztransfer->internalHeader[INT_HEAD_pdPrecision] < 0) {
		pds->xprecision = -1;
		pds->yprecision = -1;
	}
	else {
		pds->xprecision = ztransfer->internalHeader[INT_HEAD_pdPrecision] / 10;
		pds->yprecision = ztransfer->internalHeader[INT_HEAD_pdPrecision] - (pds->xprecision  * 10);
	}

	numberValues = pds->numberOrdinates * pds->numberCurves;
	if (numberValues != ztransfer->values2Number) {
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
			zmessage(ifltab, " ");
			zmessageDebug(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Warning, miss-match between number values stored and number computed", "");
		}
	}
	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Number values read: ", ztransfer->values2Number);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Number curves: ", pds->numberCurves);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Number values: ", pds->numberOrdinates);
	}

	zpdHeadToUnits(pds, ztransfer->internalHeader, ztransfer->internalHeaderNumber);

	if (ztransfer->userHeaderNumber > 0) {
		pds->userHeaderNumber = ztransfer->userHeaderNumber;
		pds->userHeader = ztransfer->userHeader;
		ztransfer->allocated[zSTRUCT_userHeader] = 0;
		pds->allocated[zSTRUCT_userHeader] = 1;
	}
	if (ztransfer->header2Number > 0) {
//		numberBytes = ztransfer->header2Number * 4;
		number = (numberLongsInInts(ztransfer->header2Number) + 1) * 8;
		clabels = (char *)calloc((size_t)number, 1);
		charInt(ztransfer->header2, clabels, pds->labelsLength, number, 0, 0, 0);
		//  Count the real number of characters needed
		//  Local start curve variable starts at 0, where reported in pds start at 1.
		if (boolReadEntire) {
			startCurve = 0;
			endCurve = pds->numberCurves;
		}
		else {
			startCurve = pds->startingCurve - 1;
			endCurve = pds->endingCurve;
			if (startCurve < 0) startCurve = 0;
			if (endCurve < 0) endCurve = 0;
		}
		count = 0;
		ipos = 0;
		start = -1;
		for (i=0; i<endCurve; i++) {
			len = (int)strlen(&clabels[ipos]);
			if (i >= startCurve) {
				if (start < 0) start = ipos;
				count += len;
				count++;  //  null term
			}
			ipos += len +1;
			if (ipos >= pds->labelsLength)
				break;
		}
		//  Now copy the labels
		pds->labelsLength = count;
		pds->labels = (char *)calloc((size_t)pds->labelsLength, 1);
		if (!pds->labels) {
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, pds->labelsLength, 0,
									zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating labels");
		}
		pds->allocated[zSTRUCT_PD_labels] = 1;
		for (i=0; i<pds->labelsLength; i++) {
			pds->labels[i] = clabels[start + i];
		}
		free(clabels);
	}
	else {
		pds->labelsLength = 0;
		pds->labels = 0;
	}



	//  Full read
	if (boolReadEntire) {
		//  Reported start curve / ordiance starts at 1 (by convention), not 0.
		pds->startingCurve = 1;
		pds->endingCurve = pds->numberCurves;
		pds->startingOrdinate = 1;
		pds->endingOrdinate = pds->numberOrdinates;
		pds->numberOrdinatesInStruct = pds->numberOrdinates;
		pds->numberCurvesInStruct = pds->numberCurves;
		if (pds->dataType == DATA_TYPE_PD) {
			pds->sizeEachValueRead = 1;
			if (boolRetrieveDoubles) {
				pds->doubleOrdinates = (double *)calloc((size_t)pds->numberOrdinates, 8);
				pds->doubleValues = (double *)calloc((size_t)numberValues, 8);
				if (!pds->doubleOrdinates || !pds->doubleValues) {
					//  memory error
					zstructFree(ztransfer);
					return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberValues, 0,
											zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating doubleValues");
				}
				convertDataArray((void *)ztransfer->values1, (void *)pds->doubleOrdinates, pds->numberOrdinates, 1, 2);
				convertDataArray((void *)ztransfer->values2, (void *)pds->doubleValues, numberValues, 1, 2);
				pds->allocated[zSTRUCT_PD_doubleOridnates] = 1;
				pds->allocated[zSTRUCT_PD_doubleValues] = 1;
			}
			else {
				//  Move pointer to pds and change memory ownership to pds
				pds->floatOrdinates = (float *)ztransfer->values1;
				pds->floatValues = (float *)ztransfer->values2;
				ztransfer->allocated[zSTRUCT_TRANS_values1] = 0;
				ztransfer->allocated[zSTRUCT_TRANS_values2] = 0;
				pds->allocated[zSTRUCT_PD_floatOrdinates] = 1;
				pds->allocated[zSTRUCT_PD_floatValues] = 1;
			}
		}
		else if (pds->dataType == DATA_TYPE_PDD) {
			pds->sizeEachValueRead = 2;
			if (boolRetrieveDoubles) {
				//  Move pointer to pds and change memory ownership to pds
				pds->doubleOrdinates = (double *)ztransfer->values1;
				pds->doubleValues = (double *)ztransfer->values2;
				ztransfer->allocated[zSTRUCT_TRANS_values1] = 0;
				ztransfer->allocated[zSTRUCT_TRANS_values2] = 0;
				pds->allocated[zSTRUCT_PD_doubleOridnates] = 1;
				pds->allocated[zSTRUCT_PD_doubleValues] = 1;
			}
			else {
				pds->floatOrdinates = (float *)calloc((size_t)pds->numberOrdinates, 4);
				pds->floatValues = (float *)calloc((size_t)numberValues, 4);
				if (!pds->floatOrdinates || !pds->floatValues) {
					//  memory error
					zstructFree(ztransfer);
					return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberValues, 0,
											zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating floatOrdinates");
				}
				convertDataArray((void *)ztransfer->values1, (void *)pds->floatOrdinates, pds->numberOrdinates, 2, 1);
				convertDataArray((void *)ztransfer->values2, (void *)pds->floatValues, numberValues, 2, 1);
				pds->allocated[zSTRUCT_PD_floatOrdinates] = 1;
				pds->allocated[zSTRUCT_PD_floatValues] = 1;
			}
		}
		else {
			//  unknown data type
			status = zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
				zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_PD,
				(long long)ztransfer->dataType, zdssErrorSeverity.WARNING, pds->pathname, "");
			zstructFree(ztransfer);
			return status;
		}

	}
	else {	//  if (!boolReadEntire) {
		//  Partial read
		//  Get the ordinates
		//  Local start curve variable starts at 0, where reported in pds start at 1.
		startOrdinate = pds->startingOrdinate - 1;
		if (startOrdinate < 0) startOrdinate = 0;
		endOrdinate = pds->endingOrdinate;
		if ((endOrdinate == 0) || (endOrdinate > pds->numberOrdinates)){
			endOrdinate = pds->numberOrdinates;
		}
		if (startOrdinate > endOrdinate) {
			startOrdinate = 0;
		}
		pds->numberOrdinatesInStruct = endOrdinate - startOrdinate;
		if (pds->numberOrdinatesInStruct < 1) {
			//  No data!
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID, zdssErrorCodes.NO_DATA_READ,
				0, 0, zdssErrorSeverity.WARNING, pds->pathname, "");
		}
		ztransfer->values1Number = pds->numberOrdinatesInStruct;
		if (ztransfer->dataType == DATA_TYPE_PDD) {
			ztransfer->values1 = (int *)calloc((size_t)(pds->numberOrdinatesInStruct + 2), 8);
			ztransfer->values2 = (int *)calloc((size_t)(pds->numberOrdinatesInStruct + 2), 8);
			wordSize = 2;
		}
		else {
			ztransfer->values1 = (int *)calloc((size_t)(pds->numberOrdinatesInStruct + 2), 4);
			ztransfer->values2 = (int *)calloc((size_t)(pds->numberOrdinatesInStruct + 2), 4);
			wordSize = 1;
		}
		//   Allocate an additional word in case of an offset
		if (!ztransfer->values1 || !ztransfer->values2) {
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, pds->numberOrdinatesInStruct, 0,
									zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating values1 or values2");
		}
		ztransfer->values1Mode = 1;
		ztransfer->allocated[zSTRUCT_TRANS_values1] = 1;
		ztransfer->values2Mode = 1;
		ztransfer->allocated[zSTRUCT_TRANS_values2] = 1;

		if (ztransfer->dataType == DATA_TYPE_PDD) {
			address = ztransfer->info[zdssInfoKeys.kinfoValues1Address] + startOrdinate;
			offset = 0;
		}
		else {
			address = ztransfer->info[zdssInfoKeys.kinfoValues1Address] + (startOrdinate / 2);
			if (isOdd(startOrdinate)) {
				offset = 1;
			}
			else {
				offset = 0;
			}
		}
		if (offset) {
			status = zgetBuff(ifltab, address, ztransfer->values1, (pds->numberOrdinatesInStruct+1), wordSize, BUFF_READ, bufferControl, buffer);
		}
		else {
			status = zgetBuff(ifltab, address, ztransfer->values1, pds->numberOrdinatesInStruct, wordSize, BUFF_READ, bufferControl, buffer);
		}
		if (zisError(status)) {
			zstructFree(ztransfer);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpdRetrieve_ID);
		}
		//  Convert to float or double
		if (ztransfer->dataType == DATA_TYPE_PD) {
			if (getEndian()) {
				if (pds->numberOrdinatesInStruct > 0) {
					zswitchInts(ztransfer->values1, (pds->numberOrdinatesInStruct + 1));
				}
			}
			pds->sizeEachValueRead = 1;
			if (boolRetrieveDoubles) {
				pds->doubleOrdinates = (double *)calloc((size_t)pds->numberOrdinatesInStruct, 8);
				if (!pds->doubleOrdinates) {
					//  memory error
					zstructFree(ztransfer);
					return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
									zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating doubleOrdinates");
				}
				convertDataArray((void *)&ztransfer->values1[offset], (void *)pds->doubleOrdinates, pds->numberOrdinatesInStruct, 1, 2);
				pds->allocated[zSTRUCT_PD_doubleOridnates] = 1;
			}
			else {
				if (offset) {
					pds->floatOrdinates = (float *)calloc((size_t)pds->numberOrdinatesInStruct, 4);
					if (!pds->floatOrdinates) {
						//  memory error
						zstructFree(ztransfer);
						return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, pds->numberOrdinatesInStruct, 0,
									zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating floatOrdinates");
					}
					convertDataArray((void *)&ztransfer->values1[offset], (void *)pds->floatOrdinates, pds->numberOrdinatesInStruct, 1, 1);
					pds->allocated[zSTRUCT_PD_floatOrdinates] = 1;
				}
				else {
					pds->floatOrdinates = (float *)ztransfer->values1;
					ztransfer->allocated[zSTRUCT_TRANS_values1] = 0;
					pds->allocated[zSTRUCT_PD_floatOrdinates] = 1;
				}
			}
		}
		else {
			pds->sizeEachValueRead = 2;
			if (boolRetrieveDoubles) {
				pds->doubleOrdinates = (double *)ztransfer->values1;
				ztransfer->allocated[zSTRUCT_TRANS_values1] = 0;
				pds->allocated[zSTRUCT_PD_doubleOridnates] = 1;
			}
			else {
				pds->floatOrdinates = (float *)calloc((size_t)pds->numberOrdinatesInStruct, 4);
				if (!pds->floatOrdinates) {
					//  memory error
					zstructFree(ztransfer);
					return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, pds->numberOrdinatesInStruct, 0,
								zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating floatOrdinates");
				}
				convertDataArray((void *)ztransfer->values1, (void *)pds->floatOrdinates, pds->numberOrdinatesInStruct, 2, 1);
				pds->allocated[zSTRUCT_PD_floatOrdinates] = 1;
			}
		}

		//  Now each curve
		startCurve = pds->startingCurve - 1;
		if (startCurve < 0) startCurve = 0;
		endCurve = pds->endingCurve;
		if ((endCurve == 0) || (endCurve > pds->numberCurves)){
			endCurve = pds->numberCurves;
		}
		if (startCurve > endCurve) {
			startCurve = 0;
		}
		pds->numberCurvesInStruct = endCurve - startCurve;
		numberValues = pds->numberOrdinatesInStruct * pds->numberCurvesInStruct;
		//  Allocate pds memory
		if (boolRetrieveDoubles) {
			pds->doubleValues = (double *)calloc((size_t)(numberValues), DOUBLE_SIZE);
			if (!pds->doubleValues) {
				zstructFree(ztransfer);
				return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberValues, 0,
							zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating doubleValues");
			}
			pds->allocated[zSTRUCT_PD_doubleValues] = 1;
		}
		else {
			pds->floatValues = (float *)calloc((size_t)(numberValues), FLOAT_SIZE);
			if (!pds->floatValues) {
				zstructFree(ztransfer);
				return zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberValues, 0,
							zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating floatValues");
			}
			pds->allocated[zSTRUCT_PD_floatValues] = 1;
		}

		for (i=startCurve; i<endCurve; i++) {

			number = startOrdinate + (i * pds->numberOrdinates);
			if (ztransfer->dataType == DATA_TYPE_PD) {
				pds->sizeEachValueRead = 1;
				if (isOdd(number)) {
					offset = 1;
				}
				else {
					offset = 0;
				}
				number /= 2;
			}
			else {
				pds->sizeEachValueRead = 2;
				offset = 0;
			}
			numberToRead = pds->numberOrdinatesInStruct * pds->sizeEachValueRead;
			address = ztransfer->info[zdssInfoKeys.kinfoValues2Address] + (long long)number;
			status = zgetBuff(ifltab, address, ztransfer->values2, (numberToRead + offset), 1, BUFF_READ, bufferControl, buffer);
			if (zisError(status)) {
				zstructFree(ztransfer);
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpdRetrieve_ID);
			}
			//  Now Convert to float or double
			ipos = (i - startCurve) * pds->numberOrdinatesInStruct;
			if (ztransfer->dataType == DATA_TYPE_PD) {
				if (getEndian()) {
					if ((numberToRead + offset) > 0) {
						zswitchInts(ztransfer->values2, (numberToRead + offset));
					}
				}
				if (boolRetrieveDoubles) {
					convertDataArray((void *)&ztransfer->values2[offset], (void *)(&pds->doubleValues[ipos]), pds->numberOrdinatesInStruct, 1, 2);
				}
				else {
					convertDataArray((void *)&ztransfer->values2[offset], (void *)(&pds->floatValues[ipos]), pds->numberOrdinatesInStruct, 1, 1);
				}
			}
			else {
				if (boolRetrieveDoubles) {
					convertDataArray((void *)ztransfer->values2, (void *)(&pds->doubleValues[ipos]), pds->numberOrdinatesInStruct, 2, 2);
				}
				else {
					convertDataArray((void *)ztransfer->values2, (void *)(&pds->floatValues[ipos]), pds->numberOrdinatesInStruct, 2, 1);
				}
			}
		}
	}

	//  Do we need to get location information?
	if ((status == STATUS_OKAY) && pds->locationStruct) {
		zlocationRetrieve(ifltab, pds->locationStruct);

		int indElev = FALSE;
		int depElev = FALSE;
		char cPart[65];
		char *saveptr;
		char cvertical_datum[CVERTICAL_DATUM_SIZE];
		int  ivertical_datum = IVERTICAL_DATUM_UNSET;
		int  ivertical_datum2;
		verticalDatumInfo _vdi;
		verticalDatumInfo *vdi = NULL;
		char errmsg[1024];
		zpathnameGetPart(pds->pathname, 3, cPart, sizeof(cPart));
		char *cp = strtok_r(cPart, "-", &saveptr);
		if (!strncasecmp(cp, "ELEV", 4)) {
			indElev = TRUE;
		}
		cp = strtok_r(NULL, "-", &saveptr);
		if (cp && !strncasecmp(cp, "ELEV", 4)) {
			depElev = TRUE;
		}
		if (indElev || depElev) {
			//------------------------//
			// elevation in parameter //
			//------------------------//
			char cvertical_datum[CVERTICAL_DATUM_SIZE];
			int  ivertical_datum = -1;
			verticalDatumInfo _vdi;
			verticalDatumInfo *vdi = NULL;
			char *vdiStr;
			zquery("VDTM", cvertical_datum, sizeof(cvertical_datum), &ivertical_datum);
			if (ivertical_datum != IVERTICAL_DATUM_UNSET) {
				//-----------------------------------//
				// specific vertical datum requested //
				//-----------------------------------//
				if (pds->locationStruct && pds->locationStruct->supplemental) {
					vdiStr = extractFromDelimitedString(
						&pds->locationStruct->supplemental,
						VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
						":",
						TRUE,
						FALSE,
						';');
					if (vdiStr) {
						char *msg = stringToVerticalDatumInfo(&_vdi, vdiStr);
						if (msg) {
							char _msg[65];
							strncpy(_msg, msg, sizeof(_msg)-1);
							sprintf(
								errmsg,
								"Cannot convert from native vertical datum of '%s' to '%s'.\n%s\n"
								"No values retrieved.",
								pds->locationStruct->verticalDatum == 0 ? "UNSET"   :
								pds->locationStruct->verticalDatum == 1 ? "NAVD-88" :
								pds->locationStruct->verticalDatum == 2 ? "NGVD-29" : "OTHER",
								cvertical_datum,
								_msg);
							status = zerrorProcessing(
								ifltab,
								DSS_FUNCTION_zpdRetrieve_ID,
								zdssErrorCodes.INVALID_HEADER_PARAMETER,
								0,
								0,
								zdssErrorSeverity.WARNING,
								pds->pathname,
								errmsg);
							return status;
						}
						else {
							vdi = &_vdi;
						}
						free(vdiStr);
					}
				}
				if (vdi == NULL) {
					sprintf(
						errmsg,
						"Cannot convert from native vertical datum of '%s' to '%s'.\n"
						"Record has no conversion information.\n"
						"No values retrieved.",
						pds->locationStruct->verticalDatum == 0 ? "UNSET"   :
						pds->locationStruct->verticalDatum == 1 ? "NAVD-88" :
						pds->locationStruct->verticalDatum == 2 ? "NGVD-29" : "OTHER",
						cvertical_datum);
						status = zerrorProcessing(
							ifltab,
							DSS_FUNCTION_zpdRetrieve_ID,
							zdssErrorCodes.INVALID_HEADER_PARAMETER,
							0,
							0,
							zdssErrorSeverity.WARNING,
							pds->pathname,
							errmsg);
						return status;
				}
				else {
					//---------------------------------------------------------------//
					// ensure the vertical datum info is returned in the user header //
					//---------------------------------------------------------------//
					verticalDatumInfoToString(&vdiStr, vdi, TRUE);
					char *headerString;
					if (pds->userHeader) {
						headerString = userHeaderToString(pds->userHeader, pds->userHeaderNumber);
						//------------------------------------------------------//
						// it should already be in the header, but just in case //
						//------------------------------------------------------//
						if (!strstr(headerString, VERTICAL_DATUM_INFO_USER_HEADER_PARAM)) {
							headerString = (char *)realloc(
								headerString,
								strlen(headerString)
								+ VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN
								+ strlen(vdiStr)
								+ 3);
							sprintf(
								headerString+strlen(headerString), 
								"%s:%s;", 
								VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
								vdiStr);
							free(pds->userHeader);
						}
					}
					else {
						headerString = (char *)malloc(
							VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN
							+ strlen(vdiStr)
							+ 3);
						sprintf(
							headerString, 
							"%s:%s;", 
							VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
							vdiStr);
					}
					free(vdiStr);
					double ind_offset = UNDEFINED_VERTICAL_DATUM_VALUE;
					double dep_offset = UNDEFINED_VERTICAL_DATUM_VALUE;
					if (indElev) {
						//-----------------------------//	
						// determine the offset to use //
						//-----------------------------//	
						switch(ivertical_datum) {
							case IVERTICAL_DATUM_NAVD88 :
								ind_offset = vdi->offsetToNavd88;
								break;
							case IVERTICAL_DATUM_NGVD29 :
								ind_offset = vdi->offsetToNgvd29;
								break;
							default :
								if(!strcmp(cvertical_datum, vdi->nativeDatum) || !strcmp(cvertical_datum, CVERTICAL_DATUM_OTHER)) {
									ind_offset = 0;
								}
								else {
									ind_offset = UNDEFINED_VERTICAL_DATUM_VALUE;
								}
								break;
						}
						if (ind_offset != 0.) {
							if (ind_offset == UNDEFINED_VERTICAL_DATUM_VALUE) {
								sprintf(
									errmsg,
									"\nVertical datum offset is undefined for datum '%s'.\n"
									"Conversion to datum '%s' could not be performed.\n"
									"No values retrieved.",
									cvertical_datum,
									cvertical_datum);
								status = zerrorProcessing(
									ifltab,
									DSS_FUNCTION_zpdRetrieve_ID,
									zdssErrorCodes.INVALID_HEADER_PARAMETER,
									0,
									0,
									zdssErrorSeverity.WARNING,
									pds->pathname,
									errmsg);
								return status;
							}
							else {
								ind_offset = getOffset(ind_offset, vdi->unit, pds->unitsIndependent);
								if (ind_offset == UNDEFINED_VERTICAL_DATUM_VALUE) {
									sprintf(
										errmsg,
										"\nData unit (%s) and/or offset unit (%s) is invalid for vertical datum conversion.\n"
										"Conversion to datum '%s' could not be performed.\n"
										"No values retrieved.",
										pds->unitsIndependent,
										vdi->unit,
										cvertical_datum);
									status = zerrorProcessing(
										ifltab,
										DSS_FUNCTION_zpdRetrieve_ID,
										zdssErrorCodes.INVALID_HEADER_PARAMETER,
										0,
										0,
										zdssErrorSeverity.WARNING,
										pds->pathname,
										errmsg);
									return status;
								}
							}
							//------------------------------//
							// add the offset to the values //
							//------------------------------//
							if (pds->doubleOrdinates) {
								for (int i = 0; i < pds->numberOrdinates; ++i) {
									pds->doubleOrdinates[i] += ind_offset;
								}
							}
							else if (pds->floatOrdinates) {
								for (int i = 0; i < pds->numberOrdinates; ++i) {
									pds->floatOrdinates[i] += ind_offset;
								}
							}
						}
					}
					if (depElev) {
						//-----------------------------//	
						// determine the offset to use //
						//-----------------------------//	
						switch(ivertical_datum) {
							case IVERTICAL_DATUM_NAVD88 :
								dep_offset = vdi->offsetToNavd88;
								break;
							case IVERTICAL_DATUM_NGVD29 :
								dep_offset = vdi->offsetToNgvd29;
								break;
							default :
								if(!strcmp(cvertical_datum, vdi->nativeDatum) || !strcmp(cvertical_datum, CVERTICAL_DATUM_OTHER)) {
									dep_offset = 0;
								}
								else {
									dep_offset = UNDEFINED_VERTICAL_DATUM_VALUE;
								}
								break;
						}
						if (dep_offset != 0.) {
							if (dep_offset == UNDEFINED_VERTICAL_DATUM_VALUE) {
								sprintf(
									errmsg,
									"\nVertical datum offset is undefined for datum '%s'.\n"
									"Conversion to datum '%s' could not be performed.\n"
									"No values retrieved.",
									cvertical_datum,
									cvertical_datum);
								if (ind_offset != UNDEFINED_VERTICAL_DATUM_VALUE && ind_offset != 0.) {
									if (pds->doubleOrdinates) {
										for (int i = 0; i < pds->numberOrdinates; ++i) {
											pds->doubleOrdinates[i] -= ind_offset;
										}
									}
									else if (pds->floatOrdinates) {
										for (int i = 0; i < pds->numberOrdinates; ++i) {
											pds->floatOrdinates[i] -= ind_offset;
										}
									}
								}
								status = zerrorProcessing(
									ifltab,
									DSS_FUNCTION_zpdRetrieve_ID,
									zdssErrorCodes.INVALID_HEADER_PARAMETER,
									0,
									0,
									zdssErrorSeverity.WARNING,
									pds->pathname,
									errmsg);
								return status;
							}
							else {
								dep_offset = getOffset(dep_offset, vdi->unit, pds->unitsDependent);
								if (dep_offset == UNDEFINED_VERTICAL_DATUM_VALUE) {
									sprintf(
										errmsg,
										"\nData unit (%s) and/or offset unit (%s) is invalid for vertical datum conversion.\n"
										"Conversion to datum '%s' could not be performed.\n"
										"No values retrieved.",
										pds->unitsDependent,
										vdi->unit,
										cvertical_datum);
									if (ind_offset != UNDEFINED_VERTICAL_DATUM_VALUE && ind_offset != 0.) {
										if (pds->doubleOrdinates) {
											for (int i = 0; i < pds->numberOrdinates; ++i) {
												pds->doubleOrdinates[i] -= ind_offset;
											}
										}
										else if (pds->floatOrdinates) {
											for (int i = 0; i < pds->numberOrdinates; ++i) {
												pds->floatOrdinates[i] -= ind_offset;
											}
										}
									}
									status = zerrorProcessing(
										ifltab,
										DSS_FUNCTION_zpdRetrieve_ID,
										zdssErrorCodes.INVALID_HEADER_PARAMETER,
										0,
										0,
										zdssErrorSeverity.WARNING,
										pds->pathname,
										errmsg);
									return status;
								}
							}
							//------------------------------//
							// add the offset to the values //
							//------------------------------//
							if (pds->doubleValues) {
								for (int i = 0; i < pds->numberOrdinates * pds->numberCurves; ++i) {
									pds->doubleValues[i] += dep_offset;
								}
							}
							else if (pds->floatValues) {
								for (int i = 0; i < pds->numberOrdinates * pds->numberCurves; ++i) {
									pds->floatValues[i] += dep_offset;
								}
							}
						}
					}
					//--------------------------------------------//
					// add the requested datum to the user header //
					//--------------------------------------------//
					headerString = (char *)realloc(
						headerString, 
						strlen(headerString)
						+ VERTICAL_DATUM_USER_HEADER_PARAM_LEN
						+ strlen(cvertical_datum)
						+ 3);
					sprintf(
						headerString+strlen(headerString),
						"%s:%s;", 
						VERTICAL_DATUM_USER_HEADER_PARAM,
						cvertical_datum);
					pds->userHeader = stringToUserHeader(headerString, &pds->userHeaderNumber);
					pds->allocated[zSTRUCT_userHeader] = TRUE;
					free(headerString);
				}
			}
		}
	}
	zstructFree(ztransfer);
	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Finished.  Pathname: ", pds->pathname);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d; Number of Ordinates: %d",
			pds->numberCurves, pds->numberOrdinates);
		zmessageDebug(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Number of Curves in struct: ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d; Number of Ordinates in struct: %d",
			pds->numberCurvesInStruct, pds->numberOrdinatesInStruct);
		zmessageDebug(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Number of Curves: ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d; Ending Curve: %d",
			pds->startingCurve, pds->endingCurve);
		zmessageDebug(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Starting Curve: ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d; Ending Row: %d",
			pds->startingOrdinate, pds->endingOrdinate);
		zmessageDebug(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Starting Row: ", messageString);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Exit, status: ", status);
	}

	return status;
}


