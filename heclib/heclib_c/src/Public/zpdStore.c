#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "heclib.h"
#include "zerrorCodes.h"

/**
*  Function:	zpdStore
*
*  Use:			Public
*
*  Description:	Primary function to store paired data
*
*  Declaration: int zpdStore(long long *ifltab, zStructPairedData *pds, int storageFlag);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*					See zopen() for more information.
*
*				zStructPairedData *pds
*					A struct that needs to contain all data and information related to this data set to store.
*					This struct must be created by one of the Paired Data Struct new methods; they are:
*						zStructPairedData* zstructPdNew(const char* pathname);		Usually for retrieving data
*						zStructPairedData* zstructPdNewDoubles(const char* pathname, double *doubleOrdinates, double *doubleValues,
*							 int numberOrdinates, int numberCurves, const char *unitsIndependent, const char *typeIndependent,
*							 const char *unitsDependent, const char *typeDependent);
*						zStructPairedData* zstructPdNewFloats(const char* pathname, float *floatOrdinates, float *floatValues,
*							 int numberOrdinates, int numberCurves, const char *unitsIndependent, const char *typeIndependent,
*							 const char *unitsDependent, const char *typeDependent);
*					When the store is complete, the struct must be freed by a call to
*						void zstructFree(zStructPairedData *pds)
*					NEVER REUSE A zStructPairedData, always free and create a new on.
*
*				int storageFlag
*					A flag indicating how to store data:
*						storageFlag = 0  Normal write, store data as either floats or doubles, depending on which arrays are used in struct
*						storageFlag = 1  Store as floats, regardless of the arrays in the struct.
*						storageFlag = 2  Store as doubles, regardless of the arrays in the struct.
*						storageFlag = 10 Pre-allocate space for storing a large family of curves.  Array type determines float or double
*						storageFlag = 11 Pre-allocate space for storing a large family of float curves.
*						storageFlag = 12 Pre-allocate space for storing a large family of double curves.
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*
*	Remarks:
*				Parameters within zStructPairedData allow you to store just one or a set of curves,
*					a set of rows, or a block of data (rows/curves). See struct description below
*					Space MUST BE PRE-ALLOCATED when storing only parts of the record.
*				The "independent variable" is the name of the parameter for the oridnate set.
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
*					The pathname of the record to store.  The "C" part is to contain the name of the "independent variable",
*					followed by a dash, then the "dependent variable" name; e.g., "Stage-Flow"
*
*				float *floatOrdinates
*					The float ordinate array containing the "independent variable" data.  If storing doubles, this must be zero.
*			or
*				double *doubleOrdinates
*					The double ordinate array containing the "independent variable" data.  If storing floats, this must be zero.
*
*				float *floatValues
*					A single or double dimensioned float array containing the "dependent variable" data.  If double dimensioned,
*					then the second curve follows the first curve (not first row, then second row).  Must match ordinate type.
*			or
*				double *doubleValues
*					A single or double dimensioned double array containing the "dependent variable" data.  If double dimensioned,
*					then the second curve follows the first curve (not first row, then second row).  Must match ordinate type.
*
*					The dimensions of the values array must be consistent with numberCurves; i.e., doubleValues[numberCurves][numberOrdinates]
*
*				int numberOrdinates
*					Number of values in the ordinate array.  This can also be thought of as the number of rows.
*
*				int numberCurves
*					Number of curves or columns, often one.  If the function was a "Stage-Damage" curve, and the curves
*					were "Total", "Residential" and "Commercial", then this would be "3", and the residential values would
*					follow (all of) the total values, and the commercial curve would follow the residential curve.
*
*				const char *unitsIndependent
*					The units of the independent data (ordinates), such as "Feet".
*					The number of characters not limited, but generally should not be greater than 25 for showing up nicely in tables.
*
*				const char *typeIndependent
*					Defines the ordinates type and can be "Linear", "Log", or "Probability".
*					The number of characters is not limited, but generally should not be greater than 25.
*
*				const char *unitsDependent
*					The units of the dependent data (curves), such as "Dollars".
*					The number of characters not limited.
*
*				const char *typeDependentt
*					The curve type.  The type can be "Linear", "Log", or "Probability".
*					The number of characters is not limited.
*
*
*
*	Optional (Set after new):
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
*					The number of characters in labels.  If zero, no labels will be stored.
*
*
*  For Storing part of a data set:
*
*				If you have allocated space for a data set, or have an existing data set, you can
*				store values, such as a curve, a set of curves, a row or set of rows, a block or an individual value
*				For example, if you wanted to replace an individual value, put the number in the values array and set
*				startingOrdinate = endingOrdinate, and startingCurve = endingCurve (remember, the first row is "row 1", not "row 0")
*				When setting part of a data set, the ordinate array is ignored, and the values array must correspond to
*				to these rows and curve numbers.
*
*
*				NOTE:  CURVES AND ORDINATES START AT ONE (1), NOT ZERO
*				int startingCurve
*					The starting curve number (starting at 1) for the data you are replacing/storing.
*					To ignore (.e.g, replacing an entire row), set to zero (the default)
*
*				int endingCurve
*					The ending curve number for the data you are replacing/storing.  Set to "0" to ignore.
*
*				int startingOrdinate
*					The starting row (starting at 1) for the data you are replacing/storing.  Set to "0" to ignore.
*
*				int endingOrdinate
*					The ending row for the data you are replacing/storing.  Set to "0" to ignore.
*
*
*  Other
*
*				char *timeZoneName
*					The time zone name for this data set.  This is not necessarily the same as the location time zone,
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

int zpdStore(long long *ifltab, zStructPairedData *pds, int storageFlag)
{

	int numberValues;
	int numberOrdinates;
	int numberCurves;
	int numberRows;
	int number;
	int status;
	int dataType;
	int boolStoreDoubles;
	int startOrdinate;
	int endOrdinate;
	int startCurve;
	int endCurve;
	int boolStoreEntire;
	int offset;
	int ipos;
	int i;
	int j;
	int numberBytes;
	int labelsLength;
	int len;
	int zero;
	long long address;

	int iposRead;
	int iposPds;
	int curveNumber;
	int originalSize;
	int newSize;
	int len1;
	int len2;

	int internalHeader[INT_HEAD_SIZE];
	int internalHeaderNumber = INT_HEAD_SIZE;
	int *internalHeader2;
	int lengthInternalHeader2;

	int valueSize;
	int sizeOrdinates;
	int sizeValues;
	int *ordinates;
	int *values;
	char *clabels=0;
	char *clabelsRead=0;

	char messageString[100];
	zStructTransfer* ztransfer;

	int buffer[1]={0}; long long bufferControl[4] ={0,0,0,0};


	if (!pds) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "zStructPairedData is null");
	}
	if (!pds->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "zStructPairedData pathname is null");
	}

	boolStoreEntire = 1;  //  Typical
	if (pds->startingCurve > 1) boolStoreEntire = 0;
	if (pds->startingOrdinate > 1) boolStoreEntire = 0;
	if ((pds->endingOrdinate != 0) && (pds->endingOrdinate != pds->numberOrdinates)) boolStoreEntire = 0;
	if ((pds->endingCurve != 0) && (pds->endingCurve != pds->numberCurves)) boolStoreEntire = 0;
	if ((pds->startingOrdinate == 0) && (pds->endingOrdinate == 0)) {
		if (pds->startingCurve != 0) boolStoreEntire = 0;
		if (pds->endingCurve != 0) boolStoreEntire = 0;
	}

	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		if (boolStoreEntire) {
			//  Be sure we have units, etc. defined.
			if (!pds->unitsDependent) {
				pds->unitsDependent = '\0';
			}
			if (!pds->typeDependent) {
				pds->typeDependent = '\0';
			}
			if (!pds->unitsIndependent) {
				pds->unitsIndependent = '\0';
			}
			if (!pds->typeIndependent) {
				pds->typeIndependent = '\0';
			}
		}
		return zpdStore6(ifltab, pds, storageFlag);
	}

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "Handle: ", zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "storageFlag: ", storageFlag);
		zmessageDebug(ifltab, DSS_FUNCTION_zpdStore_ID, "Pathname: ", pds->pathname);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d; Number of Ordinates: %d",
			pds->numberCurves, pds->numberOrdinates);
		zmessageDebug(ifltab, DSS_FUNCTION_zpdStore_ID, "Number of Curves: ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d; Ending Curve: %d",
			pds->startingCurve, pds->endingCurve);
		zmessageDebug(ifltab, DSS_FUNCTION_zpdStore_ID, "Starting Curve: ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d; Ending Row: %d",
			pds->startingOrdinate, pds->endingOrdinate);
		zmessageDebug(ifltab, DSS_FUNCTION_zpdStore_ID, "Starting Row: ", messageString);
		if (pds->unitsIndependent) {
			zmessageDebug(ifltab, DSS_FUNCTION_zpdStore_ID, "unitsIndependent: ", pds->unitsIndependent);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zpdStore_ID, "NO unitsIndependent ", "");
		}
		if (pds->typeIndependent) {
			zmessageDebug(ifltab, DSS_FUNCTION_zpdStore_ID, "typeIndependent: ", pds->typeIndependent);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zpdStore_ID, "NO typeIndependent ", "");
		}
	}

	if (!zinquire(ifltab, "write")) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
			zdssErrorCodes.WRITE_ON_READ_ONLY, 0, 0,
			zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, pds->pathname, "");
	}


	//  Normal case first - write the full record
	//  (If the record already exists, overwrite it)
	if (boolStoreEntire) {
		//  Determine if we will be writing floats or doubles
		boolStoreDoubles = -1;
		if (storageFlag == 1) {
			boolStoreDoubles = 0;
		}
		else if (storageFlag == 2) {
			boolStoreDoubles = 1;
		}
		else if (storageFlag == 11) {
			boolStoreDoubles = 0;
		}
		else if (storageFlag == 12) {
			boolStoreDoubles = 1;
		}
		else {
			if (pds->floatOrdinates) {
				boolStoreDoubles = 0;
			}
			else if (pds->doubleOrdinates) {
				boolStoreDoubles = 1;
			}
			else {
				//  No ordinates given - error our
				return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
										zdssErrorCodes.NO_DATA_GIVEN, 0, 0,
										zdssErrorSeverity.WARNING, pds->pathname, "");
			}
		}

		if (boolStoreDoubles == 0) {
			valueSize = 1;
			dataType = DATA_TYPE_PD;
		}
		else if (boolStoreDoubles == 1) {
			valueSize = 2;
			dataType = DATA_TYPE_PDD;
		}
		else {
			//  Cannot get here....
			return STATUS_NOT_OKAY;
		}
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "boolStoreDoubles: ", boolStoreDoubles);
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "valueSize: ", valueSize);
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "dataType: ", dataType);
		}

		internalHeader[INT_HEAD_pdPrecision] = 0;
		internalHeader[INT_HEAD_pdNumberOrdinates] = pds->numberOrdinates;
		internalHeader[INT_HEAD_pdNumberCurves] = pds->numberCurves;
		internalHeader[INT_HEAD_pdBoolIndependentIsXaxis] = pds->boolIndependentIsXaxis;
		internalHeaderNumber = zpdUnitsToHead(pds, internalHeader, internalHeaderNumber);
		if (pds->xprecision < 0) {
			internalHeader[INT_HEAD_pdPrecision] = -1;
		}
		else {
			internalHeader[INT_HEAD_pdPrecision] = (pds->xprecision * 10) + pds->yprecision;
		}		
		numberValues = pds->numberCurves * pds->numberOrdinates;
		if (numberValues <= 0) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
									zdssErrorCodes.NO_DATA_GIVEN, numberValues, 0,
									zdssErrorSeverity.WARNING, pds->pathname, "");
		}

		//  Store labels in internalheader2
		lengthInternalHeader2 = 0;
		internalHeader2 = 0;
		labelsLength = pds->labelsLength;
		if (pds->labelsLength > 0) {
			//  Get the size of the label array and the number of labels
			//  (number of labels should equal number of curves,
			//  although we won't call this an error.)
			numberBytes = pds->labelsLength;
			//  numberBytes is the size of the label array.
			//  Allocate integer space and shove it in
			//  charInt doesn't stop on null chars
			lengthInternalHeader2 = numberLongsInBytes(numberBytes) * 2;
			internalHeader2 = (int *)calloc((size_t)lengthInternalHeader2 + 2, 4);
			///////////   Fix me, check for memory allocation
			//  copy the character labels into the integer array
			//  (BTW, this MUST be done instead of a cast to preserve big endian, little endian compatiblity!)
			charLong((void *)pds->labels, internalHeader2, numberBytes, (lengthInternalHeader2 * 4), 1, -1);			
		}
		internalHeader[INT_HEAD_pdLabelsLength] = pds->labelsLength;

		internalHeaderNumber = zpdUnitsToHead(pds, internalHeader, internalHeaderNumber);
		ztransfer = zstructTransferNew(pds->pathname, 0);
		if (!ztransfer) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
									zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating ztransfer struct");
		}
		if (getEndian()) {
			zswitchInts(internalHeader, INT_HEAD_pdPrecision);
		}
		ztransfer->internalHeader = internalHeader;
		ztransfer->internalHeaderNumber = internalHeaderNumber;
		ztransfer->header2 = internalHeader2;
		ztransfer->header2Number = lengthInternalHeader2;
		ztransfer->userHeader = pds->userHeader;
		ztransfer->userHeaderNumber = pds->userHeaderNumber;
		ztransfer->numberValues = (pds->numberCurves + 1) * pds->numberOrdinates;
		ztransfer->dataType = dataType;

		sizeOrdinates = pds->numberOrdinates * valueSize;
		sizeValues = pds->numberCurves * pds->numberOrdinates * valueSize;

		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "sizeOrdinates: ", sizeOrdinates);
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "sizeValues: ", sizeValues);
		}

		ordinates = 0;
		values = 0;
		ztransfer->values1Number = sizeOrdinates;
		if (storageFlag > 5) {
			zero = 0;
			ztransfer->values2 = &zero;
			ztransfer->values2Number = -sizeValues;
		}
		else {
			ztransfer->values2Number = sizeValues;
		}

		//  Do we need to convert doubles to floats or visa versa?
		if (pds->floatOrdinates) {
			if (boolStoreDoubles) {
				//  Need to convert floats to doubles
				ordinates = (int *)calloc((size_t)pds->numberOrdinates, DOUBLE_SIZE);
				if (!ordinates) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, sizeOrdinates, 0,
											zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating ordinates");
				}
				convertDataArray((void *)pds->floatOrdinates, (void *)ordinates,  pds->numberOrdinates, 1, 2);
				ztransfer->values1 = ordinates;
				if (storageFlag < 6) {
					values = (int *)calloc((size_t)numberValues, DOUBLE_SIZE);
					if (!values) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, sizeValues, 0,
												zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating values");
					}
					convertDataArray((void *)pds->floatValues, (void *)values,  numberValues, 1, 2);
					ztransfer->values2 = values;
				}
			}
			else {
				//  Just floats
				if (!getEndian()) {
					ztransfer->values1 = (int *)pds->floatOrdinates;
					if (storageFlag < 6) {
						ztransfer->values2 = (int *)pds->floatValues;
					}
				}
				else {
					//  Don't swap the calling program's data - make a local copy
					ztransfer->values1 = (int *)calloc((size_t)pds->numberOrdinates, FLOAT_SIZE);
					convertDataArray((void *)pds->floatOrdinates, (void *)ztransfer->values1, pds->numberOrdinates, 1, 1);
					ztransfer->allocated[zSTRUCT_TRANS_values1] = 1;
					if (storageFlag < 6) {
						ztransfer->values2 = (int *)calloc((size_t)numberValues, FLOAT_SIZE);
						convertDataArray((void *)pds->floatValues, (void *)ztransfer->values2, numberValues, 1, 1);
						ztransfer->allocated[zSTRUCT_TRANS_values2] = 1;
					}
				}
			}
		}
		else if (pds->doubleOrdinates) {
			if (!boolStoreDoubles) {
				//  Need to convert doubles to floats
				number = numberLongsInInts(pds->numberOrdinates) * 2;
				ordinates = (int *)calloc((size_t)number, FLOAT_SIZE);
				if (!ordinates) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, sizeOrdinates, 0,
											zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating ordinates");
				}
				convertDataArray((void *)pds->doubleOrdinates, (void *)ordinates,  pds->numberOrdinates, 2, 1);
				ztransfer->values1 = ordinates;
				if (storageFlag < 6) {
					number = numberLongsInInts(numberValues) * 2;
					values = (int *)calloc((size_t)number, FLOAT_SIZE);
					if (!values) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, sizeValues, 0,
												zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating values");
					}
					convertDataArray((void *)pds->doubleValues, (void *)values,  numberValues, 2, 1);
					ztransfer->values2 = values;
				}
			}
			else {
				ztransfer->values1 = (int *)pds->doubleOrdinates;
				if (storageFlag < 6) {
					ztransfer->values2 = (int *)pds->doubleValues;
				}
			}
		}

		if (!boolStoreDoubles) {
			if (getEndian()) {
				zswitchInts(ztransfer->values1, ztransfer->values1Number);
				zswitchInts(ztransfer->values2, ztransfer->values2Number);
			}
		}
		if (boolStoreEntire)
		{
			int diskType = zdataType(ifltab,pds->pathname );
			if (diskType > 0 && diskType != dataType) // exists and not the same dataType
			{
				zdelete(ifltab, pds->pathname);
			}
		}

		status = zwrite(ifltab, ztransfer);

		if (internalHeader2) {
			free(internalHeader2);
			internalHeader2 = 0;
		}
		if (ordinates) {
			free(ordinates);
			ordinates = 0;
		}
		if (values) {
			free(values);
			values = 0;
		}

		zstructFree(ztransfer);

	}
	else {  // if (!boolStoreEntire) {
		//  Write a portion over an existing data set.
		//  Ignore ordinates, labels, etc.

		//  First determine what we have on disk
		ztransfer = zstructTransferNew(pds->pathname, 0);
		if (!ztransfer) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
									zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating ztransfer struct");
		}
		ztransfer->internalHeaderMode = 1;
		ztransfer->header2Mode = 1;
		ztransfer->values3Mode = 0;
		ztransfer->userHeaderMode = 0;
		ztransfer->values1Mode = 0;
		ztransfer->values2Mode = 0;
		//  Lock the file at a high level
		status =  zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpdStore_ID);
		}
		status = zreadInternal(ifltab, ztransfer, bufferControl, buffer, 0);
		if (zisError(status)) {
			zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpdStore_ID);
		}
		if ((ztransfer->dataType != DATA_TYPE_PD) && (ztransfer->dataType != DATA_TYPE_PDD)) {
			status = zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
				zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_PD,
				(long long)ztransfer->dataType, zdssErrorSeverity.WARNING, ztransfer->pathname, "");
			zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			return status;
		}

		if (ztransfer->dataType == DATA_TYPE_PD) {
			boolStoreDoubles = 0;
		}
		else {
			boolStoreDoubles = 1;
		}
		dataType = ztransfer->dataType;
		if (getEndian()) {
			zswitchInts(ztransfer->internalHeader, INT_HEAD_pdPrecision);
		}
		numberOrdinates = ztransfer->internalHeader[INT_HEAD_pdNumberOrdinates];
		numberCurves = ztransfer->internalHeader[INT_HEAD_pdNumberCurves];
		numberValues = numberOrdinates * numberCurves;

		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "Data set read, data type: ", dataType);
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "numberOrdinates: ", numberOrdinates);
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "numberCurves: ", numberCurves);
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "numberValues: ", numberValues);
		}

		//  Get the ordinates
		//  Local start curve variable starts at 0, where reported in pds start at 1.
		startOrdinate = pds->startingOrdinate - 1;
		if (startOrdinate < 0) startOrdinate = 0;
		endOrdinate = pds->endingOrdinate;
		if ((endOrdinate == 0) || (endOrdinate > numberOrdinates)){
			endOrdinate = numberOrdinates;
		}
		if (startOrdinate > endOrdinate) {
			startOrdinate = 0;
		}
		numberRows = endOrdinate - startOrdinate;

		//  Now each curve
		startCurve = pds->startingCurve - 1;
		if (startCurve < 0) startCurve = 0;
		endCurve = pds->endingCurve;
		if ((endCurve == 0) || (endCurve > numberCurves)){
			endCurve = numberCurves;
		}
		if (startCurve > endCurve) {
			startCurve = 0;
		}
		numberCurves = endCurve - startCurve;

		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "numberRows: ", numberRows);
			zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "numberCurves: ", numberCurves);
		}
		//  Allocate memory
		if (boolStoreDoubles) {
			ztransfer->values2 = (int *)calloc((size_t)(numberRows + 2), DOUBLE_SIZE);
		}
		else {
			ztransfer->values2 = (int *)calloc((size_t)(numberRows + 2), FLOAT_SIZE);
		}
		if (!ztransfer->values2) {
			zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberRows, 0,
									zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating values");
		}
		ztransfer->allocated[zSTRUCT_TRANS_values2] = 1;

		for (i=startCurve; i<endCurve; i++) {
			number = startOrdinate + (i * numberOrdinates);
			if (dataType == DATA_TYPE_PD) {
				if (isOdd(number)) {
					offset = 1;
				}
				else {
					offset = 0;
				}
				number /= 2;
			}
			else {
				offset = 0;
			}
			address = ztransfer->info[zdssInfoKeys.kinfoValues2Address] + (long long)number;
			if (offset) {
				status = zgetBuff(ifltab, address, ztransfer->values2, 2, 1, BUFF_READ, bufferControl, buffer);
				if (zisError(status)) {
					zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
					return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInternal_ID);
				}
				if (dataType == DATA_TYPE_PD) {
					if (getEndian()) {
						zswitchInts(ztransfer->values2, 2);
					}
				}
			}
			//  Now Convert to float or double
			ipos = (i - startCurve) * numberRows;
			if (pds->floatValues) {
				if (boolStoreDoubles) {
					convertDataArray((void *)(&pds->floatValues[ipos]), (void *)&ztransfer->values2[offset],  numberRows, 1, 2);
					sizeValues = 2;
				}
				else {
					convertDataArray((void *)(&pds->floatValues[ipos]), (void *)&ztransfer->values2[offset],  numberRows, 1, 1);
					sizeValues = 1;
				}
			}
			else if (pds->doubleValues) {
				if (boolStoreDoubles) {
					convertDataArray((void *)(&pds->doubleValues[ipos]), (void *)&ztransfer->values2[offset],  numberRows, 2, 2);
					sizeValues = 2;
				}
				else {
					convertDataArray((void *)(&pds->doubleValues[ipos]), (void *)&ztransfer->values2[offset],  numberRows, 2, 1);
					sizeValues = 1;
				}
			}
			else {
				zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
				return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
									zdssErrorCodes.NO_DATA_GIVEN, 0, 0,
									zdssErrorSeverity.WARNING, pds->pathname, "No values provided");
			}
			if (!boolStoreDoubles) {
				if (getEndian()) {
					zswitchInts(ztransfer->values2, (numberRows + offset));
				}
			}
			status = zputBuff(ifltab, address, ztransfer->values2, (numberRows + offset), sizeValues, BUFF_WRITE, bufferControl, buffer);

		}

		//  If labels are being stored, add them in
		//  len = pds->labelsLength  internalHeader[INT_HEAD_pdLabelsLength] = pds->labelsLength;
		if (pds->labelsLength > 0) {
			len = pds->labelsLength + ztransfer->internalHeader[INT_HEAD_pdLabelsLength];
			clabels = (char *)calloc((size_t)len, 1);
			clabelsRead = (char*)calloc(ztransfer->internalHeader[INT_HEAD_pdLabelsLength], 1);
			charInt(ztransfer->header2, clabelsRead, ztransfer->internalHeader[INT_HEAD_pdLabelsLength], ztransfer->internalHeader[INT_HEAD_pdLabelsLength], 0, 0, 0);
			ipos = 0;
			iposRead = 0;
			iposPds = 0;
			curveNumber = 1;
			for (i=0; i<len; i++) {
				//  Are we at a curve to add our label?
				if ((curveNumber >= pds->startingCurve) && (curveNumber <= pds->endingCurve)) {
					len1 = (int)strlen(&pds->labels[iposPds]);
					for (j=0; j<len1; j++) {
						clabels[ipos++] = pds->labels[iposPds++];
					}
					clabels[ipos++] = '\0';
					len2 = (int)strlen(&clabelsRead[iposRead]);
					iposRead += len2 + 1;
					curveNumber++;
				}
				else {
					clabels[ipos] = clabelsRead[iposRead++];
					if (clabels[ipos] == '\0') curveNumber++;
					ipos++;
				}
				if (curveNumber > ztransfer->internalHeader[INT_HEAD_pdNumberCurves]) {
					break;
				}
				if (ipos >= len) {
					break;
				}
				if (iposRead >= (ztransfer->header2Number * 4)) {
					break;
				}
			}
			if (clabelsRead) free(clabelsRead);
			clabelsRead = 0;
			originalSize = (int)ztransfer->info[zdssInfoKeys.kinfoHeader2Number];
			if (ipos > ztransfer->internalHeader[INT_HEAD_pdLabelsLength]) {
				newSize = numberIntsInBytes(ipos);
				ztransfer->internalHeader[INT_HEAD_pdLabelsLength] = ipos;
			}
			else {
				newSize = originalSize;
			}
			if (newSize > originalSize) {
				//  This is a pretty significant issue; paired data records are not designed
				//  to be efficient at expansion
				//  This may be a large data set and re-writing a expanded set may take a lot of resources
				//  Add additional space to the label array, to minimize future expansions
				//  First off, did the calling program just forget to allocate label space?
				if (originalSize < 8) {
					// Looks like it.  Estimate 10 characters per label and add space
					newSize += ztransfer->internalHeader[INT_HEAD_pdNumberCurves] * 10;
				}
				else {
					//  Probably just guessed too low
					if (originalSize < 101) {
						// Bump up by 50
						newSize += 50;
					}
					else {
						//  Pretty big array; just add 25%
						newSize += newSize/4;
					}
				}
				//  What's on disk is good, except for the label array
				//  Read it, put in new label array, then write (with expanded array)
				zstructFree(ztransfer);
				ztransfer = zstructTransferNew(pds->pathname, 1);
				if (!ztransfer) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
											zdssErrorSeverity.MEMORY_ERROR, pds->pathname, "Allocating ztransfer struct");
				}
				status = zreadInternal(ifltab, ztransfer, bufferControl, buffer, 0);
				if (zisError(status)) {
					zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
					return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpdStore_ID);
				}
				if ((ztransfer->dataType != DATA_TYPE_PD) && (ztransfer->dataType != DATA_TYPE_PDD)) {
					status = zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID,
						zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_PD,
						(long long)ztransfer->dataType, zdssErrorSeverity.WARNING, ztransfer->pathname, "");
					zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
					return status;
				}
				//  Now we have the full record.  Replace the label array
				free(ztransfer->header2);
				ztransfer->header2 = (int *)calloc(newSize + 2, 4);
				charInt(clabels, ztransfer->header2, ztransfer->internalHeader[INT_HEAD_pdLabelsLength], (newSize * 4), 1, 1, 0);
				ztransfer->header2Number = newSize;
				ztransfer->internalHeader[INT_HEAD_pdLabelsLength] = newSize * 4;
				//  RE-write the whole thing
				status = zwrite (ifltab, ztransfer);
			}
			else {
				address = ztransfer->info[zdssInfoKeys.kinfoHeader2Address];
				charInt(clabels, ztransfer->header2, ztransfer->internalHeader[INT_HEAD_pdLabelsLength], (originalSize * 4), 1, 1, 0);
				status = zputBuff(ifltab, address, ztransfer->header2, originalSize, 1, BUFF_WRITE, bufferControl, buffer);
			}
			if (clabels) {
				free(clabels);
				clabels = 0;
			}
		}
		zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
		}

		zstructFree(ztransfer);
	}


	if ((pds->locationStruct) && (status == STATUS_OKAY)) {
		zlocationStore(ifltab, pds->locationStruct, 0);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zpdStore_ID, "Exit, Pathname: ", pds->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpdStore_ID, "Status: ", status);
	}

	return status;
}

