
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>


#include "heclib.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zdssLocking.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "zprogress.h"

void zswap6_(int*, int*);

long long zinquire(long long *ifltab, const char *request)
{
	int len;
	int number;
	char requestlc[5];
	char ctemp[9];
	char creturn[2];
	long long longNumber;
	long long *info;
	long long *fileHeader;
	double d;
	int i;


	if (!request) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zinquire_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "request is null");
	}

	if (zgetVersion(ifltab) == 6) {
		zinqir6_ (ifltab, request, creturn, &number, strlen(request), sizeof(creturn));
		return (long long) number;
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


	/////////////////////////////
	/////////////  FIX ME HERE
	//////////////////
	///  IF THE FILE IS NOT OPENED YET, THIS WILL BLOW UP

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zinquire_ID, "Request: ", requestlc);
	}


	longNumber = -1;

	if (!strcmp(requestlc, "stat")) {
		longNumber = ifltab[zdssKeys.kerrorCondition];
	}
	else if (!strcmp(requestlc, "hand")) {
		longNumber = ifltab[zdssKeys.khandle];
	}
	else if (!strcmp(requestlc, "unit")) {
		longNumber = ifltab[zdssKeys.khandle];
	}
	else if (!strcmp(requestlc, "writ")) {  //  Write access
		if (ifltab[zdssKeys.kopenStatus] != OPEN_STAT_WRITE) {
			return 0;
		}
		else {
			return 1;
		}
	}
	//  Progress messages
	else if (!strcmp(requestlc, "tota")) {
		longNumber = (long)zprogress.totalNumber;
	}
	else if (!strcmp(requestlc, "curr")) {
		longNumber = (long)zprogress.currentNumber;
	}
	else if (!strcmp(requestlc, "nerr")) {
		longNumber = (long)zprogress.numberErrors;
	}
	//
	else if (!strcmp(requestlc, "nrec")) {
		longNumber = fileHeader[zdssFileKeys.knumberRecords] + fileHeader[zdssFileKeys.knumberAliases];
	}
	else if (!strcmp(requestlc, "npri")) {
		longNumber = fileHeader[zdssFileKeys.knumberRecords];
	}
	else if (!strcmp(requestlc, "nali")) {
		longNumber = fileHeader[zdssFileKeys.knumberAliases];
	}
	else if (!strcmp(requestlc, "mlvl")) {
		longNumber = zmessaging.methodLevel[MESS_METHOD_GLOBAL_ID];
	}
	else if (!strcmp(requestlc, "mlev")) {
		longNumber = zmessaging.methodLevel[MESS_METHOD_GLOBAL_ID];
	}
	else if (!strcmp(requestlc, "muni")) {
		longNumber = ifltab[zdssKeys.kfortMessUnit];
		if (longNumber <= 0) {
			longNumber = zdssVals.fortranMessageUnit;
		}
	}
	else if (!strcmp(requestlc, "mhan")) {
		longNumber = ifltab[zdssKeys.kmessHandle];
		if (longNumber <= 0) {
			longNumber = zdssVals.messageHandle;
		}
	}
	else if (!strcmp(requestlc, "erro")) {
		if ((zgetVersion(ifltab) == 7) && (ifltab[zdssKeys.kerrorCode] != 0)) {
			longNumber = ifltab[zdssKeys.kerrorCode];
		}
		else {
			longNumber = zdssVals.globalErrorFlag;
		}
	}
	else  if (!strcmp(requestlc, "fver")) {
		charInt((void *)&fileHeader[zdssFileKeys.kversion], ctemp, 4, sizeof(ctemp), 0, 1, 0);
		ctemp[4] = '\0';
		longNumber = 10000 * (ctemp[0] - '0') +
		               100 * (ctemp[2] - (ctemp[2] < '[' ? '@' : '`')) +
		                     (ctemp[3] - (ctemp[3] < '[' ? '@' : '`'));
		if (bigEndian()) {
			int iswap = (int)longNumber;
			zswap6_(&iswap, &iswap);
			longNumber = iswap;
		}
	}
	else if (!strcmp(requestlc, "size")) {
		//  Size in KB
		zpermRead(ifltab);
		longNumber = ((fileHeader[zdssFileKeys.kfileSize] - 1) * 8L) / 1024L;

	}
	else if (!strcmp(requestlc, "fsiz")) {
		//  Size in (64 bit) words
		/////    NEEDS to be fixed to accommodate int 64!!!!!!!!!!
		longNumber = fileHeader[zdssFileKeys.kfileSize] - 1;
	}
	else if (!strcmp(requestlc, "maxp")) {
		longNumber = fileHeader[zdssFileKeys.kmaxExpectedPathnames];
	}
	else if (!strcmp(requestlc, "dsiz")) {
		//   dead space size in (64 bit) words
		zpermRead(ifltab);
		longNumber = fileHeader[zdssFileKeys.kdead] ;
	}
	else if (!strcmp(requestlc, "dead")) {
		//  Precentage dead space
		zpermRead(ifltab);
		longNumber = fileHeader[zdssFileKeys.kfileSize] - 1;
		d = (double)fileHeader[zdssFileKeys.kdead] / (double)longNumber;
		d *= 100.0;
		longNumber = (long long)d;
	}
	else if (!strncmp(requestlc, "ndat", 4)) {
		//  Logical number of data to set in info area.
		//  This is for DSS-6 calling conventions compatibility
		longNumber = ifltab[zdssKeys.kgetLogicalNumberData];
	}
	else if (!strcmp(requestlc, "read")) {
		longNumber = ifltab[zdssKeys.kopenStatus];
	}
	else if (!strcmp(requestlc, "recl")) {
		longNumber = ifltab[zdssKeys.kreclaimLevel];
	}
	else if (!strcmp(requestlc, "prec")) {
		if (ifltab[zdssKeys.kaddInfoLastPath] > 0) {
			info = (long long *)ifltab[zdssKeys.kinfo];
//  fix me
		}
		else {
			longNumber = 0;
		}
	}	else if (!strcmp(requestlc, "cats")) {
		//  Catalog sort list status
		//  0 = None (no sort list available)
		//  1 = List fully sorted and ready to use (without further sorting)
		//  2 = Updates have been made and catalog will have to be further sorted
		//  3 = Significant changes and sort list should not be used - do a full re-sort.
		longNumber = fileHeader[zdssFileKeys.kcatSortStatus];
	}
	else if (!strcmp(requestlc, "call")) {
		//  Catalog sort list length - array size for pathnames last sorted
		longNumber = fileHeader[zdssFileKeys.kcatSortNumber]; //???
	}
	else if (!strcmp(requestlc, "caal")) {
		//  Catalog sort records added list length - array size for pathnames added since last sort
		longNumber = fileHeader[zdssFileKeys.kcatSortNewWrites];
	}
	else if (!strcmp(requestlc, "coll")) {
		//  Number of collection records
		longNumber = fileHeader[zdssFileKeys.knumberCollections];
	}
	else if (!strcmp(requestlc, "expa")) {
		//  Number of expansions
		longNumber = fileHeader[zdssFileKeys.knumberExpansions];
	}
	else if (!strcmp(requestlc, "dele")) {
		//  Number of deleted records
		longNumber = fileHeader[zdssFileKeys.knumberDeletes];
	}
	else if (!strcmp(requestlc, "rena")) {
		//  Number of deleted records
		longNumber = fileHeader[zdssFileKeys.knumberRenames];
	}
	else if (!strcmp(requestlc, "maxa")) {
		//  Length of longest A part of pathname in file
		longNumber = fileHeader[zdssFileKeys.kmaxA];
	}
	else if (!strcmp(requestlc, "maxb")) {
		//  Length of longest B part of pathname in file
		longNumber = fileHeader[zdssFileKeys.kmaxB];
	}
	else if (!strcmp(requestlc, "maxc")) {
		//  Length of longest C part of pathname in file
		longNumber = fileHeader[zdssFileKeys.kmaxC];
	}
	else if (!strcmp(requestlc, "maxd")) {
		//  Length of longest D part of pathname in file
		longNumber = fileHeader[zdssFileKeys.kmaxD];
	}
	else if (!strcmp(requestlc, "maxe")) {
		//  Length of longest E part of pathname in file
		longNumber = fileHeader[zdssFileKeys.kmaxE];
	}
	else if (!strcmp(requestlc, "maxf")) {
		//  Length of longest F part of pathname in file
		longNumber = fileHeader[zdssFileKeys.kmaxF];
	}
	else if (!strcmp(requestlc, "mult")) {
		//  Multi-user access mode
		longNumber = ifltab[zdssKeys.kmultiUserAccess];
	}
	else if (!strcmp(requestlc, "othe")) {
		//  Other processes accessing the file?
		longNumber = zlockPassive(ifltab, LOCKING_LOCK_TEST, LOCKING_ACCESS_READ);
	}
	else if (!strcmp(requestlc, "noth")) {
		//  Number of other processes accessing the file?
		longNumber = zlockPassive(ifltab, LOCKING_LOCK_NUMBER, LOCKING_ACCESS_READ);
	}
	else if (!strcmp(requestlc, "sque")) {
		longNumber = zsqueezeNeeded(ifltab);
	}
	else if (!strcmp(requestlc, "ftim")) {
		//  File last write time
		longNumber = zgetLastWriteTimeFile(ifltab);
	}
	else if (!strcmp(requestlc, "mtim")) {
		//  My last write time
		longNumber = zgetMyLastWriteTime(ifltab);
	}
	else if (!strcmp(requestlc, "ksta")) {
		//  For compatibility
		longNumber = (long)-1;
	}
	else {
		zmessage2(ifltab, "Request not recognized: ", request);
		longNumber = 0;
		return -1;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugLong(ifltab, DSS_FUNCTION_zinquire_ID, "number: ", longNumber);
	}

	return longNumber;
}

long long zinquire_ (long long *ifltab, const char *parameter, size_t lenParameter)
{
	long long longNumber;
	char *param;

	param = stringFortToC(parameter, lenParameter);

	longNumber = zinquire(ifltab, param);

	free(param);
	return longNumber;
}

