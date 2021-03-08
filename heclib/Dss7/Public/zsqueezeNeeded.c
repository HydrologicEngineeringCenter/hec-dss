#include "zdssKeys.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "zdssMessages.h"


/**
*  Function:	zsqueezeNeeded
*
*  Use:			semi-Public (Call zsqueeze7 with boolOnlyIfNeeded = 1 instead)
*
*  Description:	Determines if a DSS file needs squeezing or not.
*
*  Declaration: int zsqueezeNeeded(long long *ifltab);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*
*	Returns:	int boolDoSqueeze
*					= 0 if a squeeze is not needed
*					= 1 if a squeeze should be preformed
*
*
*	Remarks:	Set message level to MESS_LEVEL_USER_DIAG for reason for squeeze
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zsqueezeNeeded(long long *ifltab)
{

	int boolDoSqueeze;
	double dble;
	char filename[256];
	int reason;
	long long *fileHeader;


	boolDoSqueeze = 0;
	reason = 0;

	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	//  Squeeze because of dead space?
	//  Use 25% dead space, or 10% with 100,000 64x words, or 400,000 64x words
	dble = (double)fileHeader[zdssFileKeys.kdead] / (double)fileHeader[zdssFileKeys.kfileSize];
	if (dble > 0.25) boolDoSqueeze = 1;
	if ((dble > 0.10) && (fileHeader[zdssFileKeys.kdead] > 100000)) boolDoSqueeze = 1;
	if (fileHeader[zdssFileKeys.kdead] > 400000) boolDoSqueeze = 1;
	if (boolDoSqueeze) reason = 1;

	//  Squeeze because of deletes or renames?
	//  10% records have been deleted or renamed, or 1,000 records deleted or renamed
	dble = (double)fileHeader[zdssFileKeys.knumberDeletes] / (double)fileHeader[zdssFileKeys.knumberRecords];
	if (dble > 0.1) boolDoSqueeze = 1;
	if (fileHeader[zdssFileKeys.knumberDeletes] > 1000) boolDoSqueeze = 1;
	dble = (double)fileHeader[zdssFileKeys.knumberRenames] / (double)fileHeader[zdssFileKeys.knumberRecords];
	if (dble > 0.1) boolDoSqueeze = 1;
	if (fileHeader[zdssFileKeys.knumberRenames] > 1000) boolDoSqueeze = 1;
	if (!reason && boolDoSqueeze) reason = 2;

	//  Pointer utilization?
	//  Get average number of paths per bin * a constant

	dble = 10.0;//   (double)fileHeader[zdssFileKeys.kbinSize] /
	dble = (double)fileHeader[zdssFileKeys.knumberRecords] / ((double)fileHeader[zdssFileKeys.khashsUsed] * dble);
	if (dble > 1.0) {
		if ((fileHeader[zdssFileKeys.kmaxHash] > 35000) && (fileHeader[zdssFileKeys.kdead] < 10000)) {
			if ((fileHeader[zdssFileKeys.knumberRecords] > 1000000) && (fileHeader[zdssFileKeys.kmaxHash] < 50000)) boolDoSqueeze = 1;
		}
		else {
			boolDoSqueeze = 1;
		}
	}
	if (!reason && boolDoSqueeze) reason = 3;

	//  Any file errors?
	//  Always squeeze if a file error (although rare)
	if (fileHeader[zdssFileKeys.kerrorTotal]) boolDoSqueeze = 1;
	if (!reason && boolDoSqueeze) reason = 4;

	//  Reclaimed space?
	//  If space available to reclaim > 10% file size, or > 50,000 words
	dble = (double)fileHeader[zdssFileKeys.kreclaimedSpace] / (double)fileHeader[zdssFileKeys.kfileSize];
	if (dble > 0.1) boolDoSqueeze = 1;
	if (fileHeader[zdssFileKeys.kreclaimedSpace] > 50000) boolDoSqueeze = 1;
	if (!reason && boolDoSqueeze) reason = 5;


	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		charLong((void *)ifltab[zdssKeys.kfilename], filename, 0, sizeof(filename), 0, 1);
		zmessageDebug(ifltab, DSS_FUNCTION_zsqueeze_ID, "Exit, zsqueezeNeeded, file: ", filename);
		if (boolDoSqueeze) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zsqueeze_ID, "File DOES need to be squeezed, reason: ", reason);
			if (reason == 1) {
				zmessageDebugLong(ifltab, DSS_FUNCTION_zsqueeze_ID, "Excessive dead space: ", fileHeader[zdssFileKeys.kdead]);
			}
			else if (reason == 2) {
				zmessageDebugLong(ifltab, DSS_FUNCTION_zsqueeze_ID, "Excessive Deletes or Renames: ", fileHeader[zdssFileKeys.knumberDeletes]);
			}
			else if (reason == 3) {
				zmessageDebug(ifltab, DSS_FUNCTION_zsqueeze_ID, "Pointer utilization too high ", "");
			}
			else if (reason == 4) {
				zmessageDebugLong(ifltab, DSS_FUNCTION_zsqueeze_ID, "File Errors ", fileHeader[zdssFileKeys.kerrorTotal]);
			}
			else if (reason == 5) {
				zmessageDebugLong(ifltab, DSS_FUNCTION_zsqueeze_ID, "Excessive reclaim space available ", fileHeader[zdssFileKeys.kreclaimedSpace]);
			}
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zsqueeze_ID, "File DOES NOT need to be squeezed", "");
		}
	}

	return boolDoSqueeze;

}

