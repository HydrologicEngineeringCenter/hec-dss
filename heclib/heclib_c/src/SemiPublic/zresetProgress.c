#include "zprogress.h"
#include "hecdssInternal.h"

//  A small function to either initialize the progress values,
//  or reset to indicate the operation has terminated (normally or other)
/**
*  Function:	zresetProgress
*
*  Use:			Semi-Public
*
*  Description:	A small utility function to either initialize the progress values,
*					uor reset to indicate the operation has terminated (normally or other)
*
*  Declaration: void zresetProgress(int handle, long long total);
*
*  Parameters:
*				int handle
*					The handle of file you are watching.
*
*				long long total
*					The total number of items (usually records) you will be processing
*
*
*
*	See also:	zresetProgress(), zsetInterrupt
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/
void zresetProgress(int handle, long long total)
{
	zprogress.handle = handle;
	zprogress.totalNumber = (int)total;
	zprogress.currentNumber = 0;
	zprogress.interrupt = 0;
	zprogress.numberErrors = 0;	
}

