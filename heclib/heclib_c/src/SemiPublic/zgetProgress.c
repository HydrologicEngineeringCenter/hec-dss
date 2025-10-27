#include "heclib7.h"
#include "zprogress.h"
#include "hecdssInternal.h"

/**
 * Instance of zProgress struct
 */
struct zProgress zprogress;
/**
*  Function:	zgetProgress
*
*  Use:			Semi-Public
*
*  Description:	A small utility function to get the current progress indicator,
*					usually during a squeeze or copy or other long progress.
*
*  Declaration: int zgetProgress(int handle);
*
*  Parameters:	int handle
*					Either zero or the handle of file you are watching.
*					If you want to be sure that you have the correct indicator
*					include the handle, otherwise there is a rare chance
*					that another thread may be using the same variables
*					If it is, a -1 will be returned, otherwise the current progress
*					If you don't care, just pass in 0 for the handle
*
*
*	See also:	zresetProgress(),  zsetInterrupt()
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/
int zgetProgress(int handle)
{
	if (handle == 0) {
		return zprogress.currentNumber;
	}
	if (handle != zprogress.handle) {
		return STATUS_NOT_OKAY;
	}
	return zprogress.currentNumber;
}
