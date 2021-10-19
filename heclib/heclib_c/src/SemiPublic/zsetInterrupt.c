#include "zprogress.h"
#include "hecdssInternal.h"



/**
*  Function:	zsetInterrupt
*
*  Use:			Semi-Public
*
*  Description:	A small utility function to tell the current process that the user
*					has pressed "Cancel" or "Stop" to interrupt the process
*
*  Declaration: void zsetInterrupt(int handle);
*
*  Parameters:	int handle
*					Either zero or the handle of file you are watching.
*					If you want to be sure that you have the correct indicator
*					include the handle, otherwise there is a rare chance
*					that another thread may be using the same variables
*					If you don't care, just pass in 0 for the handle
*
*
*	See also:	zresetProgress(), zgetProgress()
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/

void zsetInterrupt(int handle)
{
	if (handle == 0) {
		zprogress.interrupt = 1;
	}
	if (handle == zprogress.handle) {
		zprogress.interrupt = 1;
	}
}

