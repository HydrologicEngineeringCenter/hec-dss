#include "hecdssInternal.h"

/**
*  Function:	zcompareRecordStatus
*
*  Use:			semi-private
*
*  Description:	Compares the status of a record wanted, verses what was read, keeping priority in order.
*
*  Declaration: int zcompareRecordStatus(int statusRead, int statusWanted)
*
*  Parameters:	int statusRead:  Status of the record read
*
*				int statusWanted:  Status wanted.
*
*  Returns:		int (boolean) same:
*					0 (zero) if the statuses are not the same, considering priority
*					1 (one)  if the statuses are the same.
*
*  Remarks:		You can ask for all valid which includes both good and aliases, or
*					just aliases.
*
*				statusRead
*					REC_STATUS_PRIMARY (1):		good (and primary path for this record)
*					REC_STATUS_ALIAS (2):		Alias - address points to primary, or previous alias
*					REC_STATUS_DELETED (11):	Deleted
*					REC_STATUS_RENAMED (12):	Renamed
*
*				int statusWanted:
*					REC_STATUS_VALID (0):		All valid (primary and alaises)
*					REC_STATUS_PRIMARY (1):		Primary only
*					REC_STATUS_ALIAS (2):		Alias only
*					REC_STATUS_DELETED (11):	Deleted only
*					REC_STATUS_RENAMED (12):	Renamed only
*					REC_STATUS_ANY (100):		Any, regardless if deleted, renamed, etc.
*
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcompareRecordStatus(int statusRead, int statusWanted)
{
	if (statusRead <= REC_STATUS_PRIMARY) {
		if (statusWanted == REC_STATUS_VALID) return 1;
		if (statusWanted == REC_STATUS_PRIMARY) return 1;
		if (statusWanted == REC_STATUS_ANY) return 1;
	}
	if (statusRead == REC_STATUS_ALIAS) {
		if (statusWanted == REC_STATUS_VALID) return 1;
		if (statusWanted == REC_STATUS_ALIAS) return 1;
		if (statusWanted == REC_STATUS_ANY) return 1;
	}
	if (statusRead == REC_STATUS_DELETED) {
		if (statusWanted == REC_STATUS_DELETED) return 1;
		if (statusWanted == REC_STATUS_ANY) return 1;
	}
	if (statusRead == REC_STATUS_RENAMED) {
		if (statusWanted == REC_STATUS_RENAMED) return 1;
		if (statusWanted == REC_STATUS_ANY) return 1;
	}
	return 0;
}

