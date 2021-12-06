#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "heclib7.h"

/**
*  Function:	zsetCollectionSequence
*
*  Use:			Public
*
*  Description:	Uses an int number to create a new collection pathname with that sequence.
*
*  Declaration: char* zsetCollectionSequence(char* pathname, int sequenceNumber);
*
*  Parameters:
*				char *pathname (input)
*					The pathname to make a collections path, and set the sequenceNumber as "XXXXXX"
*
*				int sequenceNumber
*					The sequence number to set
*
*
*	Returns:	char *collectionPathname
*					A malloc of the input pathname with the F part set to the collection sequence number.
*					BE SURE TO FREE this pathname when done.
*					null if the input pathname is not a valid pathname
*
*	Note:		Collections are identified by an F part of /C:000000|REST OF FPART/
*				Where 00000 is generally a sequence number, for example
*				/YUBA/SMARTSVILLE/FLOW/01JAN1997/1HOUR/C:000042|OPERATION A/
*
*
*
*
*	Author:			Bill Charley, 2019
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

char* zsetCollectionSequence(char* pathname, int sequenceNumber)
{
	int flen;
	int len;
	int i;
	int collectionBar;
	char *pathnameCollection;
	char fPart[MAX_F_PART_SIZE];
	char fPartColl[MAX_F_PART_SIZE];


	len = (int)strlen(pathname);
	if (len < 7) {
		return (char *)0;
	}
	//  Get the F part of the pathname
	flen = zpathnameGetPart(pathname, 6, fPart, sizeof(fPart));
	if (flen == STATUS_NOT_OKAY) {
		return (char *)0;
	}

	//  Does the pathname already have a collection F part
	//  (and we are just replacing it?)
	collectionBar = 0;
	if ((fPart[0] == 'c') || (fPart[0] == 'C')) {
		if (fPart[1] == ':') {
			for (i = 0; i < flen; i++) {
				if (fPart[i] == '|') {
					collectionBar = i;
					break;
				}
			}
		}
	}

	//  Create the collection part of the pathname
	_snprintf_s(fPartColl, sizeof(fPartColl), _TRUNCATE, "C:%6.6d|", sequenceNumber);

	//  Copy the remaining part of the F part (if already a collection, don't include old part)
	if (collectionBar) {
		collectionBar++;
		stringCat(fPartColl, sizeof(fPartColl), &fPart[collectionBar], _TRUNCATE);
	}
	else {
		stringCat(fPartColl, sizeof(fPartColl), fPart, _TRUNCATE);
	}

	//  Allocate new pathname and copy
	pathnameCollection = calloc((len + 10), 1);
	if (!pathnameCollection) {
		fprintf(stderr, "Memory exhausted, zsetCollectionSequence\n");
		return (char *)0;
	}
	stringCopy(pathnameCollection, (len + 10), pathname, len);

	//  Replace the F part with the collection F part
	zpathnameSetPart(pathnameCollection, (len + 10), fPartColl, 6);

	//  Done
	return pathnameCollection;
}

