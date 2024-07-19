#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <stdio.h>

#include "zprogress.h"
#include "zdssMessages.h"
#include "heclib.h"
#include "zerrorCodes.h"


/**
*  Function:	zcatalog6Internal
*
*  Use:			Private - use zcatalog instead
*
*  Description:	An interface to DSS-6 catalogs, so there can be compatibility between DSS-6 and DSS-7 versions
*
*  Declaration: int zcatalog6Internal(long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct,
			          int catalogHandle, int ifortUnit, int numberWanted, int boolCollection, int boolForSort);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *pathWithWildChars
*					Either null (for ignore) or a String that represents a pathname with wild characters represented
*					by a star (*) to match any string in the pathname part.  Wild characters can only be at the beginning or end of a part,
*					not inside of a string.  An example is a C part with "*Flow*" , which
*					will match all pathnames that have "Flow" anywhere in the C part, such as "Flow", "Inflow", "Outflow-Reg", etc.
*					A part such as "Flow*Reg" is not legal.  A null (//) will only match a null, where only a star (*) will match all.
*
*				zStructCatalog *catStruct
*					Either a struct created by function zstructCatalogNew(); or null.  This struct will hold a list of all
*					pathnames returned by this call.  See zStructCatalog.h for definition.  If null, then pathnames are
*					expected to be written to a file using either the catalogHandle or fortranUnit.
*					Be sure to call function zstructFree(struct) when finished.
*
*				int catalogHandle
*					A C handle (file descriptor) connected to a file opened by the calling function, usually with sopen or similar.
*					If the pathnames are not to be written to a file, then this should be zero.
*
*				int fortranUnit
*					A Fortran unit number connected to a file opened by the calling function.
*					If the pathnames are not to be written to a file, then this should be zero.
*
*				int numberWanted
*					The number of pathnames to limit the list to.  To retrieve all pathnames, set this to 0.
*					Sometimes only the first matching pathname is wanted, then this would be set to 1 (or however many).
*
*				int boolCollection
*					To search for all pathnames in a collection, pass one of the collections path in pathWithWild
*					and set this to 1.  You cannot directly search for both wild and collections (because pathWithWild
*					has to be a valid seed.  You can accomplish a similar search by passing a wild in the F part,
*					e.g. .../*|Run A/
*
*				int boolForSort
*					An int boolean flag set to "1" to indicate that this list is in preparation for sorting.
*					Sorting can take considerable more resources and time
*
*
*	Returns:	int numberCataloged
*					Number of pathnames in the file (a positive number)
*					or, if a negative number, then errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*	Remarks:	Not all version 7 functionality is available in version 6; this supports only what it can
*
*
*	See Also:	zcatalog()
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

// int zcatalog6Internal(long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct,
// 			          int catalogHandle, int ifortUnit, int numberWanted, int boolCollection, int boolForSort)
// {
// 	 char pathname[392];
// 	 char pathnameColl[392];
// 	 int npathname[1];
// 	 int filePos[1];
// 	 int istat[1];
// 	 int numberRecs[1];
// 	 int count;
// 	 int status;
// 	 char cnl[1];

// 	 char apart[MAX_PART_SIZE];
// 	 char fpart[MAX_PART_SIZE];
// 	 char bpart[MAX_PART_SIZE];
// 	 char cpart[MAX_PART_SIZE];
// 	 char dpart[MAX_PART_SIZE];
// 	 char epart[MAX_PART_SIZE];
// 	 int partAction[6];
// 	 int lengths[6];
// 	 int partMax[7];
// 	 int boolCompareWild;
// 	 int boolFound;
// 	 int one;
// 	 int temp;

// 	 filePos[0] = 0;
// 	 istat[0] = 0;


// 	 if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
// 		 zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "zcatalog6Internal: catalog handle ", catalogHandle);
// 		 zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "zcatalog6Internal: collection: ", boolCollection);
// 		 zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Sort Catalog:  ", boolForSort);
// 		 if (pathWithWild) {
// 			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Pathname with wild chars:  ", pathWithWild);
// 		 }
// 		 else {
// 			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "No Pathname with wild chars:  ", "");
// 		 }
// 	}


// 	 zinqir_(ifltab, "NREC", pathname, numberRecs, (size_t)4, (size_t)0);
// 	 if (numberRecs[0] < 1) {
// 		 //  Uhoh....
// 		 return numberRecs[0];
// 	 }

// 	 zmaxpart6_ (ifltab, partMax);
// 	 //  Swap D and F parts
// 	 temp = partMax[3];
// 	 partMax[3] = partMax[5];
// 	 partMax[5] = temp;

// 	 if (catStruct) {
// 		catStruct->listSize = numberRecs[0];
// 		catStruct->pathnameList = (char **)calloc((size_t)catStruct->listSize, WORD_SIZE);
// 		catStruct->allocated[zSTRUCT_pathname] = 1;
// 		/*if (catStruct->boolIncludeDates) {
// 			catStruct->startDates = (int *)calloc((size_t)catStruct->listSize, WORD_SIZE);
// 			catStruct->endDates   = (int *)calloc((size_t)catStruct->listSize, WORD_SIZE);
// 		} */
// 	}

// 	 if (boolCollection) {
// 		 stringCopy (pathnameColl, sizeof(pathnameColl), pathWithWild, strlen(pathWithWild));
// 		 boolCollection = zcollectionsPath(pathnameColl, strlen(pathnameColl));
// 		 boolCompareWild = 0;
// 	 }
// 	 else if (pathWithWild) {
// 		boolCompareWild = zcatParsePath(pathWithWild, partAction, lengths,
// 			apart, sizeof(apart),
// 			bpart, sizeof(bpart),
// 			cpart, sizeof(cpart),
// 			dpart, sizeof(dpart),
// 			epart, sizeof(epart),
// 			fpart, sizeof(fpart));
// 	 }
// 	 else {
// 		boolCompareWild = 0;
// 	 }

// 	 count = 0;
// 	 while (1) {
// 		 if (boolCollection) {
// 			 zcolist6_(ifltab, filePos, pathnameColl, npathname, istat, sizeof(pathname));
// 		 }
// 		 else {
// 			zplist6_(ifltab, " ", filePos, pathname, npathname, istat, 0, sizeof(pathname));
// 		 }
// 		 if (istat[0]) break;
// 		 if (npathname[0] <= 0) break;
// 		 pathname[npathname[0]] = '\0';

// 		 if (boolCompareWild) {
// 			boolFound = zcatComparePath(pathname, partAction, lengths, apart, bpart, cpart, dpart, epart, fpart, 0);
// 		 }
// 		 else {
// 			 boolFound = 1;
// 		 }
// 		 if (boolFound) {
// 			 if (catStruct) {
// 				catStruct->pathnameList[count] = stringFortToC(pathname,  (size_t)npathname[0]);
// 			 }
// 			 if (catalogHandle) {
// 				 if (boolForSort) {
// 					status = zcatSortPath(catalogHandle, pathname, (size_t)npathname[0], 0, partMax, count);
// 				 }
// 				 else {
// 					writf_(&catalogHandle, pathname, &npathname[0], &status, &temp);
// 				}
// 				if (status < 0) {
// 					perror("Attempting to write pathname ");
// 				}
// 				temp = 1;
// 				cnl[0] = '\n';
// 				writf_(&catalogHandle, cnl, &temp, &status, &temp);
// 			 }
// 			 if (ifortUnit > 0) {
// 				one = 1;
// 				status = fortranwritelc_(&ifortUnit, pathname, &one, (size_t)npathname[0]);
// 			 }
// 			 count++;
// 			 if (count >= numberRecs[0]) {
// 				 break;
// 			 }
// 			 if ((numberWanted > 0) && (count >= numberWanted)) {
// 				break;
// 			}
// 		 }
// 	 }
// 	 if (catStruct) {
// 		catStruct->numberPathnames = count;
// 		catStruct->listSize = count;
// 	}
// 	 if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
// 		 zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Exit zcatalog6Internal, number ", count);
// 	 }
// 	 return count;
// }
