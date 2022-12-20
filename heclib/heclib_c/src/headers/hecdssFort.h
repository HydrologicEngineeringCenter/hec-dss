#ifndef HECDSS_FORT_H
#define HECDSS_FORT_H

//  Fortran Interface Routines
//  For code that accesses both version 6 and version 7 DSS files

//////////////////////////////////////
#include <stdlib.h>
#include <stdio.h>
#include "fortran_string_len_size.h"


/////////////////////////////////////
//  Not converted

void zcatalogfile7_(long long *ifltab, const char *catalogFilename, int *boolSorted, const char *pathWithWildChars, int *status,
	slen_t lencatalogFilename, slen_t lenpathWithWildChars);


void zloadcache6_(long long *ifltab, int* istat);

void ztsregstorefull_(long long *ifltab, const char *path,
				const char *startDate, const char *startTime,
				int *numberValues,
				int values[], int *sizeEachValue,
				int quality[], int *sizeEachQuality,
				int notes[], int *sizeEachNote,
				const char *cnotes, int *boolCnotesProvided,
				int userHeader[], int *lengthUserHeader,
				const char *units, const char *type, int *precisionValues,
				const char *timeZoneName,
				int *storageFlag, int *status,
				slen_t pathLen, slen_t startDateLen, slen_t startTimeLen,
				slen_t cnotesLen, slen_t unitsLen, slen_t typeLen,
				slen_t timeZoneLen);
void ztsirregstorefull_(long long *ifltab, const char *path,
						const char *baseDate, int *timeGranularitySeconds,
						int timeArray[], int *numberValues,
						int values[], int *sizeEachValue,
						int quality[], int *sizeEachQuality,
						int notes[], int *sizeEachNote,
						const char *cnotes, int *boolCnotesProvided,
						int *userHeader, int *numberUserHeader,
						const char *units, const char *type, int *precisionValues,
						const char *timeZoneName,
						int *storageFlag, int *status,
						slen_t pathLen, slen_t baseDateLen, slen_t cnotesLen,
						slen_t unitsLen, slen_t typeLen,
						slen_t timeZoneNameLen);


void ztsretrieveregfull_(long long *ifltab, const char *path,
				const char *startDate, const char *startTime, int *timeOffsetSeconds,
				int *maxNumberValues, int *numberRead,
				int values[], int *sizeEachValueRequested, int *sizeEachValueRead,
				int quality[], int *sizeEachQualityRequested, int *sizeEachQualityRead,
				int notes[], int *sizeEachNoteRequested, int *sizeEachNoteRead,
				char *cnotesFort, int *totalSizeCNotes, int *totalNumberCnotesRead,
				int userHeader[], int *sizeUserHeader, int *lengthUserHeader,
				char *units, char *type, int *precisionValues,
				char *timeZoneName, int *status,
				slen_t pathLen, slen_t startDateLen, slen_t startTimeLen, slen_t cNotesLen,
				slen_t unitsLen, slen_t typeLen, slen_t timeZoneLen);


void ztsirregretrievefull_(long long *ifltab, const char *path,
							const char *startDate, const char *startTime,
							const char *endDate, const char *endTime,
							int *maxNumberValues, int *numberRead,
							int timeArray[], int *timeGranularitySeconds, int *julianBaseDate,
							int values[], int *sizeEachValueRequested, int *sizeEachValueRead,
							int quality[], int *sizeEachQualityRequested, int *sizeEachQualityRead,
							int notes[], int *sizeEachNoteRequested, int *sizeEachNoteRead,
							char *cnotesFort, int *totalSizeCNotes, int *totalNumberCnotesRead,
							int userHeader[], int *sizeUserHeader, int *lengthUserHeader,
							char *units, char *type, int *precisionValues,             //  Note: no maxUnits or maxType
							char *timeZoneName, int *retrieveFlag, int *status,
							slen_t pathLen, slen_t startDateLen, slen_t startTimeLen,
							slen_t endDateLen, slen_t endTimeLen, slen_t cNotesLen,
							slen_t unitsLen, slen_t typeLen, slen_t timeZoneLen);
/////////////////////////////////////

void ztextstorearray_ (long long *ifltab, const char *pathname, char *clines,
					 int *numberLines, int *userHeader, int *numberUserHeader,
					 int *istatus, slen_t lenPathname, slen_t lenClines);

void zlocationstore_(long long *ifltab, const char *path,
					double coordinates[3], int coordinateDescription[6],
					char *timeZoneName, char *supplemental, int *status,
					slen_t pathLen, slen_t timeZoneLen, slen_t supplementalLen);

void zopen_ (long long *ifltab, const char *dssFilename, int *status, slen_t strlenDssFilename);
void zopen6_(long long *ifltab, const char *dssFilename, int *status, slen_t strlenDssFilename);
void zopen7_(long long *ifltab, const char *dssFilename, int *status, slen_t strlenDssFilename);

void zreada_(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *readFlag, int *recordFound, slen_t pathLen);

void zreadc_(long long *ifltab, const char* pathname,
			 int *internalHeader, int *internalHeaderArraySize , int *internalHeaderNumber,
			 int *header2, int *header2ArraySize, int *header2Number,
			 int *values3, int *values3ArraySize, int *values3Number,
			 int *userHeader, int *userHeaderArraySize, int *userHeaderNumber,
			 int *values1, int *values1Size, int *values1Number,
			 int *values2, int *values2Size, int *values2Number,
			 int *numberValues, int *logicalNumberValues,
			 int *totalAllocatedSize, int *totalExpandedSize, int *dataType,
			 int *status, slen_t pathnameLen);

void zreadx_(long long *ifltab, const char *pathname,
			 int *internalHeader, int *internalHeaderArraySize , int *internalHeaderNumber,
			 int *header2, int *header2ArraySize, int *header2Number,
			 int *userHeader, int *userHeaderArraySize, int *userHeaderNumber,
			 int *values, int *valuesSize, int *valuesNumber,
			 int *readPlan, int *recordFound, slen_t pathLen);


void zwritec_(long long *ifltab, const char* pathname,
			 int *internalHeader, int *internalHeaderNumber,
			 int *header2, int *header2Number,
			 int *values3, int *values3Number,
			 int *userHeader, int *userHeaderNumber,
			 int *values1, int *values1Number,
			 int *values2, int *values2Number,
			 int *numberValues, int *logicalNumberValues,
			 int *totalAllocatedSize, int *totalExpandedSize, int *dataType,
			 int *status, slen_t pathnameLen);

void zwritex_(long long *ifltab, const char *pathname, int *npath,
			 int *internalHeader, int *internalHeaderNumber,
			 int *header2, int *header2Number,
			 int *userHeader, int *userHeaderNumber,
			 int *values, int *valuesNumber, int *dataType,
			 int *plan, int *status, int *recordFound,
			 slen_t pathLen);

void zwritea_(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *flag, int *recordFound, slen_t pathLen);

int fortranwritelc_(int *fortranUnit, const char *message, int *boolEndOfRecord, slen_t lengthMessage);
int fortranwrite_(int *fortranUnit, const char *message, slen_t lengthMessage);
void fortranrewind_(int *fortranUnit);
int fortranread_(int *fortranUnit, char *string, int *lenString, int *ISTAT, slen_t lengthString);
void flush_(int *fortranUnit);

void getcurrentrec_(int *recNumber);

void zset6_(const char* parameter, const char* charVal, int *integerValue, slen_t lenparameter, slen_t lencharVal);

void zopen6int_(long long *ifltab, const char *dssFilename, int *status, slen_t strlenDssFilename);

void zread6_(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *readFlag, int *recordFound, slen_t pathLen);
void zreadx6_(long long *ifltab, const char *path,
			 int *internalHeader, int *internalHeaderArraySize , int *internalHeaderNumber,
			 int *header2, int *header2ArraySize, int *header2Number,
			 int *userHeader, int *userHeaderArraySize, int *userHeaderNumber,
			 int *values, int *valuesSize, int *valuesNumber,
			 int *readPlan, int *recordFound, slen_t pathLen);

void zwrite6_(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *flag, int *recordFound, slen_t pathLen);
void zwritex6_(long long *ifltab, const char *path, int *npath,
			 int *internalHeader, int *internalHeaderNumber,
			 int *header2, int *header2Number,
			 int *userHeader, int *userHeaderNumber,
			 int *values, int *valuesNumber,
			 int *dataType, int *plan,
			 int *status, int *recordFound, slen_t pathLen);

void zgetrecsize6_(long long *ifltab, const char *pathname, int *nihead,
					int *nchead, int *nuhead, int *ndata, int *istatus, slen_t pathLen);

void zdelet6_(long long *ifltab, const char *pathname, int *numberPathname,
			 int *boolfound, slen_t lenPathname);
void zundel6_(long long *ifltab, const char *pathname, int *numberPathname,
				int *status, slen_t lenPathname);



void zcklnk6_(long long *ifltab, int *status);
void zckpat6_(long long *ifltab, int *status);
void zckpnb6_(long long *ifltab, int *status);

void zcopyfile6_ (long long *ifltabFrom, long long *ifltabTo, int *istat);

void zcolist6_ (long long *ifltab, int *filePos, char *pathname,
                int *nPathname, int *status, slen_t sizeof_pathname);

void zcorec6_(long long *ifltabFrom, long long *ifltabTo, const char *pathnameFrom,  const char *pathnameTo,
	int *ibuff1, int *kbuff1, int *ibuff2, int *kbuff2, int *boolDuplcate, int *istat, slen_t pathnameFromLen, slen_t pathnameToLen);

void zdcinf_ (int *method, float *baseValue, int *baseSet, int *deltaSize,
			 int *precision, int *status);

void zfilst6_(long long *ifltab);

void zmaxpart6_(long long *ifltab, int *maxParts);

void zplist6_ (long long *ifltab, const char *instr, int *filePos, char *pathname,
               int *nPathname, int *status, slen_t len_instr, slen_t sizeof_pathname);

void zquery6_ (const char *request, char *returnVal, int *returnNumb, slen_t len_request, slen_t sizeof_returnVal);

void zrdpat_(const int *icunit, int *ipos, int *inumb, char *tag,
              char *path, int *npath, int *lend, slen_t lenTag, slen_t lenPath);

void dsscopystatus_(int *numberRecs, int *numberPaths, int *currentRec,
		           int *currentPath);
///////////////////////////////////////

void zgetversion_(int *ifltab, int *version);
void zfname6_(const char *dssFilenameIn, char *dssFilenameOut, int *nname, int *exists, slen_t lenDssFilenameIn, slen_t sizeDssFilenameOut);
void zinqir_(long long *ifltab, const char *cflg, char *calpha, int *inumb, slen_t cflg_len, slen_t calpha_len);
void zinqir6_(long long *ifltab, const char *cflg, char *calpha, int *inumb, slen_t cflg_len, slen_t calpha_len);
void zcheck_(long long *ifltab, const char *pathname, int *pathnameLen,
	int *numberHeader, int *numberData, int *exists, slen_t lenPathname);
void zdelete_(long long *ifltab, const char *pathname, int *status, slen_t lenPathname);
void zdelet_(long long *ifltab, const char *pathname, int *numberPathname,
			 int *boolfound, slen_t lenPathname);
void zundelete_(long long *ifltab, const char *pathname, int *status, slen_t lenPathname);
void zundel_(long long *ifltab, const char *pathname, int *numberPathname,
			 int *status, slen_t lenPathname);
void zcopyfile_ (long long *ifltabFrom, long long *ifltabTo, int *istat);
void zcopyrecord_(long long *ifltabFrom, long long *ifltabTo, const char *pathnameFrom,
				 const char *pathnameTo, int *status, slen_t pathnameFromLen, slen_t pathnameToLen);
void zdblook6_ (long long *ifltab, int *address, int *length);
void zdebugout7_(long long *ifltab, long long *iarray, long long *address, int *len);
void zdbmod6_ (long long *ifltab, int *address, int *value,
	const char *characterValue, int *useCharacterValue, slen_t characterValueLen);
void  zdebug1_(int *ival, long long *ival8, char *crval, char *cstrng, int *ibytes, slen_t crval_len, slen_t cstrng_len);
void  zdebug7_(int *ival, long long *ival8, int *ival4a, int *ival4b,
	char *cdval, char *crvala, char *crvalb, char *cstrng, int *ibytes,
	slen_t cdval_len, slen_t crvala_len, slen_t crvalb_len, slen_t cstrng_len);
void zgetrw6_ (int *iadd, int *record, int *word);
void zgtrec6_(long long *ifltab, int *arrayVals, int *numberWords, int *iadd, int *flag);
void zloadcache_(long long *ifltab, int *istat);
void zndata6_(long long *ifltab,int *ifpos, int *juls, int *isecs, char *cpath, int *npath, int *juld, int *isecd, int *idtype, int *istat, slen_t cstrng_len);
void znextts_(long long *ifltab, const char *cpath, char *cnext, int *lforward, int *istat, slen_t cpath_len, slen_t cnext_len);
void zread_(long long *ifltab, const char *cpath, int *npathname, int *header, int *nheader,
             int *data, int *ndata, int *plan, int *exists, slen_t cpathLen);
void zrecadd_ (long long *ifltab, const char *cpath, long long *recAdds, int *status, slen_t cpathLen);
void zrenam_ (long long *ifltab, const char *pathname, int *npath, const char *newPathname, int *nnewpath,
	          int *found, slen_t cpath_len, slen_t cnewpath_len);
void zrinfo_(long long *ifltab, const char *pathname, int *lfound, int *itype, char *typeString,
			  int *ldoubles, int *lquality, int *iprecision, char *tag, char *lastWrittenDate,
			  char *lastWrittenTime, char *programName, int *version, int *numberData,
			  int *spaceAllocated, int *compression, int *lpassworded,
			  slen_t len_pathname, slen_t size_typeString, slen_t size_tag, slen_t size_lastWrittenDate,
			  slen_t size_lastWrittenTime, slen_t size_programName);
void zrits_(long long *ifltab, const char *cpath, int *juls, int *istime, int *jule, int *ietime, int *itimes,
	float *values, int *kvals, int *nvals, int *ibdate, char *cunits, char *ctype, int *istat,
	slen_t cpath_len, slen_t cunits_len, slen_t ctype_len);
void zritsc_(long long *ifltab, const char *cpath, int *juls, int *istime, int *jule, int *ietime,
	int *lgetdob, int *lfildob, int *itimes, float *svalues, double *dvalues, int *kvals, int *nvals,
	int *ibdate, int *iqual, int *lqual, int *lqread, char *cunits, char *ctype, char *csupp,
	int *itzone, char *ctzone, double *coords, int *icdesc, int *lcoords, int *inflag, int *istat,
	slen_t cpath_len, slen_t cunits_len, slen_t ctype_len, slen_t csupp_len, slen_t ctzone_len);
void zritsx_(long long *ifltab, const char *cpath, int *juls, int *istime, int *jule, int *ietime, int *itimes,
	float *svalues, int *kvals, int *nvals, int *ibdate, int *iqual, int *lqual, int *lqread,
	char *cunits, char *ctype, int *iuhead, int *kuhead, int *nuhead, int *inflag, int *istat,
	slen_t cpath_len, slen_t cunits_len, slen_t ctype_len);
void  zritsxd_(long long *ifltab, const char *cpath, int *juls, int *istime, int *jule, int *ietime,
	int *itimes, double *dvalues, int *kvals, int *nvals, int *ibdate, int *iqual, int *lqual, int *lqread,
	char *cunits, char *ctype, int *iuhead, int *kuhead, int *nuhead, int *inflag, int *istat,
	slen_t cpath_len, slen_t cunits_len, slen_t ctype_len);
void  zrpd_(long long *ifltab, const char *cpath, int *nord, int *ncurve, int *ihoriz,
	char *c1unit, char *c1type, char *c2unit, char *c2type, float *svalues, int *kvals, int *nvals,
	char *clabel, int *klabel, int *label, int *iuhead, int *kuhead, int *nuhead, int *istat,
	slen_t cpath_len, slen_t c1unit_len, slen_t c1type_len, slen_t c2unit_len, slen_t c2type_len, slen_t clabel_len);
void  zrpdd_(long long *ifltab, const char *cpath, int *nord, int *ncurve, int *ihoriz,
	char *c1unit, char *c1type, char *c2unit, char *c2type, double *values, int *kvals, int *nvals,
	char *clabel, int *klabel, int *label, int *iuhead, int *kuhead, int *nuhead, int *istat,
	slen_t cpath_len, slen_t c1unit_len, slen_t c1type_len, slen_t c2unit_len, slen_t c2type_len, slen_t clabel_len);
void zrrts_(long long *ifltab, const char *cpath, const char *cdate, const char *ctime, int *nvals, float *values,
	char *cunits, char *ctype, int *iofset, int *istat,
	slen_t cpath_len, slen_t cdate_len, slen_t ctime_len, slen_t cunits_len, slen_t ctype_len);
void zrrtsc_(long long *ifltab, const char *cpath, const char *cdate, const char *ctime,
	int *kvals, int *nvals, int *lgetdob, int *lfildob, float *svalues, double *dvalues,
	int *jqual, int *lqual, int *lqread, char *cunits, char *ctype, char *csupp,
	int *iofset, int *jcomp, int *itzone, char *ctzone, double *coords, int *icdesc, int *lcoords, int *istat,
	slen_t cpath_len, slen_t cdate_len, slen_t ctime_len, slen_t cunits_len, slen_t ctype_len, slen_t csupp_len, slen_t ctzone_len);
void zrrtsx_(long long *ifltab, const char *cpath, const char *cdate, const char *ctime,
	int *nvals, float *svalues, int *jqual, int *lqual, int *lqread, char *cunits, char *ctype,
	int *iuhead, int *kuhead, int *nuhead, int *iofset, int *jcomp, int *istat,
	slen_t cpath_len, slen_t cdate_len, slen_t ctime_len, slen_t cunits_len, slen_t ctype_len);
void zrrtsxd_(long long *ifltab, const char *cpath, const char *cdate, const char *ctime,
	int *nvals, double *dvalues, int *jqual, int *lqual, int *lqread, char *cunits, char *ctype,
	int *iuhead, int *kuhead, int *nuhead, int *iofset, int *jcomp, int *istat,
	slen_t cpath_len, slen_t cdate_len, slen_t ctime_len, slen_t cunits_len, slen_t ctype_len);
void zrtxts_(long long *ifltab, const char *cpath, char *cstring, int *kstring, int *nstring,
	int *iuhead, int *kuhead, int *nuhead, int *istat, slen_t cpath_len, slen_t cstring_len);
void zset_(const char *cflg, const char *cstr, int *numb, slen_t cflg_len, slen_t cstr_len);
void zsetfi_(long long *ifltab, const char *cparam, const char *calpha, int *inumb, int *istatus,
	slen_t cparam_len, slen_t calpha_len);
void zsits_(long long *ifltab, const char *cpath, int *itimes, float *values, int *nvals,
	int *ibdate, const char *cunits, const char *ctype, int *inflag, int *istat,
	slen_t cpath_len, slen_t cunits_len, slen_t ctype_len);
void zsitsc_(long long *ifltab, const char *cpath, int *itimes, float *values, double *dvalues,
	int *ldouble, int *nvalue, int *ibdate, int *jqual, int *lsqual, const char *cunits, const char *ctype,
	double *coords, int *ncoords, int *icdesc, int *ncdesc, const char *csupp, int *itzone, const char *ctzone,
	int *inflag, int *istat, slen_t cpath_len, slen_t cunits_len, slen_t ctype_len, slen_t csupp_len, slen_t ctzone_len);
void zsitsx_(long long *ifltab, const char *cpath, int *itimes, float *values, int *nvalue, int *ibdate,
	int *jqual, int *lsqual, const char *cunits, const char *ctype, int *iuhead, int *nuhead, int *inflag, int *istat,
	slen_t cpath_len, slen_t cunits_len, slen_t ctype_len);
void zsitsxd_(long long *ifltab, const char *cpath, int *itimes, double *dvalues, int *nvalue,
	int *ibdate, int *jqual, int *lsqual, const char *cunits, const char *ctype,
	int *iuhead, int *nuhead,  int *inflag, int *istat,
	slen_t cpath_len, slen_t cunits_len, slen_t ctype_len);
void zspd_(long long *ifltab, const char *cpath, int *nord, int *ncurve, int *ihoriz,
	const char *c1unit, const char *c1type, const char *c2unit, const char *c2type,
	float *svalues, const char *clabel, int *label, int *iuhead, int *nuhead, int *iplan, int *istat,
	slen_t cpath_len, slen_t c1unit_len, slen_t c1type_len, slen_t c2unit_len, slen_t c2type_len, slen_t clabel_len);
void zspdd_(long long *ifltab, const char *cpath, int *nord, int *ncurve, int *ihoriz,
	const char *c1unit, const char *c1type, const char *c2unit, const char *c2type, double *dvalues,
	const char *clabel, int *label, int *iuhead, int *nuhead, int *iplan, int *istat,
	slen_t cpath_len, slen_t c1unit_len, slen_t c1type_len, slen_t c2unit_len, slen_t c2type_len, slen_t clabel_len);
void  zsrst_(long long *ifltab, const char *cpath, const char *cloc, const char *catim, const char *chparm, const char *chunit,
	const char *cfparm, const char *cfunit, const char *ccmt, const char *copt, int *lsnum, int *isnum, int *ldatum,
	float *datum, int *lstage, int *ibhi, int *ibhu, float *huv, int *ibho, float *hov,
	int *ibfi, int *ibfu, float *fuv, int *ibfo, float *fov, int *nbase, float *height, float *flow,
	int *nshift, int *isti, int *istu, int *isto, int *isbi, int *isbu, float *sbuv, int *isbo,
	float *sbov, int *ishi, int *ishu, float *shuv, int *isho, float *shov, int *latmsk, int *jbdate,
	int *ietime, int *iatime, int *nshftp, float *shifts, int *lcoff, float *cofval, int *noff,
	int *iobi, int *iobu, float *obuv, int *iobo, float *obov, int *iohi, int *iohu, float *ohuv,
	int *ioho, float *ohov, float *offset, int *ihoriz, int *iplan, int *istat,
	slen_t cpath_len, slen_t cloc_len, slen_t catim_len, slen_t chparm_len, slen_t chunit_len,
	slen_t cfparm_len, slen_t cfunit_len, slen_t ccmt_len, slen_t copt_len);
void zsrsti_(long long *ifltab, const char *cpath, int *iintrp, int *iuflow, int *ioflow, int *iplan, int *istat, slen_t cpath_len);
void zsrts_(long long *ifltab, const char *cpath, const char *cdate, const char *ctime, int *nvals, float *values,
	const char *cunits, const char *ctype, int *iplan, int *istat,
	slen_t cpath_len, slen_t cdate_len, slen_t ctime_len, slen_t cunits_len, slen_t ctype_len);
void zsrtsx_(long long *ifltab, const char *cpath, const char *cdate, const char *ctime,
	int *nvals, float *values, int *jqual, int *lqual, const char *cunits, const char *ctype,
	int *iuhead, int *nuhead, int *iplan, int *jcomp, float *basev, int *lbasev,
	int *ldhigh, int *nprec, int *istat,
	slen_t cpath_len, slen_t cdate_len, slen_t ctime_len, slen_t cunits_len, slen_t ctype_len);
void zsrtsxd_(long long *ifltab, const char *cpath, const char *cdate, const char *ctime,
	int *nvals, double *dvalues, int *jqual, int *lqual, const char *cunits, const char *ctype,
	int *iuhead, int *nuhead, int *iplan, int *jcomp, float *basev, int *lbasev,
	int *ldhigh, int *nprec, int *istat,
	slen_t cpath_len, slen_t cdate_len, slen_t ctime_len, slen_t cunits_len, slen_t ctype_len);
void zstxta_(long long *ifltab, const char *cpath, const char *carray, int *narray,
	int *iuhead, int *nuhead, int *istat, slen_t cpath_len, slen_t carray_len);
void ztsinfo_(long long *ifltab, const char *cpath, int *juls, int *istime,
	int *jule, int *ietime, char *cunits, char *ctype, int *lqual, int *ldouble, int *lfound,
	slen_t cpath_len, slen_t cunits_len, slen_t ctype_len);
void zupath_(const char *cpath, int *ibpart, int *iepart, int *ilpart, int *istat, slen_t cpath_len);
void zustfh_(char *clabel, char *citem, int *nitem, int *ipos, int *ihead, int *nhead, int *ierr, slen_t clabel_len, slen_t citem_len);
void ztsends_(long long *ifltab, const char *cpath, int *isearch, int *juls, int *istime, int *jule, int *ietime, int *lfound, slen_t cpath_len);

void ztsrange_(long long *ifltab, const char* cpath, int *searchOption,
			  char *firstPath, char *lastPath, int *numberFound,
			  slen_t len_cpath, slen_t len_firstPath, slen_t len_lastPath);
void ztssrch_(long long *ifltab, const char* cinpath, int *iunit, char *coutpath, int *nfound, slen_t cinpath_len, slen_t coutpath_len);



void zrrst_(long long *ifltab, const char *cpath, int *ietim, char *cloc, char *catim,
	char *chparm, char *chunit, char *cfparm, char *cfunit, char *ccmt, char *copt,
	int *lsnum, int *isnum, int *ldatum, float *datum, int *lstage, int *ibhi, int *ibhu,
	float *huv, int *ibho, float *hov, int *ibfi, int *ibfu, float *fuv, int *ibfo,
	float *fov, int *kbase, int *nbase, float *height, float *flow, int *kshift,
	int *nshift, int *isti, int *istu, int *isto, int *isbi, int *isbu, float *sbuv,
	int *isbo, float *sbov, int *ishi, int *ishu, float *shuv, int *isho, float *shov,
	int *latmsk, int *jbdate, int *ietime, int *iatime, int *nshftp, int *kshval, int *nshval,
	float *shifts, int *lcoff, float *cofval, int *koff, int *noff, int *iobi, int *iobu,
	float *obuv, int *iobo, float *obov, int *iohi, int *iohu, float *ohuv, int *ioho,
	float *ohov, float *offset, int *ihoriz, int *istat,
	slen_t cpath_len, slen_t cloc_len, slen_t catim_len, slen_t chparm_len, slen_t chunit_len,
	slen_t cfparm_len, slen_t cfunit_len, slen_t ccmt_len, slen_t copt_len);
void zrrsti_(long long *ifltab, const char *cpath, int  *ifcat, int *iintrp, int *iuflow,
	int *ioflow, int *lfound, int *ktimes, int *ntimes, int *itimes, int *istat, slen_t cpath_len);


#endif //  HECDSS_FORT_H

