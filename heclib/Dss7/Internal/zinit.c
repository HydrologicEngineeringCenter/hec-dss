#include <stdio.h>
#include <string.h>
#ifdef _MSC_VER
#include <process.h>
#else
#include <sys/types.h>
#include <unistd.h>
#endif

#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "heclib7.h"



///  FIX ME - Hardwired numbers (e.g., flags)  need to be in #DEFINE statements!!!

////////////////////////////////////////////////////////

/**
*  Function:	zinit
*
*  Use:			Private (Internal)
*
*  Description:	Initialize ifltab pointers and various other items.
*				This is called before any other function and is called only once per session.
*
*  Declaration: void zinit()
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

//  HEC-DSS
//  Versions 1-3, written by Art Pabst and Mark Lewis, 1979 for LBL Mainframes
//  Version 4, written by Bill Charley and Brent Cullimore, 1981 for Harris Minicomputer
//  Version 5, written by Bill Charley, 1988, for IBM XT PC
//  Version 6, written by Bill Charley, 1991, for multi-platform (PC, unix),
//             Mike Perryman had significant contributions.
//  Version 7, written by Bill Charley, 2010 for 64 bit architecture

//  zinit defines all pointers used in HEC-DSS, primarily for ifltab, the file table
//  ifltab pointers are defined in struct zdssKeys
//  ifltab contains both temporary values and addresses, as well
//  as portions of the DSS file, such as the "permanent" section, the main header of the file
//
//  A DSS file consists of:
//			Permanent area;  around 70 int*8 words
//			Table:  around 8000 int*8 words.  Hash table containing address of pathname bin blocks
//          Pathname bin blocks:  about 150*32 int*8 words.  Contains pathnames and address of info blocks
//          Info block and data area
//          Info block and data area
//          ...
//			Pathname bin blocks
//			Info block and data area
//			...
//			and so on.

//  ifltab must be dimensioned to int*4 [200] or int*8 [100] (800 bytes long)
//  The ifltab array contains
//			ifltab[0] = 7;  quickly identifying this as a version 7 file.
//          ifltab[1], the size of the file header
//          ifltab[2] = "kintegrityKey1", a value used to determine if the ifltab has become corrupt (written over)
//			ifltab[1- 100]:  Information about this session, such as handle, etc.
//

struct hec_zmessageAvail zmessageAvail;
struct hec_zmessaging zmessaging;
struct hec_zdssVals zdssVals;
void zinit()
{
	int i;
	long long ifltabTemp[1];

	//  Has this already been called?
	if (zdssVals.integrityKey == DSS_INTEGRITY_KEY) {
		if (zmessageLevel(ifltabTemp, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug(ifltabTemp, DSS_FUNCTION_zinit_ID, "Subsequent call ignored", "");
		}
		return;
	}

	//  Initialize error reporting
	zerrorStructClear();



   //  Set values
   //  Minimum permanent sections is 100 words
   //  Use Caution - this can cause programs ifltab to be exceeded!!
	zdssVals.maxExpectedPathnames = 0;
	zdssVals.newCollectionFile = 0;

	//  The current process id, for seeing who is writing to the file
#ifdef _MSC_VER
	zdssVals.pid = _getpid();
#else
	zdssVals.pid = getpid();
#endif


	//  The flags to be used in different areas in ifltab to check that it has not become corrupt.
	zdssVals.integrityKey = DSS_INTEGRITY_KEY;
	//  For our old Fortran friends, still supported.
	zdssVals.fortranMessageUnit = fortranMessageUnit;
	zdssVals.messageHandle = messageHandle;
	//  Any major error is saved here (for all files in session)
	zdssVals.globalErrorFlag = 0;   //  Same as severity. 0 = no errors
	zdssVals.globalErrorMess[0] = '\0';

	zdssVals.copyEmptyRecords = 1;

	//  Identifiers at the start of a DSS file to identify it is DSS and its version
	stringCopy(zdssVals.czdss, (size_t)5, "ZDSS", (size_t)5);
	stringCopy(zdssVals.czVersion, sizeof(zdssVals.czVersion), DSS_VERSION, sizeof(DSS_VERSION));
	stringCopy(zdssVals.czVersionDate, sizeof(zdssVals.czVersionDate), DSS_VERSION_DATE, sizeof(DSS_VERSION_DATE));



	/////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////

	//  zdssKeys point to locations in the ifltab array, and the key starts with "k".
	//  zdssKeys in ifltab are specific to the file opened.
	//  "key" is used as specific values set to be sure ifltab is not corrupted

	//  zdssBinKeys point to locations in the pathname bin table, which points to
	//  record info blocks.

	//  zdssFileKeys point to locations in the file header (permanent section.)

	//  zdssInfoKeys point to locations in the info block that provides information
	//  about a single record.  InfoKeys are specific to a record.

	//  zdssVals generally point to general items

	//  Messaging is assumed not to be initialized at this point, but will be upon
	//  exit, so debug messages can appear after that.



	//  knumVersion is the numerical version number (i.e., 7) so applications
	//  can quickly tell what version is being used
    zdssKeys.knumVersion = 0;

	// khandle points to the handle for this file.  Must be SECOND word for Java functions
    zdssKeys.khandle = zdssKeys.knumVersion + 1;

    zdssKeys.kintegrityKey1 = zdssKeys.khandle + 1;

	zdssKeys.kfiHeadSize = zdssKeys.kintegrityKey1 + 1;

	//----------------------
    //  kmultiUserAccess points to the multiple user access flag
    zdssKeys.kmultiUserAccess = zdssKeys.kfiHeadSize + 1;


	zdssKeys.kopenMode = zdssKeys.kmultiUserAccess + 1;


	//     kopenStatus points to a flag indicating the open status of the file
	//  e.g., (read only)
    zdssKeys.kopenStatus = zdssKeys.kopenMode + 1;

	//  kremote point to a flag that indicates if the file is on a remote
	//  disk (network mounted or client server).  Not guaranteed.
	//  Set to 1 if remote, 0 if local or unknown
    zdssKeys.kremote = zdssKeys.kopenStatus + 1;


/*     KSWAP points to a flag indicating if all bytes need to be swapped, */
/*     because the file is a different Endian from the machine. */
    zdssKeys.kswap = zdssKeys.kremote + 1;


	// kdswap points to a flag indicating if words for DOUBLE
	// values need to be swapped to keep files compatible between
	// different Endian machines.  (swap on unix side)
    zdssKeys.kdswap = zdssKeys.kswap + 1;


	//   klocked indicates if this file is currently write locked
    zdssKeys.klocked = zdssKeys.kdswap + 1;

	//   klockLevel indicates the function level that has the lock
    zdssKeys.klockLevel = zdssKeys.klocked + 1;

	//  klockCheckSet indicates if the file was locked since the last check
    zdssKeys.klockCheckSet = zdssKeys.klockLevel + 1;

	//  klocksDenied is the number of times a lock request has been denied
	//  (probably) due to another process having the file locked
	//  (and then this process waiting for it to be unlocked)
    zdssKeys.klocksDenied = zdssKeys.klockCheckSet + 1;

	zdssKeys.klockExclusiveWord = zdssKeys.klocksDenied + 1;
	zdssKeys.klockWriteMyAddress = zdssKeys.klockExclusiveWord + 1;
	zdssKeys.klockReadMyAddress = zdssKeys.klockWriteMyAddress + 1;

	//   kpidMyAddress points to the file address when this process id
	//   is stored while writing to the file (for multi-user purpose)
	//   (The actual address starts coincidently with the lock address)
    zdssKeys.kpidMyAddress = zdssKeys.klockReadMyAddress + 1;

	//     kwritingNow indicates if file is being written to (in zwriteInternal)
    zdssKeys.kwritingNow = zdssKeys.kpidMyAddress + 1;

	//   kmyLastWriteTime is the last write time of this process (in milliseconds)
	//   (compare to file last write time, which is for all processes)
	zdssKeys.kmyLastWriteTime = zdssKeys.kwritingNow + 1;

	//  kerrorSevere points to flag that is set to non-zero should a severe error occur
	//  In this case, a severe error is usually memory corruption or damaged file,
	//  not a bad address or read or write error
	zdssKeys.kerrorSevere = zdssKeys.kmyLastWriteTime + 1;

    //  kerrorCondition indicates if an error has occurred and its severity
	//   0 - No errors
	//   See severity
    zdssKeys.kerrorCondition = zdssKeys.kerrorSevere + 1;

	//  errorCode is the actual error.  See errorProcessing for different codes
    zdssKeys.kerrorCode = zdssKeys.kerrorCondition + 1;


	//  If an important message (error) has been set,
	zdssKeys.kmessagesAvail = zdssKeys.kerrorCode + 1;

	//  kmessLevel points to the message level of this file.  If -1, the default is used
    zdssKeys.kmessLevel = zdssKeys.kmessagesAvail + 1;

	//  kmessHandle points to the output unit for this file.  If -1, stdout is used
    zdssKeys.kmessHandle = zdssKeys.kmessLevel + 1;

	//  kfortMessUnit points a Fortran output unit for this file.  If 0, it is not written to
    zdssKeys.kfortMessUnit = zdssKeys.kmessHandle + 1;



	//  kaddLast is the address of the last physical read or write
	//  It is typically used for debugging and error processing */
    zdssKeys.kaddLast = zdssKeys.kfortMessUnit + 1;


	//     kfileWritten points to a flag indicating if the file has been written to
	//    The value of IFLTAB will be set to 1 when it has.
    zdssKeys.kfileWritten = zdssKeys.kaddLast + 1;

	// kwritesSinceFlush is the number of physical writes since last flush
	zdssKeys.kwritesSinceFlush = zdssKeys.kfileWritten + 1;


	//  number of reads is the physical number of disk reads since open
	zdssKeys.knumberReads = zdssKeys.kwritesSinceFlush + 1;

	//  number of writes is the physical number of disk writes since open
	zdssKeys.knumberWrites = zdssKeys.knumberReads + 1;

	//     KSUSER points to a flag indicating if the user is the super user
	//     (i.e., their password matches the file password) (0=no, 1=yes).
    zdssKeys.ksuser = zdssKeys.knumberWrites + 1;

	//     KEXCL indicats if the file has been exclusively opened
    zdssKeys.kexclusive = zdssKeys.ksuser + 1;

	//     kpathsThisHash is the number of pathnames for this hash;
	//    statistics only
    zdssKeys.kpathsThisHash = zdssKeys.kexclusive + 1;

	//  A flag indicating that the current pathname has the same hash as another in
	//  the file (and same length); a non-unique condition (the non-unique flag
	//  will be updated on a new write)
	zdssKeys.ksameHash = zdssKeys.kpathsThisHash + 1;


	//     Other information in IFLTAB
	//  Indicator of last pathname checked found or not
	zdssKeys.kfound = zdssKeys.ksameHash + 1;

	//  Table hash of last pathanem
	zdssKeys.ktableHash = zdssKeys.kfound + 1;

	//  Bin hash (Unique hash) of last pathname
	zdssKeys.kpathnameHash = zdssKeys.ktableHash + 1;


	//  kiftPathHash is the full hash code of the pathname for the buffer area
	zdssKeys.kiftPathHash = zdssKeys.kpathnameHash + 1;

	//  Address to table hash of last pathname
	zdssKeys.kaddTableHash = zdssKeys.kiftPathHash + 1;

	//  Length of last pathname
	zdssKeys.klenLastPath = zdssKeys.kaddTableHash + 1;

	// Bin address of this table hash (may be 0 + - )
	zdssKeys.khashTableBinAdd = zdssKeys.klenLastPath + 1;

	zdssKeys.klastType = zdssKeys.khashTableBinAdd + 1;

	zdssKeys.kisaCollection = zdssKeys.klastType + 1;

	//  kpathBinAddress is the address of the pathname hash (in the path bin)
	//  This is the start of the short segment that contains the path and info address
	zdssKeys.kpathBinAddress = zdssKeys.kisaCollection + 1;

	//
	zdssKeys.kbinTypeAndCatSort = zdssKeys.kpathBinAddress + 1;
	zdssKeys.kbinLastWrite = zdssKeys.kbinTypeAndCatSort + 1;
	zdssKeys.kbinDates = zdssKeys.kbinLastWrite + 1;
	//     Last Pathname accessed Information

	// Address of pathname in pathname bin for last record checked
	zdssKeys.kpathAddressInBin = zdssKeys.kbinDates + 1;

	//     Read or computed - not stored in perm area.
	//     KAINFO points to the location of the last paths info block
	zdssKeys.kaddInfoLastPath = zdssKeys.kpathAddressInBin + 1;

   //  kbinRecordAddress points to the bin location that has
   //  the address of the record (info area)
   zdssKeys.kinfoAddInBin = zdssKeys.kaddInfoLastPath + 1;

   zdssKeys.kbinStatus = zdssKeys.kinfoAddInBin + 1;

   zdssKeys.kbinPathLen = zdssKeys.kbinStatus + 1;

	//     kbinAddCurrent points to the address of the last pathname's bin
    zdssKeys.kbinAddCurrent = zdssKeys.kbinPathLen + 1;

	//     kbinAddplist points to the address of the last pathname's bin for zplist
	//     (a function for DSS-6 compatibility only)
    zdssKeys.kbinAddplist = zdssKeys.kbinAddCurrent + 1;

	//     kbinPosPlist points to the position in that pathname bin for zplist
    zdssKeys.kbinPosPlist = zdssKeys.kbinAddplist + 1;

	//     kbinCountplist points to the count of pathname bin for that section for zplist
    zdssKeys.kbinCountplist = zdssKeys.kbinPosPlist + 1;

	//  kbinWithSpace points to a bin that has reclaimed space
	//  that has room to store the current pathname being checked.
	//  If someone were to delete a record, then write it again,
	//  this would use the same address in the same bin.
	//  If this happened a lot (sometimes it does), this would
	//  prevent long searches through deleted records
	zdssKeys.kbinWithSpace = zdssKeys.kbinCountplist + 1;

	zdssKeys.kdataFirstDate = zdssKeys.kbinWithSpace + 1;
	zdssKeys.kdataLastDate = zdssKeys.kdataFirstDate + 1;

	zdssKeys.kgetLogicalNumberData = zdssKeys.kdataLastDate + 1;
	zdssKeys.ksetLogicalNumberData = zdssKeys.kgetLogicalNumberData + 1;

	//  Filename, pointer in ifltab
	zdssKeys.kfilename = zdssKeys.ksetLogicalNumberData + 1;

	zdssKeys.kfullFilename = zdssKeys.kfilename + 1;

	//  A set catalog for comparing what's changed
	zdssKeys.kcatStruct = zdssKeys.kfullFilename + 1;

	//  CRC Table for determining CRC value for data record
	zdssKeys.kCRCtable = zdssKeys.kcatStruct + 1;

	zdssKeys.kintegrityKey2 = zdssKeys.kCRCtable + 1;




	//  The following keys include pointers to allocated memory
	//  To ensure integrity of each memory component, a flag is
	//  put at the beginning of the allocated array and at then end
	//  and checked during accesses.  The flags are not written to disk.

////////////////////////////////////////////////

	//  File Header memory area
	zdssKeys.kfileHeader  = zdssKeys.kintegrityKey2 +1;


	//////////////////////////////////////////////
	//  Pathname Bin memory area
	/*     kbinMem is a pointer to pathname bin memory that has
	been allocated for use in (only) reading and writing the pathname bin
	It is initialized by zmemoryGet and released by zmemoryFree.
	zmemoryFree is called at zclose, and must always be called
	to avoid a memory leak.  kbinMem is only to be accessed by
	zmemoryGet and zmemoryFree, never outside those functions*/

	zdssKeys.kpathBin = zdssKeys.kfileHeader + 1;


	//  Info memory area
	zdssKeys.kinfo = zdssKeys.kpathBin + 1;
	//  kiftInfoSize is the length being used by the buffer
	zdssKeys.kinfoSize = zdssKeys.kinfo + 1;
	//  kinfoAddress is the file address for the current info table
	zdssKeys.kinfoAddress = zdssKeys.kinfoSize + 1;

	//  zdssKeys.kreclaim points to malloced memory containing the reclaim array read from disk
	zdssKeys.kreclaim = zdssKeys.kinfoAddress + 1;

	//  kreclaimLevel points to the current reclaim level,
	//  which  indicates how aggressive we should be at reclaiming space.
	//  kreclaimLevel is current setting and can be lowered temporarily
	//  kreclaimLevelFile is the file level (which kreclaimLevel is set from) and is permanent (can only be lowered)
	//  ifltab[zdssKeys.kreclaimLevel] == 0 Undefined
	//  ifltab[zdssKeys.kreclaimLevel] == 1  RECLAIM_NONE   Don't use space reclamation
	//  ifltab[zdssKeys.kreclaimLevel] == 2  RECLAIM_EXCESS Reclaim space left over from extending records, etc.  (can recover records)
	//  ifltab[zdssKeys.kreclaimLevel] == 3  RECLAIM_ALL    Reclaim all possible space, including deleted records (cannot recover)
	zdssKeys.kreclaimLevel = zdssKeys.kreclaim + 1;


	zdssKeys.kintegrityKey3 = zdssKeys.kreclaimLevel + 1;


	/////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////

	//     Header section file pointers (this area stored on disk)

	//     KDSS points to the identifier 'ZDSS', indicating that this is
	//     a DSS file
    zdssFileKeys.kdss = 0;


	//  kfileHeaderSize is the size of the permanent section.
	//  This is a variable so that additional information can be
	//  added to DSS file.  Of course, programs linked with
	//  older libraries will not know what it means, so they will ignore it
	zdssFileKeys.kfileHeaderSize = zdssFileKeys.kdss + 1;

	//     KVERS is the DSS software version for this file
	//     KVERS must always remain in the same location in the file
	//     so past and future versions can recognize the file as DSS

    zdssFileKeys.kversion = zdssFileKeys.kfileHeaderSize + 1;

	//     KNRECS points to the number of records in the file
    zdssFileKeys.knumberRecords = zdssFileKeys.kversion + 1;

	//  Number of alias pathnames
	zdssFileKeys.knumberAliases = zdssFileKeys.knumberRecords + 1;

	//     kfileSize is one word greater than the file size (in words)
	//		It is the next area to write, and should contain
	//		the end of file flag
    zdssFileKeys.kfileSize = zdssFileKeys.knumberAliases + 1;


	//     KDEAD is the dead space pointer
    zdssFileKeys.kdead = zdssFileKeys.kfileSize + 1;

	//  The number of records that have been expanded since squeeze
	//  (includes same record being expanded multiple times)
	zdssFileKeys.knumberExpansions = zdssFileKeys.kdead + 1;

	zdssFileKeys.knumberCollections = zdssFileKeys.knumberExpansions + 1;

	//  Number of records renamed since last squeeze
	zdssFileKeys.knumberRenames = zdssFileKeys.knumberCollections + 1;

	//  Number of record deleted since last squeeze
	zdssFileKeys.knumberDeletes = zdssFileKeys.knumberRenames + 1;

	//  Number of aliases deleted since last squeeze
	zdssFileKeys.knumberAliasDeletes = zdssFileKeys.knumberDeletes + 1;

	//     KCREAT pointes to the date/time the file was created
    zdssFileKeys.kcreateDate = zdssFileKeys.knumberAliasDeletes + 1;

	//     klastWriteTime points to the time in mills the file was last written to
    zdssFileKeys.klastWriteTime = zdssFileKeys.kcreateDate + 1;

	//     klockAddressWord points to the word position of the lock record
    zdssFileKeys.klockAddressWord = zdssFileKeys.klastWriteTime + 1;

	//     kmaxHash points to the maximum hash value for this file
    zdssFileKeys.kmaxHash = zdssFileKeys.klockAddressWord + 1;

	//     KHUSED are the number of hash codes used.  This will always be
	//     less than or equal to kmaxHash
    zdssFileKeys.khashsUsed = zdssFileKeys.kmaxHash + 1;

	//     KMAXPH is the maximum number of pathnames for any one table hash code
    zdssFileKeys.kmaxPathsOneHash = zdssFileKeys.khashsUsed + 1;

	//     KMAXHC is the hash code for this
    zdssFileKeys.kmaxPathsHashCode = zdssFileKeys.kmaxPathsOneHash + 1;

	//     KAHASH points to the address of the hash table
    zdssFileKeys.kaddHashTableStart = zdssFileKeys.kmaxPathsHashCode + 1;

	// khashCollisions is the number of non-unique pathname hashes (for
	// a single table hash).  This is the number of additional info areas,
	// and should be either zero or a very small number
	zdssFileKeys.khashCollisions = zdssFileKeys.kaddHashTableStart + 1;

	//     kbinsPerBlock points to the number of bins per block (except
	//     for the first block).
    zdssFileKeys.kbinsPerBlock = zdssFileKeys.khashCollisions + 1;

	//     kbinsRemainInBlock points to the number of bins remaining in the current block
    zdssFileKeys.kbinsRemainInBlock = zdssFileKeys.kbinsPerBlock + 1;

	//     kbinSize points to the size of the pathname bin (in words)
    zdssFileKeys.kbinSize = zdssFileKeys.kbinsRemainInBlock + 1;

	//     kaddFirstBin points to the location of the first bin
    zdssFileKeys.kaddFirstBin = zdssFileKeys.kbinSize + 1;

	//     kaddNextEmptyBin points to the location of the next empty bin
    zdssFileKeys.kaddNextEmptyBin = zdssFileKeys.kaddFirstBin + 1;

	//     File efficiency variables

	//     ktotalBins indicates the number of pathname bins used in the file
    zdssFileKeys.ktotalBins = zdssFileKeys.kaddNextEmptyBin + 1;

	//     KBOVER is the number of overflow bins - previous bins filled
    zdssFileKeys.kbinsOverflow = zdssFileKeys.ktotalBins + 1;


	//     KFPASS points to the file password (encoded)
    zdssFileKeys.kfilePassword = zdssFileKeys.kbinsOverflow + 1;


	//  Note - for errors, there are two error situations -
	//  kerror refers to any error that occurred and may be just for that session (e.g., no write access)
	//  kfileError is when the system error occurs, such as no more disk space.
	//  kfileError generally indicates a potentially damaged file, where
	//  kerror indicates any error.  kfileError is saved with the file to indicate on subsequent opens that
	//  the file might be damaged.
	//  A flag that indicates if a file error occurred in previous write
	 zdssFileKeys.kfileError = zdssFileKeys.kfilePassword + 2;

	 //  The error code given by _get_errno
	 zdssFileKeys.kfileErrorCode = zdssFileKeys.kfileError + 1;

	//  the catalog sort list is an array of index numbers indicating the sort order of the catalog
	//  The sequence number helps how to build the sort list so that added records don't get it out of order
	 ///  OBSOLETE - REMOVE ME  (Unused)
	zdssFileKeys.kcatSequenceNumber = zdssFileKeys.kfileErrorCode + 1;

	//  kcatSortStatus is the status of the catalog sort list
	//  0 = none
	//  1 = sorted, no updates and ready for use
	//  2 = records have been added
	//  3 =  significant changes (deleted or renamed); sort list is unusable
	zdssFileKeys.kcatSortStatus = zdssFileKeys.kcatSequenceNumber + 1;

	//  kcatSortWrites is the number of new records written since
	//  the last sort list saved.  This helps determine how much
	//  the sort list might be out of date
	zdssFileKeys.kcatSortNewWrites = zdssFileKeys.kcatSortStatus + 1;
	zdssFileKeys.kcatSortDeletes = zdssFileKeys.kcatSortNewWrites + 1;

	//  kcatSortSize is the physical length of sort list (in long longs, 8 bytes)
	zdssFileKeys.kcatSortSize = zdssFileKeys.kcatSortDeletes + 1;

	//  kcatSortNumber is number of values in sort list , usually equal to length
	zdssFileKeys.kcatSortNumber = zdssFileKeys.kcatSortSize + 1;

	//  kcatSortAddress is the address of the current sort list flag
	//  The sort list is composed of the (long long) address of the path hash
	//  for each pathname in default sorted order (for a full sort only!)
	zdssFileKeys.kcatSortAddress = zdssFileKeys.kcatSortNumber + 1;
	//		  flag  (-97535)
	//        sort list
	//        ending flag (-97536)

	//  Reclaimed Space
	//  Reclaimed space is usable dead space in the file
	//  reclaim space is not allocated until the first delete or expansion
	//  We only reclaim significant size segments; that includes deleted records,
	//  space from expanded records and deleted catalog sort sequences
	//  We don't reclaim anything from pathname bins, etc.
	//  We only use reclaimed space for record info/data areas,
	//  not for bins or catalog sort areas (these generally require too much space)

	// kreclaimMin is the minimum length of space to use for reclamation
	//  If you release space smaller than this, it is ignored.
	//  kreclaimMin also indicates if the file allows the use of reclaimed space
	//  (0 = no, >0 = yes)  (Reclaimed space can be slower)
	 zdssFileKeys.kreclaimMin = zdssFileKeys.kcatSortAddress + 1;//zdssnz_1.npass;

	//  kreclaimMaxAvailable is the maximum amount of reclaimed space
	//  available in one contiguous segment (we don't do multiple segments)
	zdssFileKeys.kreclaimMaxAvailable = zdssFileKeys.kreclaimMin + 1;

	//  kreclaimTotal is the total amount of reclaimed space available in the file
	zdssFileKeys.kreclaimTotal = zdssFileKeys.kreclaimMaxAvailable + 1;

	//  kreclaimTableAddress is the address of the reclaim array in the file
	//  it is a 2x array, with the first word being the length for that segment
	//  and the second word being the address of that segment
	zdssFileKeys.kreclaimTableAddress = zdssFileKeys.kreclaimTotal + 1;

	//  Points to Address of reclaim segment with available space
	zdssFileKeys.kreclaimSegAvailableAdd = zdssFileKeys.kreclaimTableAddress + 1;

	//  Array number that corresponds to kreclaimSegAvailableAdd
	zdssFileKeys.kreclaimSegNumber = zdssFileKeys.kreclaimSegAvailableAdd + 1;

	//  Total maximum number of segments that can be allocated to reclaiming (first int)
	//  After this is reached, releasing space is ignored (time for a squeeze)
	//  And number of segments allocated for this file (second int)
	zdssFileKeys.kreclaimMaxSegment = zdssFileKeys.kreclaimSegNumber + 1;

	zdssFileKeys.kreclaimSegmentsUsed = zdssFileKeys.kreclaimMaxSegment + 1;


	//  kreclaimNumber is the number of reclaim location (pairs) for each reclaim segment
	zdssFileKeys.kreclaimNumber = zdssFileKeys.kreclaimSegmentsUsed + 1;

	//  kreclaimSize is the physcial length of the reclaim segment
	//  Note, reclaim number <= (reclaim lengh) * 2
	zdssFileKeys.kreclaimSize = zdssFileKeys.kreclaimNumber + 1;

	//  kreclaimedSpaceUsed is the number of times space has been reclaimed
	//  The first int is the number of pathnames reclaimed from the pathname bin
	//  The second is the number of record areas reclaimed
	zdssFileKeys.kreclaimedPaths = zdssFileKeys.kreclaimSize + 1;
	zdssFileKeys.kreclaimedSpace = zdssFileKeys.kreclaimedPaths + 1;

/*  maximum lengths of pathname parts */
    zdssFileKeys.kmaxPath = zdssFileKeys.kreclaimedSpace + 1;
	zdssFileKeys.kmaxA = zdssFileKeys.kmaxPath + 1;
	zdssFileKeys.kmaxB = zdssFileKeys.kmaxA + 1;
	zdssFileKeys.kmaxC = zdssFileKeys.kmaxB + 1;
	zdssFileKeys.kmaxD = zdssFileKeys.kmaxC + 1;
	zdssFileKeys.kmaxE = zdssFileKeys.kmaxD + 1;
	zdssFileKeys.kmaxF = zdssFileKeys.kmaxE + 1;

	// maximum header, data lengths (these lengths are for byte words)
	zdssFileKeys.kmaxInternalHeader = zdssFileKeys.kmaxF + 1;
	zdssFileKeys.kmaxHeader2 = zdssFileKeys.kmaxInternalHeader + 1;
	zdssFileKeys.kmaxUserHeader = zdssFileKeys.kmaxHeader2 + 1;
	zdssFileKeys.kmaxValues1Size = zdssFileKeys.kmaxUserHeader + 1;
	zdssFileKeys.kmaxValues2Size = zdssFileKeys.kmaxValues1Size + 1;
	zdssFileKeys.kmaxValues3Size = zdssFileKeys.kmaxValues2Size + 1;

	//  Number of records with internal header (8 byte)
	zdssFileKeys.knumberInternalHeader = zdssFileKeys.kmaxValues3Size + 1;
	//  Number of records with compression header
	zdssFileKeys.knumberHeader2 = zdssFileKeys.knumberInternalHeader + 1;
	//  Number of records with description header
	zdssFileKeys.knumberDataArea3 = zdssFileKeys.knumberHeader2 + 1;
	//  Number of records with user header
	zdssFileKeys.knumberUserHeader = zdssFileKeys.knumberDataArea3 + 1;
	//  Number of records that utilize data area (should be all)
	zdssFileKeys.knumberDataArea1 = zdssFileKeys.knumberUserHeader + 1;
	zdssFileKeys.knumberDataArea2 = zdssFileKeys.knumberDataArea1 + 1;

	// maximum record length (8 byte word)
	zdssFileKeys.kmaxRecordSize = zdssFileKeys.knumberDataArea2 + 1;

	//  Maximum length of a regular interval time series float record, including info
	zdssFileKeys.kmaxRtsSize = zdssFileKeys.kmaxRecordSize + 1;
	//  Maximum length of a regular interval time series double record, including info
	zdssFileKeys.kmaxRtdSize = zdssFileKeys.kmaxRtsSize + 1;
	//  Maximum length of a regular interval time series double record, including info
	zdssFileKeys.kmaxItsSize = zdssFileKeys.kmaxRtdSize + 1;
	//  Maximum length of a regular interval time series double record, including info
	zdssFileKeys.kmaxItdSize = zdssFileKeys.kmaxItsSize + 1;
	//  Maximum length of a regular interval time series double record, including info
	zdssFileKeys.kmaxPdSize = zdssFileKeys.kmaxItdSize + 1;
	//  Maximum length of a regular interval time series double record, including info
	zdssFileKeys.kmaxPddSize = zdssFileKeys.kmaxPdSize + 1;
	//  Maximum length of other data records, including info
	zdssFileKeys.kmaxOtherSize = zdssFileKeys.kmaxPddSize + 1;

	//  Maximum number of pathnames set upon original creation
	zdssFileKeys.kmaxExpectedPathnames = zdssFileKeys.kmaxOtherSize + 1;

	//  Error counters for any severe errors that this file
	//  has gone through (since last squeeze)
	zdssFileKeys.kerrorMemory = zdssFileKeys.kmaxExpectedPathnames + 1;
	zdssFileKeys.kerrorAddress = zdssFileKeys.kerrorMemory + 1;
	zdssFileKeys.kerrorWrite = zdssFileKeys.kerrorAddress + 1;
	zdssFileKeys.kerrorRead = zdssFileKeys.kerrorWrite + 1;

	//  Total errors for this file for all time before squeezes
	//  (i.e., add this with above error counts for complete error count)
	zdssFileKeys.kerrorTotal = zdssFileKeys.kerrorRead + 1;

	//  Location bounding, LR = Lower Right (usually lat, long)
	zdssFileKeys.klocBoundLR = zdssFileKeys.kerrorTotal + 1;
	zdssFileKeys.klocBoundLL = zdssFileKeys.klocBoundLR + 1;
	zdssFileKeys.klocBoundUR = zdssFileKeys.klocBoundLL + 1;
	zdssFileKeys.klocBoundUL = zdssFileKeys.klocBoundUR + 1;
	zdssFileKeys.klocBoundElev = zdssFileKeys.klocBoundUL + 1;


	zdssFileKeys.kdetune = zdssFileKeys.klocBoundElev + 1;

	zdssFileKeys.klockArraySizes = zdssFileKeys.kdetune + 1;
	zdssFileKeys.klockWriteArrayAddress = zdssFileKeys.klockArraySizes + 1;
	zdssFileKeys.klockReadArrayAddress = zdssFileKeys.klockWriteArrayAddress + 1;
	zdssFileKeys.kpidArrayAddress = zdssFileKeys.klockReadArrayAddress + 1;

	//  endian identifies if the file has been written to on a
	//  big endian (e.g., Solaris) machine
	//  1 means yes, 0 means no (default)
	zdssFileKeys.kendian = zdssFileKeys.kpidArrayAddress + 1;

	//     additional words are reserved after the perm
	//     section for any future use
	
	zdssFileKeys.kreserved1 = zdssFileKeys.kendian + 1;
	zdssFileKeys.kreserved2 = zdssFileKeys.kreserved1 + 1;
	zdssFileKeys.kreserved3 = zdssFileKeys.kreserved2 + 1;
	zdssFileKeys.kreserved4 = zdssFileKeys.kreserved3 + 1;


	//     *** End of permanent area written to file ***
	zdssFileKeys.kendFileHeader = zdssFileKeys.kreserved4 + 1;

	//  Allocate any additional space for future use...
//	size = zdssFileKeys.kendFileHeader - zdssKeys.kfileHeader + 1;
//	if (size < DSS_MIN_FILE_HEADER_SIZE) {
		//  This should always be the case
//		zdssFileKeys.kendFileHeader = zdssKeys.kfileHeader + DSS_MIN_FILE_HEADER_SIZE - 1;
//	}



	/////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////

	//  Item locations within the pathname bin
	zdssBinKeys.kbinHash = 0;
	zdssBinKeys.kbinStatus = 1;
	zdssBinKeys.kbinPathLen = 2;
	zdssBinKeys.kbinInfoAdd = 3;
	zdssBinKeys.kbinTypeAndCatSort = 4;
	zdssBinKeys.kbinLastWrite = 5;
	zdssBinKeys.kbinDates = 6;
	zdssBinKeys.kbinPath = 7;

	//  the number of words used in the bin for each record, excluding the pathname
	//  Do not change this value - it is critical.
	zdssBinKeys.kbinSize = zdssBinKeys.kbinPath - zdssBinKeys.kbinHash;



	/////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////

	/*  Record Information Block:

		Note the information block is kept in separate memory allocated on
		open and freed on close

			Information Block flag:		-97534
			Record Status:				-1:  Record expanded and moved
										 1:  Good
										 2:  Deleted
			Pathname length
			Data type / Version number
					Data type is the code for type (e.g., regular time series, paired)
					Version number - the number of times this record has been written
			Number Expansions / Expansion Flag
					Number Expansions - the number of times the size has increase
					Expansion Flag - A flag indicating how the record should be treated
					during a squeeze
			Compression Flag/ Precision
					Compression 0:  Not used,  > 0 compressed (number indicates type of compression)
					Precision of data in places past decimal (1=tenth, 2=hundredth)
			Date and time last written:	In seconds (from Jan 01, 1970)
			Program name:				Name of program last wrote (limit 8 char)
			Julian date and time (seconds) of first valid value (if time series)
			Julian date and time (seconds) of last  valid value (if time series)
			When the record was originally created (milliseconds)
			Unused
			Address Internal Header
			Length Internal Header
			Address Compression Header
			Length Compression Header
			Address Description Header
			Length Description Header in bytes
			Address User Header
			Length User Header
			Address of data area
			Data length (used - this may be less than allocated)
			Allocated data area / number data values
			Logical number data / length (ints) per value
			     Allocated data area (includes unused space for expansion)
				 Logical number of data  (after uncompression) - what the user would eventually see
			Number of aliases (addresses follow pathname)
			Unused (reserved) space
			Pathname
			Alias address 1 (optional)
			Alias address 2 (optional)
			Alias ...
	*/


	//     kinfoFlag points to a flag indicating a information block (=-9753)
    zdssInfoKeys.kinfoFlag = 0;

	//     kinfoStatus points to this records status
    zdssInfoKeys.kinfoStatus = zdssInfoKeys.kinfoFlag + 1;

	//     kinfoPathnameLength points to the length of the pathname for this record
    zdssInfoKeys.kinfoPathnameLength = zdssInfoKeys.kinfoStatus + 1;

	//     kinfoHash is the pathname hash, for easy verification of correct path
    zdssInfoKeys.kinfoHash = zdssInfoKeys.kinfoPathnameLength + 1;

	// data type, version
	// Type points to the data type (e.g., time-series)
	// Version points to the record version number (number times written)
    zdssInfoKeys.kinfoTypeVersion = zdssInfoKeys.kinfoHash + 1;

	//  number expansions, expansion flag
	//   Expansion points to the number of times this record was expanded
	//  (written to again, but number data or header larger than on disk
    zdssInfoKeys.kinfoExpansion = zdssInfoKeys.kinfoTypeVersion + 1;

	//     kinfoLastWriteTime points to the date and time last written to in seconds
    zdssInfoKeys.kinfoLastWriteTime = zdssInfoKeys.kinfoExpansion + 1;

	//     kinfoProgram points to the program which last wrote to this record
	//  Up to 16 characters for the program name
	zdssVals.numberProgram = 16;
    zdssInfoKeys.kinfoProgram = zdssInfoKeys.kinfoLastWriteTime + 1;

	//     First points to the Julian date of the first valid data, Last, last valid data
     //  (non-missing) for times series data.  0 if not calculated or other
    zdssInfoKeys.kinfoFirstDate = zdssInfoKeys.kinfoProgram + numberLongsInBytes(zdssVals.numberProgram);

	zdssInfoKeys.kinfoLastDate = zdssInfoKeys.kinfoFirstDate + 1;

	//  When the record was originally created
    zdssInfoKeys.kinfoCreationTime = zdssInfoKeys.kinfoLastDate + 1;

	//  Unused (used to be password for record)
	zdssInfoKeys.kinfoReserved1 = zdssInfoKeys.kinfoCreationTime + 1;


	//     kinfoInternalHeadAddress points to the address of the internal header
    zdssInfoKeys.kinfoInternalHeadAddress = zdssInfoKeys.kinfoReserved1 + 1;

	//     kinfoInternalHeadNumber points to the length of the internal header, in int 4 words
    zdssInfoKeys.kinfoInternalHeadNumber = zdssInfoKeys.kinfoInternalHeadAddress + 1;

	//     kinfoHeader2Address points to the address of the compression header
    zdssInfoKeys.kinfoHeader2Address = zdssInfoKeys.kinfoInternalHeadNumber + 1;

	//     kinfoHeader2Number points to the length of the compression header, int 4 words
    zdssInfoKeys.kinfoHeader2Number = zdssInfoKeys.kinfoHeader2Address + 1;

	//     kinfoUserHeadAddress points to the address of the User's header
    zdssInfoKeys.kinfoUserHeadAddress = zdssInfoKeys.kinfoHeader2Number + 1;

	//     kinfoUserHeadNumber points to the length of the User's header, int 4 words
    zdssInfoKeys.kinfoUserHeadNumber = zdssInfoKeys.kinfoUserHeadAddress + 1;

	//     kinfoValues1Address points to the data location for the first data array
    zdssInfoKeys.kinfoValues1Address = zdssInfoKeys.kinfoUserHeadNumber + 1;

	//     kinfoValues1Number points to the used length of the first data array
    zdssInfoKeys.kinfoValues1Number = zdssInfoKeys.kinfoValues1Address + 1;

	//     kinfoValues2Address points to the data location for the second data array
    zdssInfoKeys.kinfoValues2Address = zdssInfoKeys.kinfoValues1Number + 1;

	//     kinfoValues2Number points to the used length of the second data array
    zdssInfoKeys.kinfoValues2Number = zdssInfoKeys.kinfoValues2Address + 1;

	//     kinfoValues3Address points to the address of the third data array
    zdssInfoKeys.kinfoValues3Address = zdssInfoKeys.kinfoValues2Number + 1;

	//     kinfoValues3Number points to the size of the third data array
    zdssInfoKeys.kinfoValues3Number = zdssInfoKeys.kinfoValues3Address + 1;

	//  values1Number + values2Number + values3Number = size, in ints, to store
	//  totalAllocatedSize - size, in ints, to allocate on disk
	//      normally totalAllocatedSize = values1Number + values2Number + values3Number, but if the record
	//      will be expanding much (due to real-time storage), the
	//	    totalAllocatedSize might be the final expected length
	//  numberValues - the number of values represented by the data
	//  logicalNumberValues - the fully expanded data set; what the user finally sees


	//     kinfoAllocatedSize points to the allocated space
	//     This may be greater than kinfoValues1Number to allocate space for expansion
	zdssInfoKeys.kinfoAllocatedSize = zdssInfoKeys.kinfoValues3Number + 1;

	zdssInfoKeys.kinfoNumberData = zdssInfoKeys.kinfoAllocatedSize + 1;

	//     kinfoLogicalNumber points to the logical number of data (e.g., uncompressed)
	zdssInfoKeys.kinfoLogicalNumber = zdssInfoKeys.kinfoNumberData + 1;


	//     kinfoAliasesBinAddress is the number of aliases pointing to this record, addresses follow path
    zdssInfoKeys.kinfoAliasesBinAddress = zdssInfoKeys.kinfoLogicalNumber + 1;

	//     KIUNUS is unused space in the record
    zdssInfoKeys.kinfoReserved = zdssInfoKeys.kinfoAliasesBinAddress + 1;

	//     kinfoPathname points to the pathname for this record
    zdssInfoKeys.kinfoPathname = zdssInfoKeys.kinfoReserved + 1;

	//     compute the length of the information block, less the pathname and alias addresses
   zdssVals.infoSize = zdssInfoKeys.kinfoPathname - zdssInfoKeys.kinfoFlag;

	//  Max info size allocated (includes 20 spots for alias addresses)
	zdssVals.maxInfoSize = zdssVals.infoSize + numberLongsInBytes(MAX_PATHNAME_SIZE) + 20;



	/////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////

   //  Now initialize message block
   for (i=0; i<maxMessageAvail; i++) {
	   zmessageAvail.messages[i] = 0;
	   zmessageAvail.messLengths[i] = 0;
	   zmessageAvail.messHandles[i] = 0;
   }
   zmessageAvail.numberMessages = 0;
	zresetMessageLevel();

	stringCopy(zdssVals.cprogramName, sizeof(zdssVals.cprogramName), "not specified", (size_t)13);

	ifltabTemp[0] = 0;
	if (zmessageLevel(ifltabTemp, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltabTemp, DSS_FUNCTION_zinit_ID, "Completed Initialization", "");
	}

}

