#pragma once
#include <stdio.h>
#include <string.h>
#include <string>
extern "C"
{
#include "heclib.h"
#include "zStructSpatialGrid.h"
}

#include "DSSGrid.h"
#include "managedToUnmanaged.h"
#include "ZStructTimeSeriesWrapper.h"
#include "ZStructLocationWrapper.h"
#include "ZStructPairedDataWrapper.h"
#include "ZStructSpatialGridWrapper.h"
#include "ZStruct.h"
#include "ZStructCatalogWrapper.h"
#include "ZStructRecordSizeWrapper.h"
#include "ZStructArrayWrapper.h"
#include "ZStructBasicWrapper.h"
#include "ZStructRecordAddressesWrapper.h"
#include "ZStructSpatialTinWrapper.h"
#include "ZStructTextWrapper.h"
#include "ZStructTransferWrapper.h"
#include "ZTSTimeWindowWrapper.h"

namespace Hec { 
	namespace Dss{
		namespace Native {
			public ref class DSS abstract sealed
			{
			private:
			public:
				static ZStructSpatialGridWrapper^ ZStructSpatialGridNew(String^ filename);

				static String^ AlbersSRS();

				static int ZSpatialGridRetrieve(array<long long>^% ifltab, ZStructSpatialGridWrapper^% gs, bool retrieveData);

				static int ZSpatialGridStore(array<long long>^% ifltab, ZStructSpatialGridWrapper^% gs);

				/// <summary>
				/// Open a DSS file with the full path of the file.  Will decide for you to open DSS6 or DSS7.  Returns the status.  Call Zclose when you are finished with the file, or deletion/rename.
				///</summary>
				/// <returns>A file pointer of the dss file if opened. Failure returns null.</returns>
				static int ZOpen(array<long long>^% ifltab, String^ DSSFileName);

				/// <summary>
				/// ifltab is the file pointer to close.  Closes a DSS file.  Returns status.  Call ZClose when you are finished with the file, or deletion/rename.
				///</summary>
				static int ZClose(array<long long>^ ifltab);

				/// <summary>
				/// Sets the message level for a method set.
				///  Method ID	Value	Method Description
				///  MESS_METHOD_GLOBAL_ID	0	All methods both in DSS version 6 and 7
				///  MESS_METHOD_GENERAL_ID	1	All methods in DSS version 7 (includes those below)
				///  MESS_METHOD_GET_ID	2	Low - level read I / O
				///  MESS_METHOD_PUT_ID	3	Low - level write I / O
				///  MESS_METHOD_READ_ID	4	Read methods, except time series
				///  MESS_METHOD_WRITE_ID	5	Write methods, except time series
				///  MESS_METHOD_PERM_ID	6	Operations for the file header
				///  MESS_METHOD_OPEN_ID	7	Opening and creating a DSS file
				///  MESS_METHOD_CHECK_ID	8	Checking for records
				///  MESS_METHOD_LOCKING_ID	9	Locking and unlocking methods
				///  MESS_METHOD_TS_READ_ID	10	Time series read operations
				///  MESS_METHOD_TS_WRITE_ID	11	Time series write operations
				///  MESS_METHOD_ALIAS_ID	12	Record alias methods
				///  MESS_METHOD_COPY_ID	13	Record copying functions
				///  MESS_METHOD_UTILITY_ID	14	General utility functions(rename, delete, etc.)
				///  MESS_METHOD_CATALOG_ID	15	Cataloging
				///  MESS_METHOD_FILE_CHECK_ID	16	Checking file integrity
				///  MESS_METHOD_JNI_ID	17	Java Native Interface
				///</summary>
				static void ZSetMessageLevel(int methodId, int levelID);

				/// <summary>
				/// returns either STATUS_RECORD_FOUND, STATUS_RECORD_NOT_FOUND or an error code.
				///</summary>
				static int ZCheck(array<long long>^ ifltab, String^ path);

				/// <summary>
				/// Creates and returns a struct to pass data into DSS  Contains regular-interval time series data for float values.  Use ZStructFree when finished.
				///</summary>
				static ZStructTimeSeriesWrapper^ ZStructTsNewRegFloats(String^ pathName, array<float>^% floatValues, int numberValues, String^ startDate, String^ startTime, String^ units, String^ type);

				/// <summary>
				/// Creates and returns a struct to pass data into DSS  Contains regular-interval time series data for double values.  Use ZStructFree when finished.
				///</summary>
				static ZStructTimeSeriesWrapper^ ZStructTsNewRegDoubles(String^ pathName, array<double>^% floatValues, int numberValues, String^ startDate, String^ startTime, String^ units, String^ type);

				/// <summary>
				/// Creates and returns a struct to pass data into DSS  Contains irregular-interval float values.  Use ZStructFree when finished.
				///</summary>
				static ZStructTimeSeriesWrapper^ ZStructTsNewIrregFloats(String^ pathName, array<float>^% doubleValues, int numberValues, array<int>^% itimes, int minSecFlag, String^ startDateBase, String^ units, String^ type);

				/// <summary>
				/// Creates and returns a struct to pass data into DSS  Contains irregular-interval double values.  Use ZStructFree when finished.
				///</summary>
				static ZStructTimeSeriesWrapper^ ZStructTsNewIrregDoubles(String^ pathName, array<double>^% doubleValues, int numberValues, array<int>^% itimes, int minSecFlag, String^ startDateBase, String^ units, String^ type);

				/// <summary>
				/// Creates and returns a struct to pass data into DSS.  A lower level call often used for retreiving data.  Use ZStructFree when finished.
				///</summary>
				static ZStructTimeSeriesWrapper^ ZStructTsNew(String^ pathName);

				/// <summary>
				/// Creates and returns a struct to pass data into DSS.  It specifies a time window and a pathname,which fully defines the data set to read.  Use ZStructFree when finished.
				///</summary>
				static ZStructTimeSeriesWrapper^ ZStructTsNewTimes(String^ pathName, String^ startDate, String^ startTime, String^ endDate, String^ endTime);

				/// <summary>
				/// This is the function to call to store time series data in a HEC-DSS file.  The data set may be regular-interval or irregular interval and the DSS file version may be either version 7 or version 6 (without 7 extensions).  There are no other functions that are used to store time series data. 
				///</summary>
				static int ZTsStore(array<long long>^% ifltab, ZStructTimeSeriesWrapper^% tss, int storageFlag);

				/// <summary>
				///  This is the function to call to retrieve time series data in a HEC-DSS file.  The data set may be regular-interval or irregular interval and the DSS file version may be either version 7 or version 6 (without 7 extensions).  There are no other functions that are used to store time series data. 
				///</summary>
				static int ZTsRetrieve(array<long long>^% ifltab, ZStructTimeSeriesWrapper^% tss, int retrieveFlag, int boolRetrieveDoubles, int boolRetrieveQualityNotes);

				static int ZTsRetrieveEmpty(array<long long>^% ifltab, ZStructTimeSeriesWrapper^% tss);

				static ZStructTransferWrapper^ ZStructTransferNew(String^ pathName, int mode);

				static ZStructSpatialTinWrapper^ ZStructSpatialTinNew(String^ pathName);

				static ZTSTimeWindowWrapper^ ZStructTsNewTimeWindow();

				/// <summary>
				/// zstructPdNewFloats is for storing paired data float values.  Pass in the array of float ordinates (independent variable), and the float values array (dependent variable), the number of ordinates and number of curves.  The floatValues array must contain numberOrdinates * numberCurves values.  Since C doesn’t know about doubly dimensioned arrays (very well), the floatValues array is often a single dimensioned array numberOrdinates * numberCurves values long.
				///</summary>
				static ZStructPairedDataWrapper^ ZStructPdNewFloats(String^ pathname, array<float>^% floatOrdinates, array<float>^% floatValues,
					int numOrdinates, int numberCurves, String^ unitsIndependent, String^ typeIndependent, String^ unitsDependent, String^ typeDependent);

				/// <summary>
				/// zstructPdNewDoubles is for storing paired data double values.  Pass in the array of double ordinates (independent variable), and the double values array (dependent variable), the number of ordinates and number of curves.  The doubleValues array must contain numberOrdinates * numberCurves values.  Since C doesn’t know about doubly dimensioned arrays (very well), the doubleValues array is often a single dimensioned array numberOrdinates * numberCurves values long.
				///</summary>
				static ZStructPairedDataWrapper^ ZStructPdNewDoubles(String^ pathname, array<double>^% doubleOrdinates, array<double>^% doubleValues,
					int numOrdinates, int numberCurves, String^ unitsIndependent, String^ typeIndependent, String^ unitsDependent, String^ typeDependent);

				/// <summary>
				/// Creates and returns a struct to pass data into DSS.  A lower level call often used for retreiving data.  Use ZStructFree when finished.
				///</summary>
				static ZStructPairedDataWrapper^ ZStructPdNew(String^ pathName);

				/// <summary>
				/// Creates and returns a struct to pass data into DSS.  A lower level call often used for retreiving data.  Use ZStructFree when finished.
				///</summary>
				static int ZpdRetrieve(array<long long>^% ifltab, ZStructPairedDataWrapper^% pds, int retrieveDoubleFlag);

				static int ZpdStore(array<long long>^% ifltab, ZStructPairedDataWrapper^% pds, int storageFlag);

				/// <summary>
				/// Creates a new struct used for storing the catalog.  Be sure to call //zstructFree() when you are done.
				///</summary>
				static ZStructCatalogWrapper^ zStructCatalogNew();

				static ZStructLocationWrapper^ ZStructLocationNew(String^ pathName);

				static ZStructLocationWrapper^ ZLocationRetrieve(array<long long>^% ifltab, String^ PathName);

				static int ZLocationStore(array<long long>^% ifltab, ZStructLocationWrapper^% zsl, int storageFlag);

				/// <summary>
				/// Fills in a catalog struct of all pathnames in the DSS file.  If you want the pathnames to be sorted, set boolSorted to 1, otherwise 0.   Returns the number of pathnames in the struct, otherwise a negative for an error.
				///</summary>
				static int ZCatalog(array<long long>^% ifltab, String^ pathWithWild, ZStructCatalogWrapper^% catStruct, int boolSorted);

				/// <summary>
				/// Fills in a catalog struct of all pathnames in the DSS file.  If you want the pathnames to be sorted, set boolSorted to 1, otherwise 0.   Returns the number of pathnames in the struct, otherwise a negative for an error.
				///</summary>
				static int ZCatalogFile(array<long long>^% ifltab, String^ pathWithWild, int boolSorted, String^ catalogFilename);

				/// <summary>
				/// Write the catalog to a file that you have opened.  This is the same as zcatalogFile, but you are responsible for opening and closing the file.  You pass in either a valid (opened) C handle or opened Fortran unit number.  This call is usually made when you want pathnames, but do not want to allocate much memory (and in that case, you should set boolSort to zero).
				///</summary>
				static int ZCatalogToFile(array<long long>^% ifltab, int catalogHandle, int fortranUnit, int boolSort);

				static ZStructRecordSizeWrapper^ zStructRecordSizeNew(String^ filename);

				/// <summary>
				/// //zstructFree frees all memory that HEC-DSS has allocated in the struct passed in.  It will not free memory that the calling functions (outside of the HEC-DSS library) have allocated.  There should be a matching //zstructFree for each zstructNew.
				///</summary>
				static void ZStructFree(ZStruct^% zStruct);

				/// <summary>
				/// To duplicate a record within a file, use the function “zduplicateRecord” with the existing pathname and the new one.  You cannot change the D (date) or E (interval) parts of a time series record; that data must be converted for those parts to change.  If you try to rename those, the record will become unreadable. Returns STATUS_OKAY for a successful rename or an error code for an unsuccessful call.  Errors include the old record does not exist, the new record already exists, you do not have write access, among others.
				///</summary>
				static int ZDuplicateRecord(array<long long>^% ifltab, String^ existingPathname, String^ newPathname);

				/// <summary>
				/// To copy a record to another DSS file, use the function “zcopyRecord”.  You can change the pathname when you copy with the record, buy providing a new name, or you can just use the same name.  You cannot change the D (date) or E (interval) parts of a time series record; that data must be converted for those parts to change.  Returns STATUS_OKAY for a successful rename or an error code for an unsuccessful call.  Errors include the old record does not exist, the new record already exists, you do not have write access, among others.
				///</summary>
				static int ZCopyRecord(array<long long>^% ifltabFrom, array<long long>^% ifltabTo, String^ existingPathname, String^ newPathname);

				/// <summary>
				/// You can rename a single record by calling the function “zrename” and providing the old pathname and the new one.  You cannot rename the D (date) or E (interval) parts of a time series record; that data set must be converted for those parts to change.  If you try to rename those, the record will become unreadable.  Returns STATUS_OKAY for a successful rename or an error code for an unsuccessful call.  Errors include the old record does not exist, the new record already exists, you do not have write access, among others.
				///</summary>
				static int ZRename(array<long long>^% ifltab, String^ existingPathname, String^ newPathname);

				/// <summary>
				/// You can free(a single record by calling the function “zdelete” with the pathname of the record you want to delete.  If you accidently delete a record, it might be recovered by calling zundelete.  However, deleted space is returned to the recycle pool, so that function is not guaranteed to work, and there is less probability of an unfree(as more is written to the file.  “Squeezing” a file permanently removes space from undeleted records and is recommended after many deletes or renames, etc.   Returns STATUS_OKAY for successfully deleting the record, or an error code for an unsuccessful call.  Errors include the record does not exist, or you do not have write access, among others.
				///</summary>
				static int ZDelete(array<long long>^% ifltab, String^ pathname);

				/// <summary>
				/// Returns STATUS_OKAY for successfully undeleting the record, or an error code for an unsuccessful call.
				///</summary>
				static int ZUndelete(array<long long>^% ifltab, String^ pathname);

				static int ZAliasAdd(array<long long>^% ifltab, String^ existingPathname, String^ newPathname);

				static int ZAliasGetPrimary(array<long long>^% ifltab, String^ aliasPathName, String^ primaryPathName, size_t maxLenPrimaryPathname);

				static int ZGetFileVersion(String^ dssFilename);

				static int ZGetVersion(array<long long>^% ifltab);

				static int ZOpenExtended(array<long long>^% ifltab, String^ dssFilename, int fileVersion, int access, int maxExpectedPathnames, int hashSize, int binSize);

				static int ZSet(String^ parameter, String^ charVal, int integerValue);

				static int ZSetFile(array<long long>^% ifltab, String^ parameter, String^ charVal, int integerValue);

				static int ZQuery(String^ parameter, String^ charVal, size_t lenCharVal, array<int>^% integerValue);

				static long long ZInquire(array<long long>^% ifltab, String^ request);

				static int ZInquireChar(array<long long>^% ifltab, String^ request, String^% creturn, size_t creturnSize, array<int>^% number);

				static int ZFileName(String^% fullDssFilename, size_t sizeofFilename, String^ dssFileName, array<int>^% permission);

				static int ZDataType(array<long long>^% ifltab, String^ pathname);

				static String^ ZTypeName(array<long long>^% ifltab, String^ pathname);

				static int ZGetRecordSize(array<long long>^% ifltab, ZStructRecordSizeWrapper^ recordSize);

				static int zSqueezeNeeded(array<long long>^% ifltab);

				static int zSqueeze7(array<long long>^% ifltab, int boolOnlyIfNeeded, int boolInPlace);

				//doesnt work
				//static long long ZGetLastWriteTime(array<long long> ^% ifltab, String ^ pathname);

				static long long ZGetLastWriteTimeFile(array<long long>^% ifltab);

				static unsigned int zGetDataCRC(array<long long>^% ifltab, String^ pathname, unsigned int crcln);

				static int ZWhatChangedSetStart(array<long long>^% ifltab, ZStructCatalogWrapper^ catStruct, String^ pathWithWildChars, int boolUseCRC);

				static int ZWhatChanged(array<long long>^% ifltab, ZStructCatalogWrapper^ catStruct);

				static int ZWhatChangedCompare(array<long long>^% ifltab, ZStructCatalogWrapper^% catStructBefore, ZStructCatalogWrapper^% catStructChanged, String^ pathWithWild, int boolUseCRC);

				static int ZPathNameClean(String^% newPathname, size_t sizeOfNewPathName, String^ oldPathname);

				static int ZPathNameCompare(String^ pathname1, array<long long>^% pathname2, size_t pathnameLength);

				static int ZPathNameCompareCollection(String^ pathname1, String^ pathname2, size_t pathnameLength);

				static int ZPathNameGetPart(String^ pathname, int partPosition, String^% part, size_t sizeOfPart);

				static int ZPathNameSetPart(String^ pathname, size_t sizeOfPathname, String^ part, int partPosition);

				static int ZPathNamePartLengths(String^ pathname, size_t pathnameLen, array<int>^% lengths, int dimOfLengths);

				static int ZPathNamePartPositions(String^ pathname, size_t pathnameLen, array<int>^% positions, int dimOfPositions);

				static int ZPathNameForm(String^ aPart, String^ bPart, String^ cPart, String^ dPart, String^ ePart, String^ fPart, String^% pathname, size_t sizeOfPathname);

				static void ZMaxPart(array<long long>^% ifltab, array<int>^% maxParts);

				static void ZMaxPart7(array<long long>^% ifltab, array<int>^% maxParts);

				static int ZTsGetEPartFromInterval(int intervalSeconds, String^% ePart, size_t sizeOfEpart);

				static float ZMissingFlagFloat();

				static double ZMissingFlagDouble();

				static int ZIsMissingDouble(double value);

				static int ZIsMissing(Object^ value, int lengthValue);

				static void ZSetMissing(int value, int lengthValue);

				static void ZSetMissingFloatArray(array<float>^ values, int numberValues);

				static void ZSetMissingDoubleArray(array<double>^ values, int numberValues);

				static void ZSetUndefined(array<int>^ data, int dataLength);

				static void ZSetMessageGroupLevel(String^ functionGroup, int level);

				static int ZGetMessageLevel(int group);

				static void ZResetMessageLevel();

				static void ZSetMessageLevelFile(array<long long>^% ifltab, int level);

				static int ZMessageAvaliable(array<long long>^% ifltab);

				static String^ ZGetMessage(array<long long>^% ifltab);

				static String^ ZGetMessageAll(array<long long>^% ifltab);

				static void ZClearMessage(array<long long>^% ifltab);

				static void ZClearMessageAll(array<long long>^% ifltab);

				static int ZErrorCheck();

				static int ZCheckFile(array<long long>^% ifltab);

				static int ZAliasRemove(array<long long>^% ifltab, String^ aliasPathname);

				static int ZAliasRemoveAll(array<long long>^% ifltab, String^ aliasPathname);

				static int ZAliasGetPrimary(array<long long>^% ifltab, String^ aliasPathname, String^% primaryPathname, size_t maxLenPrimaryPathname);

				static int ZAliasList(array<long long>^% ifltab, String^ pathname, array<String^>^ pathnameList, int% pathnameListLength);

				static int ZSqueeze(String^ dssFilename);

				static bool IsTimeDefined(int julianDate, int timeSeconds);

				static int GetDateAndTime(int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate, String^% dateString, int sizeOfDateString, String^% hoursMins, int SizeOfHoursMins);

				static int JulianToYearMonthDay(int julian, int% year, int% month, int% day);

				static int YearMonthDayToDate(int year, int month, int day, int style, String^% dateString, size_t sizeOfDateString);

				static int YearMonthDayToJulian(int year, int month, int day);

				static int TimeStringToSeconds(String^ timeString);

				static int DateToYearMonthDay(String^ dateString, int% year, int% month, int% day);

				static int DateToJulian(String^ dateString);

			};
		}
}}
