//This is the main DLL file.
#include "pch.h"
#include "DSS.h"
#include "DSSGrid.h"
namespace Hec {
	namespace Dss {
		namespace Native {
			/// <summary>
			/// Open a DSS file with the full path of the file.  Will decide for you to open DSS6 or DSS7.  Returns the status.  Call Zclose when you are finished with the file, or deletion/rename.
			///</summary>
			/// <returns>A file pointer of the dss file if opened. Failure returns null.</returns>
			int DSS::ZOpen(array<long long>^% ifltab, String ^ DSSFileName)
			{
				pin_ptr<long long> itfltabPinned = &ifltab[0];
				IntPtr marshallToDssFileName = Marshal::StringToHGlobalAnsi(DSSFileName);
				char * ptrToDssFileName = static_cast<char*>(marshallToDssFileName.ToPointer());
				int status = hec_dss_zopen(itfltabPinned, ptrToDssFileName);
				Marshal::FreeHGlobal(marshallToDssFileName);
				return status;
			}

			/// <summary>
			/// ifltab is the file pointer to close.  Closes a DSS file.  Returns status.  Call ZClose when you are finished with the file, or deletion/rename.
			///</summary>
			int DSS::ZClose(array<long long>^ ifltab)
			{
				pin_ptr<long long> itfltabPinned = &ifltab[0];
				return zclose(itfltabPinned);
			}


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
			void DSS::ZSetMessageLevel(int methodId, int levelID)
			{
				zsetMessageLevel(methodId, levelID);
			}

			/// <summary>
			/// returns either STATUS_RECORD_FOUND, STATUS_RECORD_NOT_FOUND or an error code.
			///</summary>
			int DSS::ZCheck(array<long long>^ ifltab, String ^ path)
			{
				pin_ptr<long long> itfltabPinned = &ifltab[0];
				IntPtr marshallToDssFileName = Marshal::StringToHGlobalAnsi(path);
				char * ptrToDssFileName = static_cast<char*>(marshallToDssFileName.ToPointer());
				int status = zcheck(itfltabPinned, ptrToDssFileName);
				Marshal::FreeHGlobal(marshallToDssFileName);
				return status;
			}

			/// <summary>
			/// Creates and returns a struct to pass data into DSS  Contains regular-interval time series data for float values.  Use ZStructFree when finished.
			///</summary>
			ZStructTimeSeriesWrapper ^ DSS::ZStructTsNewRegFloats(String ^ pathName, array<float> ^% floatValues, int numberValues, String ^ startDate, String ^ startTime, String ^ units, String ^ type)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				char * ptrToStartDate = managedToUnmanagedString(startDate);
				char * ptrToStartTime = managedToUnmanagedString(startTime);
				char * ptrToUnits = managedToUnmanagedString(units);
				char * ptrToType = managedToUnmanagedString(type);
				float * floatValuesPinned = managedToUnmanagedFloatArr(floatValues);
				ZStructTimeSeriesWrapper ^ toReturn = gcnew ZStructTimeSeriesWrapper(zstructTsNewRegFloats(ptrToPathName, floatValuesPinned, numberValues, ptrToStartDate, ptrToStartTime, ptrToUnits, ptrToType));
				free(ptrToPathName);//all of these were copied using the dss library call
				free(ptrToStartDate);
				free(ptrToStartTime);
				free(ptrToUnits);
				free(ptrToType);
				toReturn->theStruct->allocated[zSTRUCT_TS_floatValues] = 1; //hacky way to get around the struct not copying float values;
				return toReturn;
			}

			/// <summary>
			/// Creates and returns a struct to pass data into DSS  Contains regular-interval time series data for double values.  Use ZStructFree when finished.
			///</summary>
			ZStructTimeSeriesWrapper ^ DSS::ZStructTsNewRegDoubles(String ^ pathName, array<double> ^% doubleValues, int numberValues, String ^ startDate, String ^ startTime, String ^ units, String ^ type)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				char * ptrToStartDate = managedToUnmanagedString(startDate);
				char * ptrToStartTime = managedToUnmanagedString(startTime);
				char * ptrToUnits = managedToUnmanagedString(units);
				char * ptrToType = managedToUnmanagedString(type);
				double * doubleValuesPinned = managedToUnmanagedDoubleArr(doubleValues);
				ZStructTimeSeriesWrapper ^ toReturn = gcnew ZStructTimeSeriesWrapper(zstructTsNewRegDoubles(ptrToPathName, doubleValuesPinned, numberValues, ptrToStartDate, ptrToStartTime, ptrToUnits, ptrToType));
				free(ptrToPathName);//all of these were copied using the dss library call
				free(ptrToStartDate);
				free(ptrToStartTime);
				free(ptrToUnits);
				free(ptrToType);
				toReturn->theStruct->allocated[zSTRUCT_TS_doubleValues] = 1; //hacky way to get around the struct not copying values;
				return toReturn;
			}

			/// <summary>
			/// Creates and returns a struct to pass data into DSS  Contains irregular-interval double values.  Use ZStructFree when finished.
			///</summary>
			ZStructTimeSeriesWrapper ^ DSS::ZStructTsNewIrregDoubles(String ^ pathName, array<double> ^% doubleValues, int numberValues, array<int> ^% itimes, int timeGranularitySeconds, String ^ startDateBase, String ^ units, String ^ type)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				char * ptrToStartDateBase = managedToUnmanagedString(startDateBase);
				char * ptrToUnits = managedToUnmanagedString(units);
				char * ptrToType = managedToUnmanagedString(type);
				double * doubleValuesPinned = managedToUnmanagedDoubleArr(doubleValues);
				int * itimesPinned = managedToUnmanagedIntArr(itimes);
				ZStructTimeSeriesWrapper ^ toReturn = gcnew ZStructTimeSeriesWrapper(zstructTsNewIrregDoubles(ptrToPathName, doubleValuesPinned, numberValues, itimesPinned, timeGranularitySeconds, ptrToStartDateBase, ptrToUnits, ptrToType));
				free(ptrToPathName);//all of these were copied using the dss library call
				free(ptrToStartDateBase);
				free(ptrToUnits);
				free(ptrToType);
				toReturn->theStruct->allocated[zSTRUCT_TS_doubleValues] = 1;//hacky way to get around the struct not copying values;
				toReturn->theStruct->allocated[zSTRUCT_TS_times] = 1;
				return toReturn;
			}

			/// <summary>
			/// Creates and returns a struct to pass data into DSS  Contains irregular-interval float values.  Use ZStructFree when finished.
			///</summary>
			ZStructTimeSeriesWrapper ^ DSS::ZStructTsNewIrregFloats(String ^ pathName, array<float> ^% floatValues, int numberValues, array<int> ^% itimes, int timeGranularitySeconds, String ^ startDateBase, String ^ units, String ^ type)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				char * ptrToStartDateBase = managedToUnmanagedString(startDateBase);
				char * ptrToUnits = managedToUnmanagedString(units);
				char * ptrToType = managedToUnmanagedString(type);
				float * floatValuesPinned = managedToUnmanagedFloatArr(floatValues);
				int * itimesPinned = managedToUnmanagedIntArr(itimes);
				ZStructTimeSeriesWrapper ^ toReturn = gcnew ZStructTimeSeriesWrapper(zstructTsNewIrregFloats(ptrToPathName, floatValuesPinned, numberValues, itimesPinned, timeGranularitySeconds, ptrToStartDateBase, ptrToUnits, ptrToType));
				free(ptrToPathName);//all of these were copied using the dss library call
				free(ptrToStartDateBase);
				free(ptrToUnits);
				free(ptrToType);
				toReturn->theStruct->allocated[zSTRUCT_TS_floatValues] = 1;//hacky way to get around the struct not copying values;
				toReturn->theStruct->allocated[zSTRUCT_TS_times] = 1;
				return toReturn;
			}

			/// <summary>
			/// Creates and returns a struct to pass data into DSS.  A lower level call often used for retreiving data.  Use ZStructFree when finished.
			///</summary>
			ZStructTimeSeriesWrapper ^ DSS::ZStructTsNew(String ^ pathName)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				ZStructTimeSeriesWrapper ^ toReturn = gcnew ZStructTimeSeriesWrapper(zstructTsNew(ptrToPathName));
				free(ptrToPathName);//all of these were copied using the dss library call
				return toReturn;
			}

			/// <summary>
			/// Creates and returns a struct to pass data into DSS.  It specifies a time window and a pathname,which fully defines the data set to read.  Use ZStructFree when finished.
			///</summary>
			ZStructTimeSeriesWrapper ^ DSS::ZStructTsNewTimes(String ^ pathName, String ^ startDate, String ^ startTime, String ^ endDate, String ^ endTime)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				char * ptrToStartDate = managedToUnmanagedString(startDate);
				char * ptrToStartTime = managedToUnmanagedString(startTime);
				char * ptrToEndDate = managedToUnmanagedString(endDate);
				char * ptrToEndTime = managedToUnmanagedString(endTime);
				ZStructTimeSeriesWrapper ^ toReturn = gcnew ZStructTimeSeriesWrapper(zstructTsNewTimes(ptrToPathName, ptrToStartDate, ptrToStartTime, ptrToEndDate, ptrToEndTime));
				free(ptrToPathName);//all of these were copied using the dss library call
				free(ptrToStartDate);
				free(ptrToStartTime);
				free(ptrToEndDate);
				free(ptrToEndTime);
				return toReturn;
			}

			/// <summary>
			/// This is the function to call to store time series data in a HEC-DSS file.  The data set may be regular-interval or irregular interval and the DSS file version may be either version 7 or version 6 (without 7 extensions).  There are no other functions that are used to store time series data. 
			///</summary>
			int DSS::ZTsStore(array<long long> ^% ifltab, ZStructTimeSeriesWrapper ^% tss, int storageFlag)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return ztsStore(ifltabPinned, tss->theStruct, storageFlag);
			}

			/// <summary>
			///  This is the function to call to retrieve time series data in a HEC-DSS file.  The data set may be regular-interval or irregular interval and the DSS file version may be either version 7 or version 6 (without 7 extensions).  There are no other functions that are used to read time series data. 
			///</summary>
			int DSS::ZTsRetrieve(array<long long> ^% ifltab, ZStructTimeSeriesWrapper ^% tss, int retrieveFlag, int boolRetrieveDoubles, int boolRetrieveQualityNotes)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return ztsRetrieve(ifltabPinned, tss->theStruct, retrieveFlag, boolRetrieveDoubles, boolRetrieveQualityNotes);
			}


			/// <summary>
			/// used to read units, or type from the header
			/// example units='INCHES',   type='PER-CUM'
			///</summary>
			char* getStringFromHeader(int* header)
			{
				int start = 1;
				int start2 = 1;
				int max_len = 8;
				char buffer[10];
				memset(buffer, '\0', 10);
				holchr_(header, &start, &max_len, buffer, &start2, sizeof(buffer)-1);
				return mallocAndCopyTrim(buffer);
			}


			/// <summary>
			///  retrieve an empty time series, skip the data,  get datatype and units.
			///</summary>
			int DSS::ZTsRetrieveEmpty(array<long long>^% ifltab, ZStructTimeSeriesWrapper^% tss)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];

				int version = zgetVersion(ifltabPinned);
				zStructTransfer* transfer;
				transfer = zstructTransferNew(tss->theStruct->pathname, 0);
				transfer->internalHeaderMode = 1;
				int status = zread(ifltabPinned, transfer);
				if (status == 0)
				{
					if (version == 7)
					{
						int intervalType = ztsProcessTimes(ifltabPinned, tss->theStruct, 0);
						status = ztsInternalHeaderUnpack(tss->theStruct, transfer->internalHeader, transfer->internalHeaderNumber);
					}
					else if (version == 6)
					{
						int v6header_units = 1;
						int v6header_type = 3;
						tss->theStruct->units = getStringFromHeader(&transfer->internalHeader[v6header_units]);
						tss->theStruct->type = getStringFromHeader(&transfer->internalHeader[v6header_type]);

					}
				}
			/*	printf("transfer->dataType = %d\n", transfer->dataType);
				printf("units = %s\n", tss->theStruct->units);
				printf("type = %s\n", tss->theStruct->type);
			*/

					zstructFree(transfer);
					return status;
			}


			ZStructTransferWrapper ^ DSS::ZStructTransferNew(String ^ pathName, int mode)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				ZStructTransferWrapper ^ toReturn = gcnew ZStructTransferWrapper(zstructTransferNew(ptrToPathName, mode));
				free(ptrToPathName);//all of these were copied using the dss library call
				return toReturn;
			}


			/// <summary>
			/// zstructPdNewFloats is for storing paired data float values.  Pass in the array of float ordinates (independent variable), and the float values array (dependent variable), the number of ordinates and number of curves.  The floatValues array must contain numberOrdinates * numberCurves values.  Since C doesn’t know about doubly dimensioned arrays (very well), the floatValues array is often a single dimensioned array numberOrdinates * numberCurves values long.
			///</summary>
			ZStructPairedDataWrapper ^ DSS::ZStructPdNewFloats(String ^ pathname, array<float> ^% floatOrdinates, array<float> ^% floatValues,
				int numOrdinates, int numberCurves, String ^ unitsIndependent, String ^ typeIndependent, String ^ unitsDependent, String ^ typeDependent)
			{
				char * ptrToPathName = managedToUnmanagedString(pathname);
				char * ptrToUnitsIndependent = managedToUnmanagedString(unitsIndependent);
				char * ptrToTypeIndependent = managedToUnmanagedString(typeIndependent);
				char * ptrToUnitsDependent = managedToUnmanagedString(unitsDependent);
				char * ptrToTypeDependent = managedToUnmanagedString(typeDependent);
				float * floatValuesPinned = managedToUnmanagedFloatArr(floatValues);
				float * floatOrdinatesPinned = managedToUnmanagedFloatArr(floatOrdinates);
				ZStructPairedDataWrapper ^ toReturn = gcnew ZStructPairedDataWrapper(zstructPdNewFloats(ptrToPathName, floatOrdinatesPinned, floatValuesPinned, numOrdinates, numberCurves, ptrToUnitsIndependent, ptrToTypeIndependent, ptrToUnitsDependent, ptrToTypeDependent));
				free(ptrToPathName);//all of these were copied using the dss library call
				free(ptrToUnitsIndependent);
				free(ptrToTypeIndependent);
				free(ptrToUnitsDependent);
				free(ptrToTypeDependent);
				toReturn->theStruct->allocated[zSTRUCT_PD_floatValues] = 1;
				toReturn->theStruct->allocated[zSTRUCT_PD_floatOrdinates] = 1;
				return toReturn;
			}

			/// <summary>
			/// zstructPdNewDoubles is for storing paired data double values.  Pass in the array of double ordinates (independent variable), and the double values array (dependent variable), the number of ordinates and number of curves.  The doubleValues array must contain numberOrdinates * numberCurves values.  Since C doesn’t know about doubly dimensioned arrays (very well), the doubleValues array is often a single dimensioned array numberOrdinates * numberCurves values long.
			///</summary>
			ZStructPairedDataWrapper ^ DSS::ZStructPdNewDoubles(String ^ pathname, array<double> ^% doubleOrdinates, array<double> ^% doubleValues,
				int numOrdinates, int numberCurves, String ^ unitsIndependent, String ^ typeIndependent, String ^ unitsDependent, String ^ typeDependent)
			{
				char * ptrToPathName = managedToUnmanagedString(pathname);
				char * ptrToUnitsIndependent = managedToUnmanagedString(unitsIndependent);
				char * ptrToTypeIndependent = managedToUnmanagedString(typeIndependent);
				char * ptrToUnitsDependent = managedToUnmanagedString(unitsDependent);
				char * ptrToTypeDependent = managedToUnmanagedString(typeDependent);
				double * floatValuesPinned = managedToUnmanagedDoubleArr(doubleValues);
				double * floatOrdinatesPinned = managedToUnmanagedDoubleArr(doubleOrdinates);
				ZStructPairedDataWrapper ^ toReturn = gcnew ZStructPairedDataWrapper(zstructPdNewDoubles(ptrToPathName, floatOrdinatesPinned, floatValuesPinned, numOrdinates, numberCurves, ptrToUnitsIndependent, ptrToTypeIndependent, ptrToUnitsDependent, ptrToTypeDependent));
				free(ptrToPathName);//all of these were copied using the dss library call
				free(ptrToUnitsIndependent);
				free(ptrToTypeIndependent);
				free(ptrToUnitsDependent);
				free(ptrToTypeDependent);
				toReturn->theStruct->allocated[zSTRUCT_PD_doubleValues] = 1;
				toReturn->theStruct->allocated[zSTRUCT_PD_doubleOridnates] = 1;
				return toReturn;
			}

			/// <summary>
			/// Creates and returns a struct to pass data into DSS.  A lower level call often used for retreiving data.  Use ZStructFree when finished.
			///</summary>
			ZStructPairedDataWrapper ^ DSS::ZStructPdNew(String ^ pathName)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				ZStructPairedDataWrapper ^ toReturn = gcnew ZStructPairedDataWrapper(zstructPdNew(ptrToPathName));
				return toReturn;
			}

			/// <summary>
			/// Creates and returns a struct to pass data into DSS.  A lower level call often used for retreiving data.  Use ZStructFree when finished.
			///</summary>
			int DSS::ZpdRetrieve(array<long long> ^% ifltab, ZStructPairedDataWrapper ^% pds, int retrieveDoubleFlag)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zpdRetrieve(ifltabPinned, pds->theStruct, retrieveDoubleFlag);
			}

			int DSS::ZpdStore(array<long long>^% ifltab, ZStructPairedDataWrapper ^% pds, int storageFlag)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zpdStore(ifltabPinned, pds->theStruct, storageFlag);
			}


			/// <summary>
			/// You can delete a single record by calling the function “zdelete” with the pathname of the record you want to delete.  If you accidently delete a record, it might be recovered by calling zundelete.  However, deleted space is returned to the recycle pool, so that function is not guaranteed to work, and there is less probability of an undelete as more is written to the file.  “Squeezing” a file permanently removes space from undeleted records and is recommended after many deletes or renames, etc.   Returns STATUS_OKAY for successfully deleting the record, or an error code for an unsuccessful call.  Errors include the record does not exist, or you do not have write access, among others.
			///</summary>
			int DSS::ZDelete(array<long long> ^% ifltab, String ^ pathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToPathName = Marshal::StringToHGlobalAnsi(pathname);
				char * ptrToPathName = static_cast<char*>(marshallToPathName.ToPointer());
				int toReturn = zdelete(ifltabPinned, ptrToPathName);
				Marshal::FreeHGlobal(marshallToPathName);
				return toReturn;
			}

			int DSS::ZSet(String ^ parameter, String ^ charVal, int integerValue)
			{
				IntPtr marshallToParameter = Marshal::StringToHGlobalAnsi(parameter);
				IntPtr marshallToCharVal = Marshal::StringToHGlobalAnsi(charVal);
				char * ptrToParameter = static_cast<char*>(marshallToParameter.ToPointer());
				char * ptrTocharVal = static_cast<char*>(marshallToCharVal.ToPointer());
				int toReturn = zset(ptrToParameter, ptrTocharVal, integerValue);
				Marshal::FreeHGlobal(marshallToParameter);
				Marshal::FreeHGlobal(marshallToCharVal);
				return toReturn;
			}

			int DSS::ZDataType(array<long long> ^% ifltab, String ^ pathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathname);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zdataType(ifltabPinned, strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::zSqueezeNeeded(array<long long> ^% ifltab)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zsqueezeNeeded(ifltabPinned);
			}

			int DSS::zSqueeze7(array<long long> ^% ifltab, int boolOnlyIfNeeded, int boolInPlace)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zsqueeze7(ifltabPinned, boolOnlyIfNeeded, boolInPlace);
			}

			int DSS::ZSqueeze(String ^ dssFilename)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dssFilename);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zsqueeze(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			String ^ DSS::AlbersSRS() {
				String ^ rval = gcnew String( SHG_SRC_DEFINITION);
				return rval;
			}


			bool DSS::IsTimeDefined(int julianDate, int timeSeconds)
			{
				return isTimeDefined(julianDate, timeSeconds) == 1;
			}

			int DSS::GetDateAndTime(int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate, String ^% dateString, int sizeOfDateString, String ^% hoursMins, int sizeOfHoursMins)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(dateString);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(hoursMins);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				int toReturn = getDateAndTime(timeMinOrSec, timeGranularitySeconds, julianBaseDate, ptrToFirst, sizeOfDateString, ptrToSecond, sizeOfHoursMins);
				dateString = gcnew String(ptrToFirst);
				hoursMins = gcnew String(ptrToSecond);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
				return toReturn;
			}

			int DSS::JulianToYearMonthDay(int julian, int % year, int % month, int % day)
			{
				pin_ptr<int> yearPinned = &year;
				pin_ptr<int> monthPinned = &month;
				pin_ptr<int> dayPinned = &day;
				return julianToYearMonthDay(julian, yearPinned, monthPinned, dayPinned);
			}

			int DSS::YearMonthDayToDate(int year, int month, int day, int style, String ^% dateString, size_t sizeOfDateString)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dateString);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = yearMonthDayToDate(year, month, day, style, strPtr, sizeOfDateString);
				dateString = gcnew String(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::YearMonthDayToJulian(int year, int month, int day)
			{
				return yearMonthDayToJulian(year, month, day);
			}

			int DSS::TimeStringToSeconds(String ^ timeString)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(timeString);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = timeStringToSeconds(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::DateToYearMonthDay(String ^ dateString, int % year, int % month, int % day)
			{
				pin_ptr<int> yearPinned = &year;
				pin_ptr<int> monthPinned = &month;
				pin_ptr<int> dayPinned = &day;
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dateString);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = dateToYearMonthDay(strPtr, yearPinned, monthPinned, dayPinned);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::DateToJulian(String ^ dateString)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dateString);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = dateToJulian(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			
		}
	}
}