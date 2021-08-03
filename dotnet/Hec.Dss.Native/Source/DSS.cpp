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
				int status = zopen(itfltabPinned, ptrToDssFileName);
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
			/// Open a DSS log file with the full path of the file. Returns the status.  Call ZCloseLog when you are finished with the file, or deletion/rename.
			///</summary>
			/// <returns>A file pointer of the dss file if opened. Failure returns null.</returns>
			int DSS::ZOpenLog(String ^ DSSFileName)
			{
				IntPtr marshallToDssFileName = Marshal::StringToHGlobalAnsi(DSSFileName);
				char * ptrToDssFileName = static_cast<char*>(marshallToDssFileName.ToPointer());
				int status = zopenLog(ptrToDssFileName);
				Marshal::FreeHGlobal(marshallToDssFileName);
				return status;
			}

			/// <summary>
			/// Closes a DSSlog file.  Returns status.  Call ZCloseLog when you are finished with the log file
			///</summary>
			void DSS::ZCloseLog()
			{
				zcloseLog();
				return;
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
			/// Will tell you if the given status is an error or not
			///</summary>
			int DSS::ZIsError(int status)
			{
				return zisError(status);
			}

			/// <summary>
			/// Will tell you the error severity of the error
			///</summary>
			int DSS::ZErrorSeverity(int status)
			{
				return zerrorSeverity(status);
			}

			/// <summary>
			/// returns either STATUS_RECORD_FOUND, STATUS_RECORD_NOT_FOUND or an error code.
			///</summary>
			int DSS::ZCheck(array<long long>^ ifltab, String ^ DSSFileName)
			{
				pin_ptr<long long> itfltabPinned = &ifltab[0];
				IntPtr marshallToDssFileName = Marshal::StringToHGlobalAnsi(DSSFileName);
				char * ptrToDssFileName = static_cast<char*>(marshallToDssFileName.ToPointer());
				int status = zcheck(itfltabPinned, ptrToDssFileName);
				Marshal::FreeHGlobal(marshallToDssFileName);
				return status;
			}

			/// <summary>
			/// returns zero, or the last error code that occurred
			///</summary>
			int DSS::ZErrorCode(array<long long>^ ifltab)
			{
				pin_ptr<long long> itfltabPinned = &ifltab[0];
				return zerrorCode(itfltabPinned);
			}

			/// <summary>
			/// returns the severity of the most severe error
			///</summary>
			int DSS::ZFileError(array<long long>^ ifltab)
			{
				pin_ptr<long long> itfltabPinned = &ifltab[0];
				return zfileError(itfltabPinned);
			}

			/// <summary>
			/// returns the missing flag value
			///</summary>
			float DSS::ZMissingFlag()
			{
				return zmissingFlag();
			}

			/// <summary>
			/// Sets the missing flag value with a float
			///</summary>
			void DSS::ZSetMissingFloat(float value)
			{
				pin_ptr<float> valuePinned = &value;
				zsetMissingFloat(valuePinned);
				return;
			}

			/// <summary>
			/// Sets the missing flag value with a double
			///</summary>
			void DSS::ZSetMissingDouble(double value)
			{
				pin_ptr<double> valuePinned = &value;
				zsetMissingDouble(valuePinned);
				return;
			}

			/// <summary>
			/// Checks to see if a value is missing
			///</summary>
			int DSS::ZIsMissingFloat(float value)
			{
				return zisMissingFloat(value);
			}

			/// <summary>
			/// Checks to see if a value is missing
			///</summary>
			int DSS::ZIsMissingFloat(double value)
			{
				return zisMissingDouble(value);
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


			/// <summary>
			///  Gets the time window for a data set
			///</summary>  
			void DSS::ZTSends(array<long long> ^% ifltab, String ^ cpath, array<int> ^ searchOption, array<int> ^ startJulian, array<int> ^ startMinutes, array<int> ^ endJulian, array<int> ^ endMinutes, array<int> ^ exists, size_t len_cpath)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCPath = Marshal::StringToHGlobalAnsi(cpath);
				char * ptrToCPath = static_cast<char*>(marshallToCPath.ToPointer());
				pin_ptr<int> searchOptionPinned = &searchOption[0];
				pin_ptr<int> startJulianPinned = &startJulian[0];
				pin_ptr<int> startMinutesPinned = &startMinutes[0];
				pin_ptr<int> endJulianPinned = &endJulian[0];
				pin_ptr<int> endMintesPinned = &endMinutes[0];
				pin_ptr<int> existsPinned = &exists[0];
				ztsends_(ifltabPinned, ptrToCPath, searchOptionPinned, startJulianPinned, startMinutesPinned, endJulianPinned, endMintesPinned, existsPinned, len_cpath);
				Marshal::FreeHGlobal(marshallToCPath);
				return;
			}

			ZStructArrayWrapper ^ DSS::ZStructArrayNew(String ^ pathName)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				ZStructArrayWrapper ^ toReturn = gcnew ZStructArrayWrapper(zstructArrayNew(ptrToPathName));
				free(ptrToPathName);//all of these were copied using the dss library call
				return toReturn;
			}

			ZStructTransferWrapper ^ DSS::ZStructTransferNew(String ^ pathName, int mode)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				ZStructTransferWrapper ^ toReturn = gcnew ZStructTransferWrapper(zstructTransferNew(ptrToPathName, mode));
				free(ptrToPathName);//all of these were copied using the dss library call
				return toReturn;
			}

			ZStructTextWrapper ^ DSS::ZStructTextNew(String ^ pathName)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				ZStructTextWrapper ^ toReturn = gcnew ZStructTextWrapper(zstructTextNew(ptrToPathName));
				free(ptrToPathName);//all of these were copied using the dss library call
				return toReturn;
			}

			ZStructTextWrapper ^ DSS::ZStructTextStringNew(String ^ pathName, String ^ text)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				char * ptrToText = managedToUnmanagedString(text);
				ZStructTextWrapper ^ toReturn = gcnew ZStructTextWrapper(zstructTextStringNew(ptrToPathName, ptrToText));
				free(ptrToPathName);//all of these were copied using the dss library call
				toReturn->theStruct->allocated[zSTRUCT_TX_textString] = 1;//hacky way to get around the struct not copying values;
				return toReturn;
			}

			ZStructLocationWrapper ^ DSS::ZLocationRetrieve(array<long long> ^% ifltab, String ^ pathName)
			{
				int ver = (int)ifltab[0];

				char * ptrToPathName = managedToUnmanagedString(pathName);
				zStructLocation* loc = zstructLocationNew(ptrToPathName);
				loc->xOrdinate = -9999;
				loc->yOrdinate = -9999;
				loc->zOrdinate = -9999;

				ZStructLocationWrapper ^ toReturn = gcnew ZStructLocationWrapper(loc);
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				if( ver >6)
				  zlocationRetrieve(ifltabPinned, loc);
				free(ptrToPathName);//all of these were copied using the dss library call
			
				return toReturn;
			}

			ZStructLocationWrapper ^ DSS::ZStructLocationNew(String ^ pathName)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				ZStructLocationWrapper ^ toReturn = gcnew ZStructLocationWrapper(zstructLocationNew(ptrToPathName));
				free(ptrToPathName);//all of these were copied using the dss library call
				return toReturn;
			}

			int DSS::ZLocationStore(array<long long> ^% ifltab, ZStructLocationWrapper ^% zsl, int storageFlag)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				int status = zlocationStore(ifltabPinned, zsl->theStruct, storageFlag);

				return status;

			}

			int DSS::ZLocationStructValid(ZStructLocationWrapper ^% zsl)
			{
				return zlocationStructValid(zsl->theStruct);
			}

			ZStructSpatialTinWrapper ^ DSS::ZStructSpatialTinNew(String ^ pathName)
			{
				char * ptrToPathName = managedToUnmanagedString(pathName);
				ZStructSpatialTinWrapper ^ toReturn = gcnew ZStructSpatialTinWrapper(zstructSpatialTinNew(ptrToPathName));
				free(ptrToPathName);//all of these were copied using the dss library call
				return toReturn;
			}

			ZTSTimeWindowWrapper ^ DSS::ZStructTsNewTimeWindow()
			{
				return gcnew ZTSTimeWindowWrapper(zstructTsNewTimeWindow());
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
			/// Creates a new struct used for storing the catalog.  Be sure to call zstructFree() when you are done.
			///</summary>
			ZStructCatalogWrapper ^ DSS::zStructCatalogNew()
			{
				return gcnew ZStructCatalogWrapper(zstructCatalogNew());
			}

			/// <summary>
			/// Fills in a catalog struct of all pathnames in the DSS file.  If you want the pathnames to be sorted, set boolSorted to 1, otherwise 0.   Returns the number of pathnames in the struct, otherwise a negative for an error.
			///</summary>
			int DSS::ZCatalog(array<long long> ^% ifltab, String ^ pathWithWild, ZStructCatalogWrapper ^% catStruct, int boolSorted)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToPathWithWild = Marshal::StringToHGlobalAnsi(pathWithWild);
				char * ptrToPathWithWild = static_cast<char*>(marshallToPathWithWild.ToPointer());
				int toReturn = zcatalog(ifltabPinned, ptrToPathWithWild, catStruct->theStruct, boolSorted);
				Marshal::FreeHGlobal(marshallToPathWithWild);
				return toReturn;
			}

			/// <summary>
			/// Fills in a catalog struct of all pathnames in the DSS file.  If you want the pathnames to be sorted, set boolSorted to 1, otherwise 0.   Returns the number of pathnames in the struct, otherwise a negative for an error.
			///</summary>
			int DSS::ZCatalogFile(array<long long> ^% ifltab, String ^ pathWithWild, int boolSorted, String ^ catalogFilename)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToPathWithWild = Marshal::StringToHGlobalAnsi(pathWithWild);
				char * ptrToPathWithWild = static_cast<char*>(marshallToPathWithWild.ToPointer());
				IntPtr marshallToCatalogFilename = Marshal::StringToHGlobalAnsi(catalogFilename);
				char * ptrToCatalogFilename = static_cast<char*>(marshallToCatalogFilename.ToPointer());
				int toReturn = zcatalogFile(ifltabPinned, ptrToCatalogFilename, boolSorted, ptrToPathWithWild);
				Marshal::FreeHGlobal(marshallToPathWithWild);
				Marshal::FreeHGlobal(marshallToCatalogFilename);
				return toReturn;
			}

			/// <summary>
			/// Write the catalog to a file that you have opened.  This is the same as zcatalogFile, but you are responsible for opening and closing the file.  You pass in either a valid (opened) C handle or opened Fortran unit number.  This call is usually made when you want pathnames, but do not want to allocate much memory (and in that case, you should set boolSort to zero).
			///</summary>
			int DSS::ZCatalogToFile(array<long long> ^% ifltab, int catalogHandle, int fortranUnit, int boolSort)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zcatalogToFile(ifltabPinned, catalogHandle, fortranUnit, boolSort);
			}

			ZStructRecordSizeWrapper ^ DSS::zStructRecordSizeNew(String ^ filename)
			{
				char * ptrToFilename = managedToUnmanagedString(filename);
				ZStructRecordSizeWrapper ^ toReturn = gcnew ZStructRecordSizeWrapper(zstructRecordSizeNew(ptrToFilename));
				return toReturn;
			}

			/// <summary>
			/// zstructFree frees all memory that HEC-DSS has allocated in the struct passed in.  It will not free memory that the calling functions (outside of the HEC-DSS library) have allocated.  There should be a matching zstructFree for each zstructNew.
			///</summary>
			void DSS::ZStructFree(ZStruct ^% zStruct)
			{
				delete zStruct;
			}

			/// <summary>
			/// To duplicate a record within a file, use the function “zduplicateRecord” with the existing pathname and the new one.  You cannot change the D (date) or E (interval) parts of a time series record; that data must be converted for those parts to change.  If you try to rename those, the record will become unreadable. Returns STATUS_OKAY for a successful rename or an error code for an unsuccessful call.  Errors include the old record does not exist, the new record already exists, you do not have write access, among others.
			///</summary>
			int DSS::ZDuplicateRecord(array<long long> ^% ifltab, String ^ existingPathname, String ^ newPathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToExistingPathName = Marshal::StringToHGlobalAnsi(existingPathname);
				char * ptrToExistingPathName = static_cast<char*>(marshallToExistingPathName.ToPointer());
				IntPtr marshallToNewPathName = Marshal::StringToHGlobalAnsi(newPathname);
				char * ptrToNewPathName = static_cast<char*>(marshallToNewPathName.ToPointer());
				int toReturn = zduplicateRecord(ifltabPinned, ptrToExistingPathName, ptrToNewPathName);
				Marshal::FreeHGlobal(marshallToExistingPathName);
				Marshal::FreeHGlobal(marshallToNewPathName);
				return toReturn;
			}

			/// <summary>
			/// To copy a record to another DSS file, use the function “zcopyRecord”.  You can change the pathname when you copy with the record, buy providing a new name, or you can just use the same name.  You cannot change the D (date) or E (interval) parts of a time series record; that data must be converted for those parts to change.  Returns STATUS_OKAY for a successful rename or an error code for an unsuccessful call.  Errors include the old record does not exist, the new record already exists, you do not have write access, among others.
			///</summary>
			int DSS::ZCopyRecord(array<long long> ^% ifltabFrom, array<long long> ^% ifltabTo, String ^ existingPathname, String ^ newPathname)
			{
				pin_ptr<long long> ifltabFromPinned = &ifltabFrom[0];
				pin_ptr<long long> ifltabToPinned = &ifltabTo[0];
				IntPtr marshallToExistingPathName = Marshal::StringToHGlobalAnsi(existingPathname);
				char * ptrToExistingPathName = static_cast<char*>(marshallToExistingPathName.ToPointer());
				IntPtr marshallToNewPathName = Marshal::StringToHGlobalAnsi(newPathname);
				char * ptrToNewPathName = static_cast<char*>(marshallToNewPathName.ToPointer());
				int toReturn = zcopyRecord(ifltabFromPinned, ifltabToPinned, ptrToExistingPathName, ptrToNewPathName);
				Marshal::FreeHGlobal(marshallToExistingPathName);
				Marshal::FreeHGlobal(marshallToNewPathName);
				return toReturn;
			}

			/// <summary>
			/// You can rename a single record by calling the function “zrename” and providing the old pathname and the new one.  You cannot rename the D (date) or E (interval) parts of a time series record; that data set must be converted for those parts to change.  If you try to rename those, the record will become unreadable.  Returns STATUS_OKAY for a successful rename or an error code for an unsuccessful call.  Errors include the old record does not exist, the new record already exists, you do not have write access, among others.
			///</summary>
			int DSS::ZRename(array<long long> ^% ifltab, String ^ existingPathname, String ^ newPathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToExistingPathName = Marshal::StringToHGlobalAnsi(existingPathname);
				IntPtr marshallToNewPathName = Marshal::StringToHGlobalAnsi(newPathname);
				char * ptrToExistingPathName = static_cast<char*>(marshallToExistingPathName.ToPointer());
				char * ptrToNewPathName = static_cast<char*>(marshallToNewPathName.ToPointer());
				int toReturn = zrename(ifltabPinned, ptrToExistingPathName, ptrToNewPathName);
				Marshal::FreeHGlobal(marshallToExistingPathName);
				Marshal::FreeHGlobal(marshallToNewPathName);
				return toReturn;
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

			/// <summary>
			/// Returns STATUS_OKAY for successfully undeleting the record, or an error code for an unsuccessful call.
			///</summary>
			int DSS::ZUndelete(array<long long> ^% ifltab, String ^ pathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToPathName = Marshal::StringToHGlobalAnsi(pathname);
				char * ptrToPathName = static_cast<char*>(marshallToPathName.ToPointer());
				int toReturn = zundelete(ifltabPinned, ptrToPathName);
				Marshal::FreeHGlobal(marshallToPathName);
				return toReturn;
			}

			int DSS::ZAliasAdd(array<long long> ^% ifltab, String ^ existingPathname, String ^ newPathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToExistingPathName = Marshal::StringToHGlobalAnsi(existingPathname);
				IntPtr marshallToNewPathName = Marshal::StringToHGlobalAnsi(newPathname);
				char * ptrToExistingPathName = static_cast<char*>(marshallToExistingPathName.ToPointer());
				char * ptrToNewPathName = static_cast<char*>(marshallToNewPathName.ToPointer());
				int toReturn = zaliasAdd(ifltabPinned, ptrToExistingPathName, ptrToNewPathName);
				Marshal::FreeHGlobal(marshallToExistingPathName);
				Marshal::FreeHGlobal(marshallToNewPathName);
				return toReturn;
			}

			int DSS::ZAliasGetPrimary(array<long long> ^% ifltab, String ^ aliasPathName, String ^ primaryPathName, size_t maxLenPrimaryPathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToAliasPathName = Marshal::StringToHGlobalAnsi(aliasPathName);
				IntPtr marshallToPrimaryPathName = Marshal::StringToHGlobalAnsi(primaryPathName);
				char * ptrToAliasPathName = static_cast<char*>(marshallToAliasPathName.ToPointer());
				char * ptrToPrimaryPathName = static_cast<char*>(marshallToPrimaryPathName.ToPointer());
				int toReturn = zaliasGetPrimary(ifltabPinned, ptrToAliasPathName, ptrToPrimaryPathName, maxLenPrimaryPathname);
				Marshal::FreeHGlobal(marshallToAliasPathName);
				Marshal::FreeHGlobal(marshallToPrimaryPathName);
				return toReturn;
			}

			int DSS::ZGetFileVersion(String ^ dssFilename)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dssFilename);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zgetFileVersion(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZGetVersion(array<long long> ^% ifltab)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zgetVersion(ifltabPinned);
			}

			int DSS::ZOpenExtended(array<long long> ^% ifltab, String ^ dssFilename, int fileVersion, int access, int maxExpectedPathnames, int hashSize, int binSize)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dssFilename);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zopenExtended(ifltabPinned, strPtr, fileVersion, access, maxExpectedPathnames, hashSize, binSize);
				Marshal::FreeHGlobal(marshallToCharStar);
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

			int DSS::ZSetFile(array<long long> ^% ifltab, String ^ parameter, String ^ charVal, int integerValue)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToParameter = Marshal::StringToHGlobalAnsi(parameter);
				IntPtr marshallToCharVal = Marshal::StringToHGlobalAnsi(charVal);
				char * ptrToParameter = static_cast<char*>(marshallToParameter.ToPointer());
				char * ptrTocharVal = static_cast<char*>(marshallToCharVal.ToPointer());
				int toReturn = zsetFile(ifltabPinned, ptrToParameter, ptrTocharVal, integerValue);
				Marshal::FreeHGlobal(marshallToParameter);
				Marshal::FreeHGlobal(marshallToCharVal);
				return toReturn;
			}

			int DSS::ZQuery(String ^ parameter, String ^ charVal, size_t lenCharVal, array<int> ^% integerValue)
			{
				pin_ptr<int> integerValuePinned = &integerValue[0];
				IntPtr marshallToParameter = Marshal::StringToHGlobalAnsi(parameter);
				IntPtr marshallToCharVal = Marshal::StringToHGlobalAnsi(charVal);
				char * ptrToParameter = static_cast<char*>(marshallToParameter.ToPointer());
				char * ptrTocharVal = static_cast<char*>(marshallToCharVal.ToPointer());
				int toReturn = zquery(ptrToParameter, ptrTocharVal, lenCharVal, integerValuePinned);
				Marshal::FreeHGlobal(marshallToParameter);
				Marshal::FreeHGlobal(marshallToCharVal);
				return toReturn;
			}

			long long DSS::ZInquire(array<long long> ^% ifltab, String ^ request)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(request);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				long long toReturn = zinquire(ifltabPinned, strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZInquireChar(array<long long> ^% ifltab, String ^ request, String ^ creturn, size_t creturnSize, array<int> ^% number)
			{
				(creturn, creturnSize);
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				pin_ptr<int> numberPinned = &number[0];
				IntPtr marshallToRequest = Marshal::StringToHGlobalAnsi(request);
				IntPtr marshallToCReturn = Marshal::StringToHGlobalAnsi(creturn);
				char * ptrToRequest = static_cast<char*>(marshallToRequest.ToPointer());
				char * ptrToCReturn = static_cast<char*>(marshallToCReturn.ToPointer());
				int toReturn = zinquireChar(ifltabPinned, ptrToRequest, ptrToCReturn, creturnSize, numberPinned);
				Marshal::FreeHGlobal(marshallToRequest);
				Marshal::FreeHGlobal(marshallToCReturn);
				return toReturn;
			}

			int DSS::ZFileName(String ^% fullDssFilename, size_t sizeofFilename, String ^ dssFileName, array<int> ^% permission)
			{
				pin_ptr<int> permissionPinned = &permission[0];
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(fullDssFilename);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(dssFileName);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				int toReturn = zfileName(ptrToFirst, sizeofFilename, ptrToSecond, permissionPinned);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
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

			String^ DSS::ZTypeName(array<long long> ^% ifltab, String ^ pathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathname);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zdataType(ifltabPinned, strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);

				String^ rval = gcnew String(ztypeName(toReturn, 1));
				return rval;
			}



			int DSS::ZGetRecordSize(array<long long> ^%  ifltab, ZStructRecordSizeWrapper ^ recordSize)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zgetRecordSize(ifltabPinned, recordSize->theStruct);
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

			//doesnt work
			/* long long DSS::ZGetLastWriteTime(array<long long> ^% ifltab, String ^ pathname)
			{
			pin_ptr<long long> ifltabPinned = &ifltab[0];
			IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathname);
			char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
			long long toReturn = zgetLastWriteTime(ifltabPinned, strPtr);
			Marshal::FreeHGlobal(marshallToCharStar);
			return toReturn;
			}*/

			long long DSS::ZGetLastWriteTimeFile(array<long long> ^% ifltab)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zgetLastWriteTimeFile(ifltabPinned);
			}

			unsigned int DSS::zGetDataCRC(array<long long> ^% ifltab, String ^ pathname, unsigned int crcln)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathname);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				unsigned int toReturn = zgetDataCRC(ifltabPinned, strPtr, crcln);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZWhatChangedSetStart(array<long long> ^% ifltab, ZStructCatalogWrapper ^ catStruct, String ^ pathWithWildChars, int boolUseCRC)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathWithWildChars);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zwhatChangedSetStart(ifltabPinned, catStruct->theStruct, strPtr, boolUseCRC);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZWhatChanged(array<long long> ^% ifltab, ZStructCatalogWrapper ^ catStruct)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zwhatChanged(ifltabPinned, catStruct->theStruct);
			}

			int DSS::ZWhatChangedCompare(array<long long> ^% ifltab, ZStructCatalogWrapper ^% catStructBefore, ZStructCatalogWrapper ^% catStructChanged, String ^ pathWithWild, int boolUseCRC)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathWithWild);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zwhatChangedCompare(ifltabPinned, catStructBefore->theStruct, catStructChanged->theStruct, strPtr, boolUseCRC);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZPathNameClean(String ^% newPathname, size_t sizeOfNewPathName, String ^ oldPathname)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(newPathname);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(oldPathname);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				int toReturn = zpathnameClean(ptrToFirst, sizeOfNewPathName, ptrToSecond);
				newPathname = gcnew String(ptrToFirst);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
				return toReturn;
			}

			int DSS::ZPathNameCompare(String ^ pathname1, array<long long> ^% pathname2, size_t pathnameLength)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(pathname1);
				pin_ptr<long long> pathname2Pinned = &pathname2[0];
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				int toReturn = zpathnameCompare(ptrToFirst, pathname2Pinned, pathnameLength);
				Marshal::FreeHGlobal(marshallToCharStar1);
				return toReturn;
			}

			int DSS::ZPathNameCompareCollection(String ^ pathname1, String ^ pathname2, size_t pathnameLength)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(pathname1);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(pathname2);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				int toReturn = zpathnameCompareCollection(ptrToFirst, ptrToSecond, pathnameLength);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
				return toReturn;
			}

			int DSS::ZPathNameGetPart(String ^ pathname, int partPosition, String ^% part, size_t sizeOfPart)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(pathname);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(part);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				int toReturn = zpathnameGetPart(ptrToFirst, partPosition, ptrToSecond, sizeOfPart);
				part = gcnew String(ptrToSecond);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
				return toReturn;
			}

			int DSS::ZPathNameSetPart(String ^ pathname, size_t sizeOfPathname, String ^ part, int partPosition)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(pathname);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(part);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				int toReturn = zpathnameSetPart(ptrToFirst, sizeOfPathname, ptrToSecond, partPosition);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
				return toReturn;
			}

			int DSS::ZPathNamePartLengths(String ^ pathname, size_t pathnameLen, array<int> ^% lengths, int dimOfLengths)
			{
				pin_ptr<int> lengthPinned = &lengths[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathname);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zpathnamePartLengths(strPtr, pathnameLen, lengthPinned, dimOfLengths);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZPathNamePartPositions(String ^ pathname, size_t pathnameLen, array<int> ^% positions, int dimOfPositions)
			{
				pin_ptr<int> positionPinned = &positions[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathname);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zpathnamePartPositions(strPtr, pathnameLen, positionPinned, dimOfPositions);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZPathNameForm(String ^ aPart, String ^ bPart, String ^ cPart, String ^ dPart, String ^ ePart, String ^ fPart, String ^% pathname, size_t sizeOfPathname)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(pathname);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(aPart);
				IntPtr marshallToCharStar3 = Marshal::StringToHGlobalAnsi(bPart);
				IntPtr marshallToCharStar4 = Marshal::StringToHGlobalAnsi(cPart);
				IntPtr marshallToCharStar5 = Marshal::StringToHGlobalAnsi(dPart);
				IntPtr marshallToCharStar6 = Marshal::StringToHGlobalAnsi(ePart);
				IntPtr marshallToCharStar7 = Marshal::StringToHGlobalAnsi(fPart);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				char * ptrToThird = static_cast<char*>(marshallToCharStar3.ToPointer());
				char * ptrToFourth = static_cast<char*>(marshallToCharStar4.ToPointer());
				char * ptrToFifth = static_cast<char*>(marshallToCharStar5.ToPointer());
				char * ptrToSixth = static_cast<char*>(marshallToCharStar6.ToPointer());
				char * ptrToSeventh = static_cast<char*>(marshallToCharStar7.ToPointer());
				int toReturn = zpathnameForm(ptrToSecond, ptrToThird, ptrToFourth, ptrToFifth, ptrToSixth, ptrToSeventh, ptrToFirst, sizeOfPathname);
				pathname = gcnew String(ptrToFirst);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
				Marshal::FreeHGlobal(marshallToCharStar3);
				Marshal::FreeHGlobal(marshallToCharStar4);
				Marshal::FreeHGlobal(marshallToCharStar5);
				Marshal::FreeHGlobal(marshallToCharStar6);
				Marshal::FreeHGlobal(marshallToCharStar7);
				return toReturn;
			}

			void DSS::ZMaxPart(array<long long> ^% ifltab, array<int> ^% maxParts)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				pin_ptr<int> maxPartsPinned = &maxParts[0];
				zmaxPart(ifltabPinned, maxPartsPinned);
			}

			void DSS::ZMaxPart7(array<long long> ^% ifltab, array<int> ^% maxParts)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				pin_ptr<int> maxPartsPinned = &maxParts[0];
				zmaxPart7(ifltabPinned, maxPartsPinned);
			}

			int DSS::ZTsGetEPartFromInterval(int intervalSeconds, String ^% ePart, size_t sizeOfEpart)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(ePart);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = ztsGetEPartFromInterval(intervalSeconds, strPtr, sizeOfEpart);
				ePart = gcnew String(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			float DSS::ZMissingFlagFloat()
			{
				return zmissingFlagFloat();
			}

			double DSS::ZMissingFlagDouble()
			{
				return zmissingFlagDouble();
			}

			int DSS::ZIsMissingDouble(double value)
			{
				return zisMissingDouble(value);
			}

			int DSS::ZIsMissing(Object ^ value, int lengthValue)
			{
				pin_ptr<void> valuePinned = &value;
				return zisMissing(valuePinned, lengthValue);
			}

			void DSS::ZSetMissing(int value, int lengthValue)
			{
				pin_ptr<int> valuePinned = &value;
				zsetMissing(valuePinned, lengthValue);
			}

			void DSS::ZSetMissingFloatArray(array<float> ^ values, int numberValues)
			{
				pin_ptr<float> valuePinned = &values[0];
				zsetMissingFloatArray(valuePinned, numberValues);
			}

			void DSS::ZSetMissingDoubleArray(array<double> ^ values, int numberValues)
			{
				pin_ptr<double> valuePinned = &values[0];
				zsetMissingDoubleArray(valuePinned, numberValues);
			}

			void DSS::ZSetUndefined(array<int> ^ data, int dataLength)
			{
				pin_ptr<int> dataPinned = &data[0];
				zsetUndefined(dataPinned, dataLength);
			}

			void DSS::ZSetMessageGroupLevel(String ^ functionGroup, int level)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(functionGroup);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				zsetMessageGroupLevel(strPtr, level);
				Marshal::FreeHGlobal(marshallToCharStar);
			}

			int DSS::ZGetMessageLevel(int group)
			{
				return zgetMessageLevel(group);
			}

			void DSS::ZResetMessageLevel()
			{
				zresetMessageLevel();
			}

			void DSS::ZSetMessageLevelFile(array<long long> ^% ifltab, int level)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				zsetMessageLevelFile(ifltabPinned, level);
			}

			int DSS::ZMessageAvaliable(array<long long> ^% ifltab)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zmessageAvailable(ifltabPinned);
			}

			String ^ DSS::ZGetMessage(array<long long> ^% ifltab)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				const char * returned = zgetMessage(ifltabPinned);
				return gcnew String(returned);
			}

			String ^ DSS::ZGetMessageAll(array<long long> ^% ifltab)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				const char * returned = zgetMessageAll(ifltabPinned);
				return gcnew String(returned);
			}

			void DSS::ZClearMessage(array<long long> ^% ifltab)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				zclearMessage(ifltabPinned);
			}

			void DSS::ZClearMessageAll(array<long long> ^% ifltab)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				zclearMessageAll(ifltabPinned);
			}

			int DSS::ZErrorCheck()
			{
				return zerrorCheck();
			}

			int DSS::ZCheckFile(array<long long> ^% ifltab)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zcheckFile(ifltabPinned);
			}

			int DSS::ZAliasRemove(array<long long> ^% ifltab, String ^ aliasPathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(aliasPathname);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zaliasRemove(ifltabPinned, strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZAliasRemoveAll(array<long long> ^% ifltab, String ^ aliasPathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(aliasPathname);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zaliasRemoveAll(ifltabPinned, strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZAliasGetPrimary(array<long long> ^% ifltab, String ^ aliasPathname, String ^% primaryPathname, size_t maxLenPrimaryPathname)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(aliasPathname);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(primaryPathname);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				int toReturn = zaliasGetPrimary(ifltabPinned, ptrToFirst, ptrToSecond, maxLenPrimaryPathname);
				primaryPathname = gcnew String(ptrToSecond);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
				return toReturn;
			}

			int DSS::ZAliasList(array<long long> ^% ifltab, String ^ pathname, array<String ^> ^ pathnameList, int % pathnameListLength)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				pin_ptr<int> pathnameListLengthPinned = &pathnameListLength;
				int size = pathnameListLength;
				char ** pathNameListCharPtr = new char *[size];
				array<IntPtr> ^ marshallArray = gcnew array<IntPtr>(size);
				for (int i = 0; i < size; i++)
				{
					IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathnameList[i]);
					char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
					marshallArray[i] = marshallToCharStar;
					pathNameListCharPtr[i] = strPtr;
				}
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathname);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zaliasList(ifltabPinned, strPtr, pathNameListCharPtr, pathnameListLengthPinned);
				Marshal::FreeHGlobal(marshallToCharStar);
				for (int i = 0; i < size; i++)
				{
					Marshal::FreeHGlobal(marshallArray[i]);
				}
				delete pathNameListCharPtr;
				return 0;
			}

			int DSS::FortranOpen(int % unit, String ^% filename, size_t lenFilename)
			{
				pin_ptr<int> unitPinned = &unit;
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(filename);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = fortranopen_(unitPinned, strPtr, lenFilename);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::FortranClose(int % unit)
			{
				pin_ptr<int> unitPinned = &unit;
				return fortranclose_(unitPinned);
			}

			int DSS::IsUnitConnected(int % unit)
			{
				pin_ptr<int> unitPinned = &unit;
				return isunitconnected_(unitPinned);
			}

			int DSS::ZCKMUL6(array<long long> ^% ifltab)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zckmul6_(ifltabPinned);
			}

			void DSS::ZPseudorts6(String ^ CFROMPATH, String ^% CTOPATH, int % INTL, int % IACTION, int % ISTATUS, size_t lenFrom, size_t lenTo)
			{
				pin_ptr<int> IACTIONPinned = &IACTION;
				pin_ptr<int> INTLPinned = &INTL;
				pin_ptr<int> ISTATUSPinned = &ISTATUS;
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(CFROMPATH);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(CTOPATH);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				zpseudorts6_(ptrToFirst, ptrToSecond, INTLPinned, IACTIONPinned, ISTATUSPinned, lenFrom, lenTo);
				CTOPATH = gcnew String(ptrToSecond);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
			}

			//doesn't work
			/* String ^ DSS::ZStatus(int % errorCode, int % severity)
			{
			pin_ptr<int> errorCodePinned = &errorCode;
			pin_ptr<int> severityPinned = &severity;
			char * returned = zstatus(errorCodePinned, severityPinned);
			return gcnew String(returned);
			}*/

			int DSS::ZPdStore6(array<long long> ^% ifltab, ZStructPairedDataWrapper ^% pds, int storageFlag)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zpdStore6(ifltabPinned, pds->theStruct, storageFlag);
			}

			int DSS::ZPdRetrieve6(array<long long> ^% ifltab, ZStructPairedDataWrapper ^% pds, int retrieveFlag)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zpdRetrieve6(ifltabPinned, pds->theStruct, retrieveFlag);
			}

			int DSS::ZOpen6(array<long long> ^% ifltab, String ^ dssFilename)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dssFilename);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zopen6(ifltabPinned, strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZOpen7(array<long long> ^% ifltab, String ^ dssFilename)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dssFilename);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zopen7(ifltabPinned, strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZSqueeze(String ^ dssFilename)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dssFilename);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = zsqueeze(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::ZCopyFile(array<long long> ^% ifltab, array<long long> ^% ifltabTo, int statusWanted)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				pin_ptr<long long> ifltabToPinned = &ifltabTo[0];
				return zcopyFile(ifltabPinned, ifltabToPinned, statusWanted);
			}

			int DSS::ZConvertVersion(String ^ fileNameFrom, String ^ fileNameTo)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(fileNameFrom);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(fileNameTo);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				int toReturn = zconvertVersion(ptrToFirst, ptrToSecond);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
				return toReturn;
			}

			void DSS::ZFname(String ^ dssFilenameIn, String ^% dssFilenameOut, int % nname, int % exists, size_t lenDssFilenameIn, size_t sizeDssFilenameOut)
			{
				pin_ptr<int> nnamePinned = &nname;
				pin_ptr<int> existsPinned = &exists;
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(dssFilenameIn);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(dssFilenameOut);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				zfname(ptrToFirst, ptrToSecond, nnamePinned, existsPinned, lenDssFilenameIn, sizeDssFilenameOut);
				dssFilenameOut = gcnew String(ptrToSecond);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
			}

			int DSS::SortFiles(String ^% unsortedIn, String ^% sortedOut)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(unsortedIn);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(sortedOut);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				int toReturn = sortfiles(ptrToFirst, ptrToSecond);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
				return toReturn;
			}

			void DSS::ZReadx(array<long long>^% ifltab, String ^ pathname, array<int>^% internalHeader, int % internalHeaderArraySize, int % internalHeaderNumber, array<int>^% header2,
				int % header2ArraySize, int % header2Number, array<int>^% userHeader, int % userHeaderArraySize, int % userHeaderNumber, array<int>^% values,
				int % valuesSize, int % valuesNumber, int % readPlan, int % recordFound, size_t pathlen)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				pin_ptr<int> internalHeaderPinned = &internalHeader[0];
				pin_ptr<int> internalHeaderArraySizePinned = &internalHeaderArraySize;
				pin_ptr<int> internalHeaderNumberPinned = &internalHeaderNumber;
				pin_ptr<int> header2Pinned = &header2[0];
				pin_ptr<int> header2ArraySizePinned = &header2ArraySize;
				pin_ptr<int> header2NumberPinned = &header2Number;
				pin_ptr<int> userHeaderPinned = &userHeader[0];
				pin_ptr<int> userHeaderArraySizePinned = &userHeaderArraySize;
				pin_ptr<int> userHeaderNumberPinned = &userHeaderNumber;
				pin_ptr<int> valuesPinned = &values[0];
				pin_ptr<int> valueSizePinned = &valuesSize;
				pin_ptr<int> valuesNumberPinned = &valuesNumber;
				pin_ptr<int> readPlanPinned = &readPlan;
				pin_ptr<int> recordFoundPinned = &recordFound;
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathname);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				zreadx(ifltabPinned, strPtr, internalHeaderPinned, internalHeaderArraySizePinned, internalHeaderNumberPinned, header2Pinned, header2ArraySizePinned, header2NumberPinned, userHeaderPinned, userHeaderArraySizePinned, userHeaderNumberPinned, valuesPinned, valueSizePinned, valuesNumberPinned, readPlanPinned, recordFoundPinned);
				Marshal::FreeHGlobal(marshallToCharStar);
			}

			ZStructSpatialGridWrapper ^ DSS::ZStructSpatialGridNew(String ^ filename)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(filename);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				zStructSpatialGrid * newGrid = zstructSpatialGridNew(ptrToFirst);
				ZStructSpatialGridWrapper ^ ToReturn = gcnew ZStructSpatialGridWrapper(newGrid);
				Marshal::FreeHGlobal(marshallToCharStar1);
				return ToReturn;
			}

			String ^ DSS::AlbersSRS() {
				String ^ rval = gcnew String( SHG_SRC_DEFINITION);
				return rval;
			}

			int DSS::ZSpatialGridStore(array<long long>^% ifltab, ZStructSpatialGridWrapper ^% gs)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zspatialGridStore(ifltabPinned, gs->theStruct);
			}

			int DSS::ZSpatialGridRetrieve(array<long long>^% ifltab, ZStructSpatialGridWrapper ^% gs, bool retrieveData)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				int status = DSSGrid::RetrieveGriddedData(ifltabPinned, gs->theStruct, retrieveData ? 1 : 0);
				return status; 
			}

			/*int DSS::ZSpatialGridRetrieveVersion(array<long long> ^% ifltab, String ^ cpath, int % gridStructVersion)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(cpath);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				pin_ptr<int> gridStructVersionPinned = &gridStructVersion;
				int toReturn = zspatialGridRetrieveVersion(ifltabPinned, strPtr, gridStructVersionPinned);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}*/

			void DSS::PrintGridStruct(array<long long> ^% ifltab, int function_id, ZStructSpatialGridWrapper ^% gs)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				printGridStruct(ifltabPinned, function_id, gs->theStruct);
			}

			void DSS::Zdtype_(array<long long>^% ifltab, String ^ pathname, int % numDataCompressed, int % boolExists, array<char>^% charRecordType, int % recordType, size_t pathLength, size_t charRecordTypeLength)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(pathname);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				pin_ptr<int> numDataCompressedPinned = &numDataCompressed;
				pin_ptr<int> boolExistsPinned = &boolExists;
				pin_ptr<char> charRecordTypePinned = &charRecordType[0];
				pin_ptr<int> recordTypePinned = &recordType;
				zdtype_(ifltabPinned, strPtr, numDataCompressedPinned, boolExistsPinned, charRecordTypePinned, recordTypePinned, pathLength, charRecordTypeLength);
				Marshal::FreeHGlobal(marshallToCharStar);
			}

			bool DSS::IsTimeDefined(int julianDate, int timeSeconds)
			{
				return isTimeDefined(julianDate, timeSeconds) == 1;
			}

			int DSS::ZTsGetSizes6(array<long long> ^% ifltab, ZStructTimeSeriesWrapper ^% tss, ZStructRecordSizeWrapper ^% timeSeriesRecordSizes)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return ztsGetSizes6(ifltabPinned, tss->theStruct, timeSeriesRecordSizes->theStruct);
			}

			int DSS::ZGetRecordSize6(array<long long> ^% ifltab, ZStructRecordSizeWrapper ^% recordSize)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zgetRecordSize6(ifltabPinned, recordSize->theStruct);
			}

			int DSS::ZGetRecordSize7(array<long long> ^% ifltab, ZStructRecordSizeWrapper ^% recordSize)
			{
				pin_ptr<long long> ifltabPinned = &ifltab[0];
				return zgetRecordSize7(ifltabPinned, recordSize->theStruct);
			}

			long long DSS::GetCurrentTimeMillis()
			{
				return getCurrentTimeMillis();
			}

			void DSS::GetCurrentDateTime(int % julian, int % secondsPastMidnight, int % millsPastSecond)
			{
				pin_ptr<int> julianPinned = &julian;
				pin_ptr<int> secondsPastMidnightPinned = &secondsPastMidnight;
				pin_ptr<int> millsPastSecondPinned = &millsPastSecond;
				return getCurrentDateTime(julianPinned, secondsPastMidnightPinned, millsPastSecondPinned);
			}

			void DSS::GetCurrentTimeString(String ^% timeString, size_t lenTimeString)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(timeString);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				getCurrentTimeString(strPtr, lenTimeString);
				timeString = gcnew String(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
			}

			void DSS::GetCurrentDateString(String ^% dateString, size_t sizeOfDateString)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dateString);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				getCurrentTimeString(strPtr, sizeOfDateString);
				dateString = gcnew String(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
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

			void DSS::GetDateTimeString(int julian, String ^% dateString, size_t sizeOfDateString, int dateStyle, int secondsPastMidnight, String ^% timeString, size_t sizeofTimeString, int timeStyle)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(dateString);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(timeString);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				getDateTimeString(julian, ptrToFirst, sizeOfDateString, dateStyle, secondsPastMidnight, ptrToSecond, sizeofTimeString, timeStyle);
				dateString = gcnew String(ptrToFirst);
				timeString = gcnew String(ptrToSecond);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
			}

			/*int DSS::JulianToDate(int julianDate, int style, String ^% dateString, size_t sizeOfDateString)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(dateString);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = julianToDate(julianDate, style, strPtr, sizeOfDateString);
				dateString = gcnew String(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}
*/
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

			void DSS::MinsToDateTime(int minsSince1900, String ^% dateString, String ^% timeString, size_t sizeOfDateString, size_t sizeOfTimeString)
			{
				IntPtr marshallToCharStar1 = Marshal::StringToHGlobalAnsi(dateString);
				IntPtr marshallToCharStar2 = Marshal::StringToHGlobalAnsi(timeString);
				char * ptrToFirst = static_cast<char*>(marshallToCharStar1.ToPointer());
				char * ptrToSecond = static_cast<char*>(marshallToCharStar2.ToPointer());
				minsToDateTime(minsSince1900, ptrToFirst, ptrToSecond, sizeOfDateString, sizeOfTimeString);
				dateString = gcnew String(ptrToFirst);
				timeString = gcnew String(ptrToSecond);
				Marshal::FreeHGlobal(marshallToCharStar1);
				Marshal::FreeHGlobal(marshallToCharStar2);
			}

			void DSS::MinutesToHourMin(int minutes, String ^% hoursMins, size_t lenHoursMins)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(hoursMins);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				minutesToHourMin(minutes, strPtr, lenHoursMins);
				hoursMins = gcnew String(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
			}

			void DSS::SecondsToTimeString(int secondsPastMidnight, int millsPastSecond, int timeStyle, String ^% timeString, size_t sizeofTimeString)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(timeString);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				secondsToTimeString(secondsPastMidnight, millsPastSecond, timeStyle, strPtr, sizeofTimeString);
				timeString = gcnew String(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
			}

			int DSS::TimeStringToSeconds(String ^ timeString)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(timeString);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				int toReturn = timeStringToSeconds(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			float DSS::TimeStringToSecondsMills(String ^ timeString)
			{
				IntPtr marshallToCharStar = Marshal::StringToHGlobalAnsi(timeString);
				char * strPtr = static_cast<char*>(marshallToCharStar.ToPointer());
				float toReturn = timeStringToSecondsMills(strPtr);
				Marshal::FreeHGlobal(marshallToCharStar);
				return toReturn;
			}

			int DSS::DayOfWeek(int julian)
			{
				return dayOfWeek(julian);
			}

			int DSS::IncrementTime(int intervalSeconds, int numberPeriods, int julianStart, int secondsStart, int % julianEnd, int % secondsEnd)
			{
				pin_ptr<int> julianEndPinned = &julianEnd;
				pin_ptr<int> secondsEndPinned = &secondsEnd;
				return incrementTime(intervalSeconds, numberPeriods, julianStart, secondsStart, julianEndPinned, secondsEndPinned);
			}

			int DSS::NumberPeriods(int intervalSeconds, int julianStart, int secondsStart, int julianEnd, int secondsEnd)
			{
				return numberPeriods(intervalSeconds, julianStart, secondsStart, julianEnd, secondsEnd);
			}

			int DSS::AddCentury(int year)
			{
				return addCentury(year);
			}

			int DSS::IsLeapYear(int year)
			{
				return isLeapYear(year);
			}

			int DSS::CleanTime(int % julianDate, int % timeMinSec, int timeGranularitySeconds)
			{
				pin_ptr<int> julianDatePinned = &julianDate;
				pin_ptr<int> timeMinSecPinned = &timeMinSec;
				return cleanTime(julianDatePinned, timeMinSecPinned, timeGranularitySeconds);
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


			void DSS::PrintCurrentTime(int lineFeed)
			{
				printCurrentTime(lineFeed);
			}

			int DSS::CompareTimes(int julianFirst, int secondsFirst, int timeGranularitySecondsFirst, int julainBaseFirst, int julianSecond, int secondsSecond, int julianBaseSecond, int timeGranularitySecondsSecond)
			{
				return compareTimes(julianFirst, secondsFirst, julainBaseFirst, timeGranularitySecondsFirst, julianSecond, secondsSecond, julianBaseSecond, timeGranularitySecondsSecond);
			}
		}
	}
}