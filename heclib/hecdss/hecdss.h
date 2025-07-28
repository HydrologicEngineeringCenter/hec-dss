/*
 This project (hecdss) contains code for a shared object/dll, providing an API to work with DSS files.

 Only DSS version 7 files are supported.  DSS version 6 files should be converted to DSS version 7
 using HEC-DSSVue https://www.hec.usace.army.mil/software/hec-dssvue/.  If you don't have any gridded data 
 is is ok to convert from DSS version 6 to DSS version 7 using the method hec_dss_convertToVersion7(...) in this libraray.


 This API is designed with perspective that the calling/client code is in charge of managing memory.
 The only exception is hec_dss_open(const char* filename, dss_file** dss).   hec_dss_open allocates
 one internal structure that must be freed by calling hec_dss_close;

 For reading data: The client passes in pre-allocated arrays, with the size, then the API copies data
 into those arrays

*/

#if defined(_MSC_VER)
#ifdef HECDSS_EXPORTS
#define HECDSS_API __declspec(dllexport)
#else
#define HECDSS_API __declspec(dllimport)
#endif
#else
#define HECDSS_API __attribute__((visibility("default")))
#endif

// public declaration
typedef struct dss_file dss_file;

/// <summary>
/// Returns a version string of this API
/// </summary>
/// <returns></returns>
HECDSS_API const char* hec_dss_api_version();

HECDSS_API int hec_dss_CONSTANT_MAX_PATH_SIZE();

/// <summary>
/// use hec_dss_open to connect to a DSS file.
/// </summary>
/// <param name="filename">input: filename to open or create</param>
/// <param name="dss">output: pointer to dss_file</param>
/// <returns>zero if successful</returns>
HECDSS_API int hec_dss_open(const char* filename, dss_file** dss);


/// <summary>
/// use hec_dss_close to close the connection to a DSS file.
/// This must be called when a program is done using a DSS file.
/// </summary>
/// <param name="dss"></param>
/// <returns>zero if successful</returns>
HECDSS_API int hec_dss_close(dss_file* dss);

/// <summary>
/// Gets version of DSS file that has already been opened.
/// </summary>
/// <param name="dss"></param>
/// <returns></returns>
HECDSS_API int hec_dss_getVersion(dss_file* dss);

/// <summary>
/// Gets the version of a DSS file based on filename
/// Returns:
///    7:  A DSS version 7 file
///    6:  A DSS version 6 file
///    0 : File does not exist
///   -1 : Not a DSS file(but file exists)
///   -2 : Invalid file name
///   -3 : Open error(undefined)
/// < -3 : abs(error) is system open or read error
/// </summary>
/// <param name="filename"></param>
/// <returns></returns>
HECDSS_API int hec_dss_getFileVersion(const char* filename);

/// <summary>
/// Sets internal number values inside DSS
/// for example calling hec_dss_set_value("mlvl",10) sets the DSS output level to 10
/// </summary>
/// <returns>zero on sucess</returns>
HECDSS_API int hec_dss_set_value(const char* name, const int value);

/// <summary>
/// Sets an internal character value inside DSS
/// for example calling hec_dss_set_string("PROG","DSSVue") sets the program name 
/// </summary>
/// <returns>zero on sucess</returns>
HECDSS_API int hec_dss_set_string(const char* name, const char* value);




/// <summary>
/// Used to find the range of DateTime
/// </summary>
/// <param name="dss">pointer to DSS file</param>
/// <param name="pathname">DSS pathname</param>
/// <param name="boolFullSet">set to 1 to return the dates of the entire dataset (multiple records)</param>
/// <param name="firstValidJulian">output: Returns the Julian date of the first valid value</param>
/// <param name="firstSeconds">output: Returns the time of day in seconds (for firstJulian) of the first valid value</param>
/// <param name="lastValidJulian">output: Returns the Julian date of the last valid value</param>
/// <param name="lastSeconds">output: Returns the time of day in seconds of the last valid value</param>
/// <returns></returns>
HECDSS_API int  hec_dss_tsGetDateTimeRange(dss_file* dss, const char* pathname, const int boolFullSet,
  int* firstValidJulian, int* firstSeconds,
  int* lastValidJulian, int* lastSeconds);


/// <summary>
/// returns the number of periods between two dates/times for a given interval
/// </summary>
/// <param name="intervalSeconds">interval in seconds , for example 3600 would be used for 1Hour data</param>
/// <param name="julianStart">starting julian date</param>
/// <param name="startSeconds">staring seconds</param>
/// <param name="julianEnd">ending julian date</param>
/// <param name="endSeconds">ending seconds</param>
/// <returns></returns>
HECDSS_API int  hec_dss_numberPeriods(const int intervalSeconds, const int julianStart, const int startSeconds,
    const int julianEnd, const int endSeconds);


/// <summary>
/// Gets size information about a time series record or data set (series of records)
/// </summary>
/// <param name="dss">pointer to DSS file</param>
/// <param name="pathname">DSS pathname</param>
/// <param name="startDate">start date </param>
/// <param name="startTime">start time</param>
/// <param name="endDate">end date</param>
/// <param name="endTime">end time</param>
/// <param name="numberValues">output:number of values available </param>
/// <param name="qualityElementSize">width of quality available for each value</param>
/// <returns></returns>
HECDSS_API int hec_dss_tsGetSizes(dss_file* dss, const char* pathname,
  const char* startDate, const char* startTime,
  const char* endDate, const char* endTime,
  int* numberValues, int* qualityElementSize);


/// <summary>
/// Returns basic time series information for a dss pathname
/// </summary>
/// <param name="dss">pointer to DSS file</param>
/// <param name="pathname">DSS pathname</param>
/// <param name="units">output: units</param>
/// <param name="unitsLength">size of units buffer</param>
/// <param name="type">output: type of data: PER-AVER, PER-CUM,INST-VAL,INST-CUM https://www.hec.usace.army.mil/confluence/dssvuedocs/latest/introduction/time-series-conventions</param>
/// <param name="typeLength">size of type buffer</param>
/// <returns></returns>
HECDSS_API int hec_dss_tsRetrieveInfo(dss_file* dss, const char* pathname, char* units,
  const int unitsLength, char* type, const int typeLength);


/// <summary>
/// Returns number of records (includes aliases)
/// </summary>
/// <param name="dss"></param>
/// <returns>number of records</returns>
HECDSS_API int hec_dss_record_count(dss_file* dss);

/// <summary>
/// Used to read the catalog of a DSS file
/// </summary>
/// <param name="dss">input:pointer to DSS file</param>
/// <param name="pathBuffer">allocated buffer that is loaded with pathnames </param>
/// <param name="recordTypes">output array of record types corresponding to each path</param>
/// <param name="pathFilter">	Either null (for ignore) or a String that represents a pathname with wild characters represented
///  by a star(*) to match any string in the pathname part.Wild characters can only be at the beginning or end of a part,
///  not inside of a string.An example is a C part with "*Flow*", which
///  will match all pathnames that have "Flow" anywhere in the C part, such as "Flow", "Inflow", "Outflow-Reg", etc.
///  A part such as "Flow*Reg" is not supported. A null(//) will only match a null, where only a star (*) will match all. </param>
/// <param name="count">number of paths that can be stored in pathBuffer, and length of recordTypes array</param>
/// <param name="pathBufferItemSize">max allowable length of each pathname</param>
/// <returns>zero on success</returns>
HECDSS_API int hec_dss_catalog(dss_file* dss, char* pathBuffer, int* recordTypes, const char* pathFilter,
  const int count, const int pathBufferItemSize);




/// <summary>
/// Returns the DSS dataType as an integer
/// A return value of zero or negative indicates an error finding the return type
/// </summary>
/// <param name="pdss"></param>
/// <param name="pathname"></param>
/// <returns></returns>
HECDSS_API int hec_dss_dataType(dss_file* dss, const char* pathname);


/// <summary>
/// use hec_dss_tsRetrieve to read time series data
/// usually used in combination with first calling hec_dss_tsGetSizes(..)
/// to allocate the valueArray
/// </summary>
/// <param name="dss">input: pointer to dss file</param>
/// <param name="pathname">input: path to read from DSS file</param>
/// <param name="startDate">input: start date</param>
/// <param name="startTime">input: end time/param>
/// <param name="endDate">input: end date</param>
/// <param name="endTime">input: end time</param>
/// <param name="timeArray">input/output: array to hold integer representation of date-times</param>
/// <param name="valueArray">input/output: array to hold double representation of values</param>
/// <param name="arraySize">input: length of timeArray and valueArray </param>
/// <param name="numberValuesRead">output: number of values returned</param>
/// <param name="julianBaseDate">base integer </param>
/// <param name="timeGranularitySeconds"></param>
/// <param name="units">output: units</param>
/// <param name="unitsLength">size of units buffer</param>
/// <param name="type">output: type of data: PER-AVER, PER-CUM,INST-VAL,INST-CUM https://www.hec.usace.army.mil/confluence/dssvuedocs/latest/introduction/time-series-conventions</param>
/// <param name="typeLength">size of type buffer</param>
/// <param name="timeZoneName">output: time zone name (for the time-series), can be different from the location time-zone</param>
/// <param name="timeZoneNameLength">size of timeZoneName buffer</param>
/// <returns></returns>

HECDSS_API int hec_dss_tsRetrieve(dss_file* dss, const char* pathname,
  const char* startDate, const char* startTime,
  const char* endDate, const char* endTime,
  int* timeArray, double* valueArray, const int arraySize,
  int* numberValuesRead, int* quality, const int qualityWidth,
  int* julianBaseDate, int* timeGranularitySeconds,
  char* units, const int unitsLength,
  char* type, const int typeLength,
  char* timeZoneName, const int timeZoneNameLength);


/// <summary>
/// Stores regular time series to DSS
/// </summary>
/// <param name="dss">pointer to dss file</param>
/// <param name="pathname">path to read from DSS file</param>
/// <param name="startDate">input: start date</param>
/// <param name="startTime">input: end time/param>
/// <param name="valueArray">timeseries point values</param>
/// <param name="valueArraySize">number of points</param>
/// <param name="qualityArray">array of quality flags</param>
/// <param name="qualityArraySize">number of quality values per point</param>
/// <param name="saveAsFloat">set to true to save disk space</param>
/// <param name="units">units of data</param>
/// <param name="type">type of data: PER-AVER, PER-CUM,INST-VAL,INST-CUM https://www.hec.usace.army.mil/confluence/dssvuedocs/latest/introduction/time-series-conventions</param>
/// <param name="timeZoneName">time zone name (for the time-series), can be different from the location time-zone</param>
/// <param name="storageFlag">
/// A flag indicating how to handle existing data on disk:
/// 
/// For regular‐interval data:
///   0 – Always replace data.
///   1 – Only replace missing data.
///   2 – Write regardless, even if all data is missing (write a “missing” record).
///   3 – If the record is entirely missing, do not write it and delete any existing on‐disk copy.
///   4 – Do not allow missing input data to replace valid data.
/// </param>
/// <returns>
/// True on success, false on failure.  // ← adjust to your actual return semantics
/// </returns>
HECDSS_API int hec_dss_tsStoreRegular(dss_file* dss, const char* pathname,
  const char* startDate, const char* startTime,
  double* valueArray, const int valueArraySize,
  int* qualityArray, const int qualityArraySize,
  const int saveAsFloat,
  const char* units, const char* type, const char* timeZoneName, int storageFlag);

/// <summary>
/// Stores Irregular interval data to DSS
/// </summary>
/// <param name="dss">pointer to dss file</param>
/// <param name="pathname">pathname to store to</param>
/// <param name="startDateBase">starting base date,  defaults to 01Jan1990 if empty or null</param>
/// <param name="times">An integer array of minutes or seconds that correspond to the date/time for each value</param>
/// <param name="timeGranularitySeconds">The number of seconds a unit in times represents, usually MINUTE_GRANULARITY(60) or SECOND_GRANULARITY(1) </param>
/// <param name="valueArray">double array containing the data to store.</param>
/// <param name="valueArraySize">number of values in valueArrary</param>
/// <param name="qualityArray">The array that contains quality or other additional information.  A single quality value may be an int or multiple of an int.  This API only supports single int per quality.</param>
/// <param name="qualityArraySize">lenght of quality array, should match valueArraySize</param>
/// <param name="saveAsFloat">when true saves to disk, with float(4-bytes) otherwise uses 8-bytes per value.</param>
/// <param name="units">units such as 'cfs'</param>
/// <param name="type">type of data: PER-AVER, PER-CUM,INST-VAL,INST-CUM https://www.hec.usace.army.mil/confluence/dssvuedocs/latest/introduction/time-series-conventions</param>
/// <param name="storageFlag">
/// A flag indicating how to handle existing data on disk:
/// 
///For irregular‐interval data:
///   0 – Merge new data with old (for adding data).
///   1 – Replace old data with new (for editing/changing data).
/// </param>
/// <returns>status of zero on success</returns>
HECDSS_API int hec_dss_tsStoreIregular(dss_file* dss, const char* pathname,
  const char* startDateBase,
  int* times, const int timeGranularitySeconds,
  double* valueArray, const int valueArraySize,
  int* qualityArray, const int qualityArraySize,
  const int saveAsFloat,
  const char* units, const char* type, const char* timeZoneName, int storageFlag);

HECDSS_API int hec_dss_locationRetrieve(dss_file* dss, const char* fullPath,
  double* x, double* y, double* z,
  int* coordinateSystem, int* coordinateID,
  int* horizontalUnits, int* horizontalDatum,
  int* verticalUnits, int* verticalDatum,
  char* timeZoneName, const int timeZoneNameLength,
  char* supplemental, const int supplementalLength);

/// <summary>
/// saves location information 
/// </summary>
/// <param name="dss"></param>
/// <param name="fullPath"></param>
/// <param name="x"></param>
/// <param name="y"></param>
/// <param name="z"></param>
/// <param name="coordinateSystem"></param>
/// <param name="coordinateID"></param>
/// <param name="horizontalUnits"></param>
/// <param name="horizontalDatum"></param>
/// <param name="verticalUnits"></param>
/// <param name="verticalDatum"></param>
/// <param name="timeZoneName"></param>
/// <param name="supplemental"></param>
/// <param name="replace">set to 1 to overwrite existing data, otherwise use 0</param>
/// <returns></returns>
HECDSS_API int hec_dss_locationStore(dss_file* dss, const char* fullPath,
  const double x, const double y, const double z,
  const int coordinateSystem, const int coordinateID,
  const int horizontalUnits, const int horizontalDatum,
  const int verticalUnits, const int verticalDatum,
  const char* timeZoneName,
  const char* supplemental,
  const int replace);

/// <summary>
/// hec_dss_pdRetrieveInfo is used by client app to determine
/// required array sizes for calling hec_dss_pdRetrieve.
/// 
/// </summary>
/// <param name="dss">pointer to dss_file</param>
/// <param name="pathname">path of paired data</param>
/// <param name="numberOrdinates">number of rows for ordinates, and curves</param>
/// <param name="numberCurves">number of columns in the curve dataset</param>
/// <returns></returns>
HECDSS_API int hec_dss_pdRetrieveInfo(dss_file* dss, const char* pathname,
  int* numberOrdinates, int* numberCurves,
  char* unitsIndependent, const int unitsIndependentLength,
  char* unitsDependent, const int unitsDependentLength,
  char* typeIndependent, const int typeIndependentLength,
  char* typeDependent, const int typeDependentLength,
  int* labelsLength);

/// <summary>
/// Returns the record type.
/// RECORD_TYPE_100  "Regular-interval time series"
/// RECORD_TYPE_101  "Regular-interval time series pattern"
/// RECORD_TYPE_105  "Regular-interval time series doubles"
/// RECORD_TYPE_110  "Irregular-interval time series"
/// RECORD_TYPE_111  "Irregular-interval time series pattern"
/// RECORD_TYPE_115  "Irregular-interval time series doubles"
/// RECORD_TYPE_200  "Paired Data"
/// RECORD_TYPE_205  "Paired Data doubles"
/// RECORD_TYPE_300  "Text Data"
/// RECORD_TYPE_400  "Gridded - Undefined grid with time"
/// RECORD_TYPE_401  "Gridded - Undefined grid"
/// RECORD_TYPE_410  "Gridded - HRAP grid with time reference"
/// RECORD_TYPE_411  "Gridded - HRAP grid"
/// RECORD_TYPE_420  "Gridded - Albers with time reference"
/// RECORD_TYPE_421  "Gridded - Albers"
/// RECORD_TYPE_430  "Gridded - SHG with time reference"
/// RECORD_TYPE_431  "Gridded - SHG"
/// 
/// </summary>
/// <param name="dss">pointer to dss_file</param>
/// <param name="pathname">pathname to check record type</param>
/// <returns>integer record type</returns>
HECDSS_API int hec_dss_recordType(dss_file* dss, const char* pathname);


/// <summary>
/// Reads paired data from DSS
/// </summary>
/// <param name="dss"></param>
/// <param name="pathname"></param>
/// <param name="doubleOrdinates"></param>
/// <param name="doubleOrdinatesLength"></param>
/// <param name="doubleValues"></param>
/// <param name="doubleValuesLength"></param>
/// <param name="numberOrdinates"></param>
/// <param name="numberCurves"></param>
/// <param name="unitsIndependent"></param>
/// <param name="unitsIndependentLength"></param>
/// <param name="typeIndependent"></param>
/// <param name="typeIndependentLength"></param>
/// <param name="unitsDependent"></param>
/// <param name="unitsDependentLength"></param>
/// <param name="typeDependent"></param>
/// <param name="typeDependentLength"></param>
/// <param name="labels"> contains multiple labels separated with \0</param>
/// <param name="labelsLength">total length of labels</param>
/// <param name="timeZoneName">name of time-zone</param>
/// <returns></returns>
HECDSS_API int hec_dss_pdRetrieve(dss_file* dss, const char* pathname,
  double* doubleOrdinates, const int  doubleOrdinatesLength,
  double* doubleValues, const int doubleValuesLength,
  int* numberOrdinates, int* numberCurves,
  char* unitsIndependent, const int unitsIndependentLength,
  char* typeIndependent, const int typeIndependentLength,
  char* unitsDependent, const int unitsDependentLength,
  char* typeDependent, const int typeDependentLength,
  char* labels, const int labelsLength,
  char* timeZoneName, const int timeZoneNameLength);

/// <summary>
/// Stores Paired data to DSS
/// </summary>
/// <param name="dss"></param>
/// <param name="pathname"></param>
/// <param name="doubleOrdinates"></param>
/// <param name="doubleOrdinatesLength"></param>
/// <param name="doubleValues"></param>
/// <param name="doubleValuesLength"></param>
/// <param name="numberOrdinates"></param>
/// <param name="numberCurves"></param>
/// <param name="unitsIndependent"></param>
/// <param name="typeIndependent"></param>
/// <param name="unitsDependent"></param>
/// <param name="typeDependent"></param>
/// <param name="labels"></param>
/// <param name="labelsLength"></param>
/// <returns></returns>
HECDSS_API int hec_dss_pdStore(dss_file* dss, const char* pathname,
  double* doubleOrdinates, const int  doubleOrdinatesLength,
  double* doubleValues, const int doubleValuesLength,
  const int numberOrdinates, const int numberCurves,
  const char* unitsIndependent,
  const char* typeIndependent,
  const char* unitsDependent,
  const char* typeDependent,
  const char* labels, const int labelsLength,
  const char* timeZoneName);


/// <summary>
/// Reads gridded/spatial data from DSS
/// </summary>
/// <param name="dss"></param>
/// <param name="pathname"></param>
/// <param name="boolRetrieveData"></param>
/// <param name="type"></param>
/// <param name="dataType"></param>
/// <param name="lowerLeftCellX"></param>
/// <param name="lowerLeftCellY"></param>
/// <param name="numberOfCellsX"></param>
/// <param name="numberOfCellsY"></param>
/// <param name="numberOfRanges"></param>
/// <param name="srsDefinitionType"></param>
/// <param name="timeZoneRawOffset"></param>
/// <param name="isInterval"></param>
/// <param name="isTimeStamped"></param>
/// <param name="dataUnits"></param>
/// <param name="dataUnitsLength"></param>
/// <param name="dataSource"></param>
/// <param name="dataSourceLength"></param>
/// <param name="srsName"></param>
/// <param name="srsNameLength"></param>
/// <param name="srsDefinition"></param>
/// <param name="srsDefinitionLength"></param>
/// <param name="timeZoneID"></param>
/// <param name="timeZoneIDLength"></param>
/// <param name="cellSize"></param>
/// <param name="xCoordOfGridCellZero"></param>
/// <param name="yCoordOfGridCellZero"></param>
/// <param name="nullValue"></param>
/// <param name="maxDataValue"></param>
/// <param name="minDataValue"></param>
/// <param name="meanDataValue"></param>
/// <param name="rangeLimitTable"></param>
/// <param name="rangeTablesLength"></param>
/// <param name="numberEqualOrExceedingRangeLimit"></param>
/// <param name="data"></param>
/// <param name="dataLength"></param>
/// <returns></returns>
HECDSS_API int hec_dss_gridRetrieve(dss_file* dss, const char* pathname, int boolRetrieveData,
  int* type, int* dataType,
  int* lowerLeftCellX, int* lowerLeftCellY,
  int* numberOfCellsX, int* numberOfCellsY,
  int* numberOfRanges, int* srsDefinitionType,
  int* timeZoneRawOffset, int* isInterval,
  int* isTimeStamped,
  char* dataUnits, const int dataUnitsLength,
  char* dataSource, const int dataSourceLength,
  char* srsName, const int srsNameLength,
  char* srsDefinition, const int srsDefinitionLength,
  char* timeZoneID, const int timeZoneIDLength,
  float* cellSize, float* xCoordOfGridCellZero,
  float* yCoordOfGridCellZero, float* nullValue,
  float* maxDataValue, float* minDataValue,
  float* meanDataValue,
  float* rangeLimitTable, const int rangeTablesLength,
  int* numberEqualOrExceedingRangeLimit,
  float* data, const int dataLength);

/// <summary>
/// Stores gridded (spatial) data to DSS
/// </summary>
/// <param name="dss"></param>
/// <param name="pathname"></param>
/// <param name="gridType"></param>
/// <param name="dataType"></param>
/// <param name="lowerLeftCellX"></param>
/// <param name="lowerLeftCellY"></param>
/// <param name="numberOfCellsX"></param>
/// <param name="numberOfCellsY"></param>
/// <param name="numberOfRanges"></param>
/// <param name="srsDefinitionType"></param>
/// <param name="timeZoneRawOffset"></param>
/// <param name="isInterval"></param>
/// <param name="isTimeStamped"></param>
/// <param name="dataUnits"></param>
/// <param name="dataSource"></param>
/// <param name="srsName"></param>
/// <param name="srsDefinition"></param>
/// <param name="timeZoneID"></param>
/// <param name="cellSize"></param>
/// <param name="xCoordOfGridCellZero"></param>
/// <param name="yCoordOfGridCellZero"></param>
/// <param name="nullValue"></param>
/// <param name="maxDataValue"></param>
/// <param name="minDataValue"></param>
/// <param name="meanDataValue"></param>
/// <param name="rangeLimitTable"></param>
/// <param name="numberEqualOrExceedingRangeLimit"></param>
/// <param name="data"></param>
/// <returns></returns>
HECDSS_API int hec_dss_gridStore(dss_file* dss, const char* pathname,
  const int gridType, const int dataType,
  const int lowerLeftCellX, const int lowerLeftCellY,
  const int numberOfCellsX, const int numberOfCellsY,
  const int numberOfRanges, const int srsDefinitionType,
  const int timeZoneRawOffset, int isInterval,
  const int isTimeStamped,
  const char* dataUnits,
  const char* dataSource,
  const char* srsName,
  const char* srsDefinition,
  const char* timeZoneID,
  const float cellSize, const float xCoordOfGridCellZero,
  const float yCoordOfGridCellZero, const float nullValue,
  const float maxDataValue, const float minDataValue,
  const float meanDataValue,
  float* rangeLimitTable,
  int* numberEqualOrExceedingRangeLimit,
  float* data);

/// <summary>
/// converts character date to separate year,month, and day
/// </summary>
/// <param name="date">date string to parse.  case insensitive</param>
/// <param name="year">output: integer year</param>
/// <param name="month">output: integer month</param>
/// <param name="day">output: integer day</param>
/// <returns></returns>
HECDSS_API int hec_dss_dateToYearMonthDay(const char* date, int* year, int* month, int* day);

/// <summary>
/// Deletes data from DSS
/// </summary>
/// <param name="dss">dss_file pointer</param>
/// <param name="pathname">pathame for data to delete.</param>
/// <returns></returns>
HECDSS_API int hec_dss_delete(dss_file* dss, const char* pathname);

/// <summary>
/// squeeze (compress) a DSS file
/// </summary>
/// <param name="pathname"></param>
/// <returns></returns>
HECDSS_API int hec_dss_squeeze(const char* pathname);


/// <summary>
/// converts character date to integer representation
/// </summary>
/// <param name="date"></param>
/// <returns>julian date</returns>
HECDSS_API int hec_dss_dateToJulian(const char* date);

/// <summary>
/// Converts julian date to separate year, month, and day
/// </summary>
/// <param name="julian"></param>
/// <param name="year"></param>
/// <param name="month"></param>
/// <param name="day"></param>
/// <returns></returns>
HECDSS_API void hec_dss_julianToYearMonthDay(const int julian, int* year, int* month, int* day);

/// <summary>
/// Converts from DSS 6 to DSS7
/// Warning: DSS6 grids will not be converted. (Java libraries are necessary for version 6 grids)
/// </summary>
/// <param name="dssFilename"></param>
/// <param name="filenameVersion6"></param>
/// <returns></returns>
HECDSS_API int hec_dss_convertToVersion7(const char* filenameVersion6, const char* filenameVersion7);


HECDSS_API int hec_dss_arrayStore(dss_file* dss, const char* pathname,
  int* intValues, const int intValuesLength,
  float* floatValues, const int floatValuesLength,
  double* doubleValues, const int doubleValuesLength);


HECDSS_API int hec_dss_arrayRetrieveInfo(dss_file* dss, const char* pathname,
  int* intValuesRead, int* floatValuesRead, int* doubleValuesRead);

HECDSS_API int hec_dss_arrayRetrieve(dss_file* dss, const char* pathname,
  int* intValues, const int intValuesLength,
  float* floatValues, const int floatValuesLength,
  double* doubleValues, const int doubleValuesLength);

