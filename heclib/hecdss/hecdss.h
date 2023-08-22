// The following ifdef block is the standard way of creating macros which make exporting
// from a DLL simpler. All files within this DLL are compiled with the HECDSS_EXPORTS
// symbol defined on the command line. This symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see
// HECDSS_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
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
/// Use hec_dss_tsGetSizes to determine sizes (memory needs) before calling
/// hec_dss_tsRetrieve
/// </summary>
/// <param name="pdss">input: pointer to dss file</param>
/// <param name="pathname">input: path to read from DSS file</param>
/// <param name="startDate">input: start date</param>
/// <param name="startTime">input: end time/param>
/// <param name="endDate">input: end date</param>
/// <param name="endTime">input: end time</param>
/// <param name="numberValues">output: number of values found</param>
/// <param name="qualityElementSize">number of columns of quality data</param>
/// <returns>zero on success</returns>
HECDSS_API int hec_dss_tsGetSizes(dss_file* dss, const char* pathname,
  const char* startDate, const char* startTime,
  const char* endDate, const char* endTime,
  int* numberValues, int* qualityElementSize);

/// <summary>
/// Retrive an empty time series.
/// Used to get units and type for a time series
/// </summary>
/// <param name="pdss">input:pointer to dss file</param>
/// <param name="pathname">input: path to data</param>
/// <param name="units">output: units of this time series</param>
/// <param name="unitsLength">input: size of units buffer</param>
/// <param name="type">output: type of data: PER-AVER, PER-CUM,INST-VAL,INST-CUM https://www.hec.usace.army.mil/confluence/dssvuedocs/latest/introduction/time-series-conventions</param>
/// <param name="typeLength">input: size of units buffer</param>
/// <returns>zero on success</returns>
HECDSS_API int hec_dss_tsRetrieveInfo(dss_file* dss, const char* pathname, char* units,
  const int unitsLength, char* type, const int typeLength);


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
/// </summary>
/// <param name="pdss">input: pointer to dss file</param>
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
/// <param name="units"></param>
/// <param name="unitsLength"></param>
/// <param name="type"></param>
/// <param name="typeLength"></param>
/// <returns></returns>
HECDSS_API int hec_dss_tsRetrieve(dss_file* dss, const char* pathname,
  const char* startDate, const char* startTime,
  const char* endDate, const char* endTime,
  int* timeArray, double* valueArray, const int arraySize,
  int* numberValuesRead, int* quality, const int qualityWidth,
  int* julianBaseDate, int* timeGranularitySeconds,
  char* units, const int unitsLength, char* type, const int typeLength);