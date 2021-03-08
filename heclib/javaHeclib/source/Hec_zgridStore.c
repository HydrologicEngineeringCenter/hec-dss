#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"
#include "javaHeclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zgridStore(
	JNIEnv       *env, 
	jobject       obj, 
	jintArray    j_ifltab,		                                              
	jobject		 j_gridContainer)
	                               
{
	jclass cls;
    jfieldID fid;
    jstring j_cpath;	
	jstring jstr;	
	jfloatArray	jflArray;
	float			*floatArray;
	jintArray	jintAry;
	int			*intAry;
	
	
	int arraySize;

	const char *cpath;
	int status;
	
	
	
	int *ifltab;
	const char *cstr;

	zStructSpatialGrid *gridStruct;

	
	jint capacity=47;
	(*env)->EnsureLocalCapacity(env, capacity);
	status = 0;
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_gridContainer);
    fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_cpath = (*env)->GetObjectField(env, j_gridContainer, fid); 
        cpath = (*env)->GetStringUTFChars(env, j_cpath,  0);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zgridStore; Pathname: ", cpath);
		}
    }
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PATHNAME,
			0, 0, zdssErrorSeverity.WARNING, "", "zgridStore");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	gridStruct = zstructSpatialGridNew(cpath); 
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);
	(*env)->DeleteLocalRef(env, j_cpath);

	if (!gridStruct) {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
			0, 0, zdssErrorSeverity.MEMORY_ERROR, gridStruct->pathname, "zgridStore, gridStruct");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}


	// Data Units
	fid = (*env)->GetFieldID (env, cls, "_dataUnits", "Ljava/lang/String;");
    if (fid) {
		jstr = (*env)->GetObjectField(env, j_gridContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				gridStruct->_dataUnits = mallocAndCopy(cstr);
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _dataUnits");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}


	// Data Source
	fid = (*env)->GetFieldID(env, cls, "_dataSource", "Ljava/lang/String;");
	if (fid) {
		jstr = (*env)->GetObjectField(env, j_gridContainer, fid);
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr, 0);
			if (cstr) {
				gridStruct->_dataSource = mallocAndCopy(cstr);
			}
			(*env)->ReleaseStringUTFChars(env, jstr, cstr);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _dataSource");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	// Source Name
	fid = (*env)->GetFieldID(env, cls, "_srsName", "Ljava/lang/String;");
	if (fid) {
		jstr = (*env)->GetObjectField(env, j_gridContainer, fid);
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr, 0);
			if (cstr) {
				gridStruct->_srsName = mallocAndCopy(cstr);
			}
			(*env)->ReleaseStringUTFChars(env, jstr, cstr);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _srsName");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	// Source Definition
	fid = (*env)->GetFieldID(env, cls, "_srsDefinition", "Ljava/lang/String;");
	if (fid) {
		jstr = (*env)->GetObjectField(env, j_gridContainer, fid);
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr, 0);
			if (cstr) {
				gridStruct->_srsDefinition = mallocAndCopy(cstr);
			}
			(*env)->ReleaseStringUTFChars(env, jstr, cstr);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _srsDefinition");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	// Timezone ID
	fid = (*env)->GetFieldID(env, cls, "_timeZoneID", "Ljava/lang/String;");
	if (fid) {
		jstr = (*env)->GetObjectField(env, j_gridContainer, fid);
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr, 0);
			if (cstr) {
				gridStruct->_timeZoneID = mallocAndCopy(cstr);
			}
			(*env)->ReleaseStringUTFChars(env, jstr, cstr);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _timeZoneID");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_type", "I");
	if (fid) {
		gridStruct->_type = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _type");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_version", "I");
	if (fid) {
		gridStruct->_version = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _version");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_dataType", "I");
	if (fid) {
		gridStruct->_dataType = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _dataType");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_lowerLeftCellX", "I");
	if (fid) {
		gridStruct->_lowerLeftCellX = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _lowerLeftCellX");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_lowerLeftCellY", "I");
	if (fid) {
		gridStruct->_lowerLeftCellY = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _lowerLeftCellY");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_numberOfCellsX", "I");
	if (fid) {
		gridStruct->_numberOfCellsX = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _numberOfCellsX");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_numberOfCellsY", "I");
	if (fid) {
		gridStruct->_numberOfCellsY = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _numberOfCellsY");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_compressionMethod", "I");
	if (fid) {
		gridStruct->_compressionMethod = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _compressionMethod");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_sizeofCompressedElements", "I");
	if (fid) {
		gridStruct->_sizeofCompressedElements = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _sizeofCompressedElements");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_numberOfRanges", "I");
	if (fid) {
		gridStruct->_numberOfRanges = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _numberOfRanges");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_srsDefinitionType", "I");
	if (fid) {
		gridStruct->_srsDefinitionType = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _srsDefinitionType");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_timeZoneRawOffset", "I");
	if (fid) {
		gridStruct->_timeZoneRawOffset = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _timeZoneRawOffset");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_isInterval", "I");
	if (fid) {
		gridStruct->_isInterval = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _isInterval");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_isTimeStamped", "I");
	if (fid) {
		gridStruct->_isTimeStamped = (int)(*env)->GetIntField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _isTimeStamped");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_cellSize", "F");
	if (fid) {
		gridStruct->_cellSize = (float)(*env)->GetFloatField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _cellSize");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}



	fid = (*env)->GetFieldID(env, cls, "_maxDataValue", "F");
	if (fid) {
		float mval = (float)(*env)->GetFloatField(env, j_gridContainer, fid);
		gridStruct->_maxDataValue = calloc(1, 4);
		convertDataArray((void *)(&mval), (void *)gridStruct->_maxDataValue, 1, 1, 1);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _maxDataValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_minDataValue", "F");
	if (fid) {
		float mval = (float)(*env)->GetFloatField(env, j_gridContainer, fid);
		gridStruct->_minDataValue = calloc(1, 4);
		convertDataArray((void *)(&mval), (void *)gridStruct->_minDataValue, 1, 1, 1);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _minDataValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_meanDataValue", "F");
	if (fid) {
		float mval = (float)(*env)->GetFloatField(env, j_gridContainer, fid);
		gridStruct->_meanDataValue = calloc(1, 4);
		convertDataArray((void *)(&mval), (void *)gridStruct->_meanDataValue, 1, 1, 1);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _meanDataValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_xCoordOfGridCellZero", "F");
	if (fid) {
		gridStruct->_xCoordOfGridCellZero = (float)(*env)->GetFloatField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _xCoordOfGridCellZero");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_yCoordOfGridCellZero", "F");
	if (fid) {
		gridStruct->_yCoordOfGridCellZero = (float)(*env)->GetFloatField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _yCoordOfGridCellZero");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_nullValue", "F");
	if (fid) {
		gridStruct->_nullValue = (float)(*env)->GetFloatField(env, j_gridContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, _nullValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	

	fid = (*env)->GetFieldID(env, cls, "_rangeLimitTable", "[F");
	if (fid) {
		jflArray = (*env)->GetObjectField(env, j_gridContainer, fid);
		if (jflArray) {
			floatArray = (*env)->GetFloatArrayElements(env, jflArray, 0);
			if (floatArray) {
				arraySize = (int)(*env)->GetArrayLength(env, jflArray);
				if (arraySize < 1) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
						arraySize, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, no range limit table");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					(*env)->ReleaseFloatArrayElements(env, jflArray, floatArray, 0);
					zstructFree(gridStruct);
					return status;
				}
				gridStruct->_rangeLimitTable = (float *)calloc(arraySize, 4);
				if (!gridStruct->_rangeLimitTable) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
						arraySize, 0, zdssErrorSeverity.MEMORY_ERROR, gridStruct->pathname, "zgridStore,  rangeLimitTable");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					zstructFree(gridStruct);
					return status;
				}
				convertDataArray((void *)floatArray, (void *)gridStruct->_rangeLimitTable, arraySize, 1, 1);
				(*env)->ReleaseFloatArrayElements(env, jflArray, floatArray, 0);
				(*env)->DeleteLocalRef(env, jflArray);
			}
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, no range limit table");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_numberEqualOrExceedingRangeLimit", "[I");
	if (fid) {
		jintAry = (*env)->GetObjectField(env, j_gridContainer, fid);
		if (jintAry) {
			intAry = (*env)->GetIntArrayElements(env, jintAry, 0);
			if (intAry) {
				arraySize = (int)(*env)->GetArrayLength(env, jintAry);
				if (arraySize < 1) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
						arraySize, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, no range histogram array");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					(*env)->ReleaseIntArrayElements(env, jintAry, intAry, 0);
					zstructFree(gridStruct);
					return status;
				}
				gridStruct->_numberEqualOrExceedingRangeLimit = (int *)calloc(arraySize, 4);
				if (!gridStruct->_numberEqualOrExceedingRangeLimit) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
						arraySize, 0, zdssErrorSeverity.MEMORY_ERROR, gridStruct->pathname, "zgridStore, range histogram");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					zstructFree(gridStruct);
					return status;
				}
				convertDataArray(intAry, gridStruct->_numberEqualOrExceedingRangeLimit, arraySize, 1, 1);
				(*env)->ReleaseIntArrayElements(env, jintAry, intAry, 0);
				(*env)->DeleteLocalRef(env, jintAry);
			}
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, no range histogram array");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_data", "[F");
	if (fid) {
		jflArray = (*env)->GetObjectField(env, j_gridContainer, fid);
		if (jflArray) {
			floatArray = (*env)->GetFloatArrayElements(env, jflArray, 0);
			if (floatArray) {
				arraySize = (int)(*env)->GetArrayLength(env, jflArray);
				if (arraySize < 1) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
						arraySize, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, data");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					(*env)->ReleaseFloatArrayElements(env, jflArray, floatArray, 0);
					zstructFree(gridStruct);
					return status;
				}
				gridStruct->_data = (float *)calloc(arraySize, 4);
				if (!gridStruct->_data) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
						arraySize, 0, zdssErrorSeverity.MEMORY_ERROR, gridStruct->pathname, "zgridStore,  data");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					zstructFree(gridStruct);
					return status;
				}
				convertDataArray((void *)floatArray, (void *)gridStruct->_data, arraySize, 1, 1);
				(*env)->ReleaseFloatArrayElements(env, jflArray, floatArray, 0);
				(*env)->DeleteLocalRef(env, jflArray);
			}
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridStore, no data");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Before grid data store ", "");
	}

	//printGridStruct(gridStruct);
	status = 0;
	status = zspatialGridStore((long long *)ifltab, gridStruct);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After grid data store.  Status: ", status);
	}

	zstructFree(gridStruct);

	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}
