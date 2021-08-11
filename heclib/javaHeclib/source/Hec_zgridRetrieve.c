#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"
#include "javaHeclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zgridRetrieveVersion(
	JNIEnv       *env,
	jobject       obj,
	jintArray    j_ifltab,
	jstring		 j_path,
	jintArray    j_version)
{

	const char *cpath;
	int status;
	int gridStructVersion;


	int *ifltab;

	status = 0;
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);

	cpath = (*env)->GetStringUTFChars(env, j_path, 0);
	if (cpath == NULL) return -1;

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zgridRetrieveVersion; Pathname: ", cpath);
	}


	status = zspatialGridRetrieveVersion((long long *)ifltab,cpath,&gridStructVersion);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After zspatialGridRetrieveVersion.  gridStructVersion: ", gridStructVersion);
	}


	(*env)->SetIntArrayRegion(env, j_version, 0, 1, &gridStructVersion);
	(*env)->ReleaseStringUTFChars(env, j_path, cpath);  // release resources
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return status;
}


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zgridRetrieve(
	JNIEnv       *env,
	jobject       obj,
	jintArray    j_ifltab,
	jobject		 j_gridContainer,
	jboolean	 j_retrieveData)

{
	jclass cls;
	jfieldID fid;
	jstring j_cpath;
	jstring jstr;
	jint jnumber;
	jfloat jflnumber;
	jfloatArray	jflArray;
	float			*flArray;
	jintArray	jintAry;
	
	int boolRetrieveData;

	

	const char *cpath;
	int status;



	int *ifltab;
	

	zStructSpatialGrid *gridStruct;


	jint capacity = 47;
	(*env)->EnsureLocalCapacity(env, capacity);
	status = 0;
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);

	//  Get the pathname
	cls = (*env)->GetObjectClass(env, j_gridContainer);
	fid = (*env)->GetFieldID(env, cls, "fullName", "Ljava/lang/String;");
	if (fid != 0) {
		j_cpath = (*env)->GetObjectField(env, j_gridContainer, fid);
		cpath = (*env)->GetStringUTFChars(env, j_cpath, 0);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zgridRetrieve; Pathname: ", cpath);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PATHNAME,
			0, 0, zdssErrorSeverity.WARNING, "", "zgridRetrieve");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	gridStruct = zstructSpatialGridNew(cpath);
	(*env)->ReleaseStringUTFChars(env, j_cpath, cpath);
	(*env)->DeleteLocalRef(env, j_cpath);

	if (!gridStruct) {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
			0, 0, zdssErrorSeverity.MEMORY_ERROR, gridStruct->pathname, "zgridRetrieve, gridStruct");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	if ((int)j_retrieveData) {
		boolRetrieveData = 1;
	}
	else {
		boolRetrieveData = 0;
	}

	status = zspatialGridRetrieve((long long *)ifltab, gridStruct, boolRetrieveData);

	
	//printGridStruct(gridStruct);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After grid data retrieve.  Status: ", status);
	}

	if (status != STATUS_OKAY) {
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return (jint)status;
	}

	// Data Units
	fid = (*env)->GetFieldID(env, cls, "_dataUnits", "Ljava/lang/String;");
	if (fid) {
		if (gridStruct->_dataUnits) {
			jstr = (*env)->NewStringUTF(env, gridStruct->_dataUnits);
			(*env)->SetObjectField(env, j_gridContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_dataUnits string read:  ", gridStruct->_dataUnits);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _dataUnits");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}


	// Data Source
	fid = (*env)->GetFieldID(env, cls, "_dataSource", "Ljava/lang/String;");
	if (fid) {
		if (gridStruct->_dataSource) {
			jstr = (*env)->NewStringUTF(env, gridStruct->_dataSource);
			(*env)->SetObjectField(env, j_gridContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_dataSource string read:  ", gridStruct->_dataSource);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _dataSource");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	// Source Name
	fid = (*env)->GetFieldID(env, cls, "_srsName", "Ljava/lang/String;");
	if (fid) {
		if (gridStruct->_srsName) {
			jstr = (*env)->NewStringUTF(env, gridStruct->_srsName);
			(*env)->SetObjectField(env, j_gridContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_srsName string read:  ", gridStruct->_srsName);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _srsName");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	// Source Definition
	fid = (*env)->GetFieldID(env, cls, "_srsDefinition", "Ljava/lang/String;");
	if (fid) {
		if (gridStruct->_srsDefinition) {
			jstr = (*env)->NewStringUTF(env, gridStruct->_srsDefinition);
			(*env)->SetObjectField(env, j_gridContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_srsDefinition string read:  ", gridStruct->_srsDefinition);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _srsDefinition");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	// Timezone ID
	fid = (*env)->GetFieldID(env, cls, "_timeZoneID", "Ljava/lang/String;");
	if (fid) {
		if (gridStruct->_timeZoneID) {
			jstr = (*env)->NewStringUTF(env, gridStruct->_timeZoneID);
			(*env)->SetObjectField(env, j_gridContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_timeZoneID string read:  ", gridStruct->_timeZoneID);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _timeZoneID");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_structVersion", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_structVersion;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_structVersion read:  ", gridStruct->_structVersion);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _gridStructVersion");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_type", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_type;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_gridType read:  ", gridStruct->_type);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _gridType");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_version", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_version;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_version read:  ", gridStruct->_version);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _version");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_dataType", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_dataType;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_dataType read:  ", gridStruct->_dataType);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _dataType");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_lowerLeftCellX", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_lowerLeftCellX;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_lowerLeftCellX read:  ", gridStruct->_lowerLeftCellX);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _lowerLeftCellX");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_lowerLeftCellY", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_lowerLeftCellY;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_lowerLeftCellY read:  ", gridStruct->_lowerLeftCellY);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _lowerLeftCellY");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_numberOfCellsX", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_numberOfCellsX;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_numberOfCellsX read:  ", gridStruct->_numberOfCellsX);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _numberOfCellsX");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_numberOfCellsY", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_numberOfCellsY;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_numberOfCellsY read:  ", gridStruct->_numberOfCellsY);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _numberOfCellsY");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_compressionMethod", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_compressionMethod;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _compressionMethod:  ", gridStruct->_compressionMethod);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _compressionMethod");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_storageDataType", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_storageDataType;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "_storageDataType read:  ", gridStruct->_storageDataType);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _storageDataType");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}
	

	fid = (*env)->GetFieldID(env, cls, "_numberOfRanges", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_numberOfRanges;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _numberOfRanges:  ", gridStruct->_numberOfRanges);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _numberOfRanges");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_srsDefinitionType", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_srsDefinitionType;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _srsDefinitionType:  ", gridStruct->_srsDefinitionType);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _srsDefinitionType");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_timeZoneRawOffset", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_timeZoneRawOffset;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _timeZoneRawOffset:  ", gridStruct->_timeZoneRawOffset);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _timeZoneRawOffset");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_isInterval", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_isInterval;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _isInterval:  ", gridStruct->_isInterval);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _isInterval");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_isTimeStamped", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_isTimeStamped;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _isTimeStamped:  ", gridStruct->_isTimeStamped);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _isTimeStamped");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_cellSize", "F");
	if (fid) {
		jflnumber = (jfloat)gridStruct->_cellSize;
		(*env)->SetFloatField(env, j_gridContainer, fid, jflnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugFloat((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _cellSize:  ", gridStruct->_cellSize);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _cellSize");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}


	fid = (*env)->GetFieldID(env, cls, "_maxDataValue", "F");
	if (fid) {
		jflnumber = *(jfloat*)gridStruct->_maxDataValue;
		(*env)->SetFloatField(env, j_gridContainer, fid, jflnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugFloat((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _maxDataValue:  ", *((float*)gridStruct->_maxDataValue));
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _maxDataValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_minDataValue", "F");
	if (fid) {
		jflnumber = *(jfloat*)gridStruct->_minDataValue;
		(*env)->SetFloatField(env, j_gridContainer, fid, jflnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugFloat((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _minDataValue:  ",*((float*) gridStruct->_minDataValue));
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _minDataValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_meanDataValue", "F");
	if (fid) {
		jflnumber = *(jfloat*)gridStruct->_meanDataValue;
		(*env)->SetFloatField(env, j_gridContainer, fid, jflnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugFloat((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _meanDataValue:  ", (*(float*)gridStruct->_meanDataValue));
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _meanDataValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_xCoordOfGridCellZero", "F");
	if (fid) {
		jflnumber = (jfloat)gridStruct->_xCoordOfGridCellZero;
		(*env)->SetFloatField(env, j_gridContainer, fid, jflnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugFloat((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _xCoordOfGridCellZero:  ", gridStruct->_xCoordOfGridCellZero);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _xCoordOfGridCellZero");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_yCoordOfGridCellZero", "F");
	if (fid) {
		jflnumber = (jfloat)gridStruct->_yCoordOfGridCellZero;
		(*env)->SetFloatField(env, j_gridContainer, fid, jflnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugFloat((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _yCoordOfGridCellZero:  ", gridStruct->_yCoordOfGridCellZero);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _yCoordOfGridCellZero");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_nullValue", "F");
	if (fid) {
		jflnumber = (jfloat)gridStruct->_nullValue;
		(*env)->SetFloatField(env, j_gridContainer, fid, jflnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugFloat((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _nullValue:  ", gridStruct->_nullValue);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _nullValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_sizeofCompressedElements", "I");
	if (fid) {
		jnumber = (jint)gridStruct->_sizeofCompressedElements;
		(*env)->SetIntField(env, j_gridContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "zgridRetrieve _sizeofCompressedElements:  ", gridStruct->_sizeofCompressedElements);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, _sizeofCompressedElements");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}
	// Array values

	fid = (*env)->GetFieldID(env, cls, "_rangeLimitTable", "[F");
	if (fid) {
		flArray = (float *)calloc(gridStruct->_numberOfRanges, 4);
		if (gridStruct->_rangeLimitTable) {
			convertDataArray((void *)gridStruct->_rangeLimitTable, (void *)flArray, gridStruct->_numberOfRanges, 1, 1);
			jflArray = (*env)->NewFloatArray(env, (jint)gridStruct->_numberOfRanges);
			(*env)->SetFloatArrayRegion(env, jflArray, 0, (jint)gridStruct->_numberOfRanges, flArray);
			(*env)->SetObjectField(env, j_gridContainer, fid, jflArray);
			(*env)->DeleteLocalRef(env, jflArray);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, no range limit table");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "_numberEqualOrExceedingRangeLimit", "[I");
	if (fid) {
		jintAry = (*env)->NewIntArray(env, (jint)gridStruct->_numberOfRanges);
		if (jintAry) {
			(*env)->SetIntArrayRegion(env, jintAry, 0, (jint)gridStruct->_numberOfRanges, gridStruct->_numberEqualOrExceedingRangeLimit);
			(*env)->SetObjectField(env, j_gridContainer, fid, jintAry);
			(*env)->DeleteLocalRef(env, jintAry);
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, no range histogram array");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(gridStruct);
		return status;
	}
	if (boolRetrieveData)
	{
		fid = (*env)->GetFieldID(env, cls, "_data", "[F");
		if (fid) {
			int gridSize = gridStruct->_numberOfCellsX * gridStruct->_numberOfCellsY;
			flArray = (float*)calloc(gridSize, 4);
			if (gridSize) {
				convertDataArray((void*)gridStruct->_data, (void*)flArray, gridSize, 1, 1);
				jflArray = (*env)->NewFloatArray(env, (jint)gridSize);
				(*env)->SetFloatArrayRegion(env, jflArray, 0, (jint)gridSize, gridStruct->_data);
				(*env)->SetObjectField(env, j_gridContainer, fid, jflArray);
				(*env)->DeleteLocalRef(env, jflArray);
			}
		}
		else {
			status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
				0, 0, zdssErrorSeverity.WARNING, gridStruct->pathname, "zgridRetrieve, no data");
			(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
			zstructFree(gridStruct);
			return status;
		}
	}
	zstructFree(gridStruct);
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}

