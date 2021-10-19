#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"
#include "javaHeclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1ztinRetrieve(
	JNIEnv       *env, 
	jobject       obj, 
	jintArray    j_ifltab,		                                              
	jobject		 j_tinContainer,
	jboolean	 j_retrieveData)
	                               
{
	jclass cls;
    jfieldID fid;
    jstring j_cpath;	
	jstring jstr;	
	jdoubleArray jdoubArray;
	double *doubArray;
	jintArray	jintAry;
	jobjectArray stringArray;
	jint jnumber;
	jdouble jdoub;
	jclass stringClass;
	jclass intArrayClass;
	jobjectArray doubleDim;	

	const char *cpath;
	int i;
	int ipos;
	int lenLabels;
	int status;
	int count;
	int len;
	int number;
	int boolRetrieveData;
	int *ifltab;

	zStructSpatialTin *tinStruct;

	
	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	status = 0;
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_tinContainer);
    fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_cpath = (*env)->GetObjectField(env, j_tinContainer, fid); 
        cpath = (*env)->GetStringUTFChars(env, j_cpath,  0);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_ztinRetrieve; Pathname: ", cpath);
		}
    }
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PATHNAME,
			0, 0, zdssErrorSeverity.WARNING, "", "ztinRetrieve");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	tinStruct = zstructSpatialTinNew(cpath); 
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);
	(*env)->DeleteLocalRef(env, j_cpath);

	if (!tinStruct) {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
			0, 0, zdssErrorSeverity.MEMORY_ERROR, tinStruct->pathname, "ztinRetrieve, tinStruct");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	if ((int)j_retrieveData) {
		boolRetrieveData = 1;
	}
	else {
		boolRetrieveData = 0;
	}

	status = zspatialTinRetrieve((long long *)ifltab, tinStruct, boolRetrieveData);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After tin data retrieve.  Status: ", status);
	}

	if (status != STATUS_OKAY) {
		zstructFree(tinStruct);	
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return (jint)status;
	}


	// Geospatial metadata
	
	fid = (*env)->GetFieldID (env, cls, "SpatialReferenceSystem", "Ljava/lang/String;");
	if (fid) {
		if (tinStruct->SpatialReferenceSystem) {
			jstr = (*env)->NewStringUTF(env, tinStruct->SpatialReferenceSystem);
			(*env)->SetObjectField (env, j_tinContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "SpatialReferenceSystem string read:  ", tinStruct->SpatialReferenceSystem);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}		
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PARAMETER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, SpatialReferenceSystem");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "SRSType", "I");
	if (fid) {
		jnumber = (jint)tinStruct->SRSType;
		(*env)->SetIntField(env, j_tinContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "SRSType read:  ", tinStruct->SRSType);
		}
	}	
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, SRSType");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "SRSName", "Ljava/lang/String;");
	if (fid) {
		if (tinStruct->SRSName) {
			jstr = (*env)->NewStringUTF(env, tinStruct->SRSName);
			(*env)->SetObjectField (env, j_tinContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "SRSName string read:  ", tinStruct->SRSName);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}		
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PARAMETER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, SRSName");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "SRSUnits", "Ljava/lang/String;");
	if (fid) {
		if (tinStruct->SRSUnits) {
			jstr = (*env)->NewStringUTF(env, tinStruct->SRSUnits);
			(*env)->SetObjectField (env, j_tinContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "SRSUnits string read:  ", tinStruct->SRSUnits);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}		
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PARAMETER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, SRSUnits");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}


	//  metadata for data values
	fid = (*env)->GetFieldID (env, cls, "units", "Ljava/lang/String;");
	if (fid) {
		if (tinStruct->units) {
			jstr = (*env)->NewStringUTF(env, tinStruct->units);
			(*env)->SetObjectField (env, j_tinContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "units string read:  ", tinStruct->units);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}		
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PARAMETER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, units");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "type", "Ljava/lang/String;");
	if (fid) {
		if (tinStruct->type) {
			jstr = (*env)->NewStringUTF(env, tinStruct->type);
			(*env)->SetObjectField (env, j_tinContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "type string read:  ", tinStruct->type);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}		
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PARAMETER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, type");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "timeZoneName", "Ljava/lang/String;");
	if (fid) {
		if (tinStruct->timeZoneName) {
			jstr = (*env)->NewStringUTF(env, tinStruct->timeZoneName);
			(*env)->SetObjectField (env, j_tinContainer, fid, jstr);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "timeZoneName string read:  ", tinStruct->timeZoneName);
			}
			(*env)->DeleteLocalRef(env, jstr);
		}		
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PARAMETER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, timeZoneName");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "minXCoordinate", "D");
	if (fid) {
		jdoub = (jdouble)tinStruct->minXCoordinate;
		(*env)->SetDoubleField(env, j_tinContainer, fid, jdoub);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, minXCoordinate");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "minYCoordinate", "D");
	if (fid) {
		jdoub = (jdouble)tinStruct->minYCoordinate;
		(*env)->SetDoubleField(env, j_tinContainer, fid, jdoub);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, minYCoordinate");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "maxXCoordinate", "D");
	if (fid) {
		jdoub = (jdouble)tinStruct->maxXCoordinate;
		(*env)->SetDoubleField(env, j_tinContainer, fid, jdoub);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, maxXCoordinate");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "maxYCoordinate", "D");
	if (fid) {
		jdoub = (jdouble)tinStruct->maxYCoordinate;
		(*env)->SetDoubleField(env, j_tinContainer, fid, jdoub);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, maxYCoordinate");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "minValue", "D");
	if (fid) {
		jdoub = (jdouble)tinStruct->minValue;
		(*env)->SetDoubleField(env, j_tinContainer, fid, jdoub);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, minValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "meanValue", "D");
	if (fid) {
		jdoub = (jdouble)tinStruct->meanValue;
		(*env)->SetDoubleField(env, j_tinContainer, fid, jdoub);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, meanValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "maxValue", "D");
	if (fid) {
		jdoub = (jdouble)tinStruct->maxValue;
		(*env)->SetDoubleField(env, j_tinContainer, fid, jdoub);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, maxValue");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	// TIN is made up of points and connections
	fid = (*env)->GetFieldID (env, cls, "numberPoints", "I");
	if (fid) {
		jnumber = (jint)tinStruct->numberPoints;
		(*env)->SetIntField(env, j_tinContainer, fid, jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "numberPoints read:  ", tinStruct->numberPoints);
		}
		if (tinStruct->numberPoints <= 0) {
			status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
				tinStruct->numberPoints, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, numberPoints");
			(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
			return status;
		}
	}	
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, numberPoints");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "slendernessRatio", "D");
	if (fid) {
		jdoub = (jdouble)tinStruct->slendernessRatio;
		(*env)->SetDoubleField(env, j_tinContainer, fid, jdoub);		
	}	
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, slendernessRatio");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	if (boolRetrieveData) {
		//  Set the coordinates array
		doubArray = (double *)calloc(tinStruct->numberPoints, 8);
		fid = (*env)->GetFieldID(env, cls, "xCoordinate", "[D");
		if (fid) {
			if (tinStruct->xCoordinate) {
				convertDataArray((void *)tinStruct->xCoordinate, (void *)doubArray, tinStruct->numberPoints, 1, 2);
				jdoubArray = (*env)->NewDoubleArray(env, (jint)tinStruct->numberPoints);
				(*env)->SetDoubleArrayRegion(env, jdoubArray, 0, (jint)tinStruct->numberPoints, doubArray);
				(*env)->SetObjectField(env, j_tinContainer, fid, jdoubArray);
				(*env)->DeleteLocalRef(env, jdoubArray);
			}
		}
		fid = (*env)->GetFieldID(env, cls, "yCoordinate", "[D");
		if (fid) {
			if (tinStruct->yCoordinate) {
				convertDataArray((void *)tinStruct->yCoordinate, (void *)doubArray, tinStruct->numberPoints, 1, 2);
				jdoubArray = (*env)->NewDoubleArray(env, (jint)tinStruct->numberPoints);
				(*env)->SetDoubleArrayRegion(env, jdoubArray, 0, (jint)tinStruct->numberPoints, doubArray);
				(*env)->SetObjectField(env, j_tinContainer, fid, jdoubArray);
				(*env)->DeleteLocalRef(env, jdoubArray);
			}
		}
		fid = (*env)->GetFieldID(env, cls, "value", "[D");
		if (fid) {
			if (tinStruct->value) {
				convertDataArray((void *)tinStruct->value, (void *)doubArray, tinStruct->numberPoints, 1, 2);
				jdoubArray = (*env)->NewDoubleArray(env, (jint)tinStruct->numberPoints);
				(*env)->SetDoubleArrayRegion(env, jdoubArray, 0, (jint)tinStruct->numberPoints, doubArray);
				(*env)->SetObjectField(env, j_tinContainer, fid, jdoubArray);
				(*env)->DeleteLocalRef(env, jdoubArray);
			}
		}
		free(doubArray);

		fid = (*env)->GetFieldID(env, cls, "pointType", "[I");
		if (fid) {
			if (tinStruct->pointType) {
				jintAry = (*env)->NewIntArray(env, (jint)tinStruct->numberPoints);
				(*env)->SetIntArrayRegion(env, jintAry, 0, (jint)tinStruct->numberPoints, tinStruct->pointType);
				(*env)->SetObjectField(env, j_tinContainer, fid, jintAry);
				(*env)->DeleteLocalRef(env, jintAry);
			}
		}


		//  Set the (doubly-dimensioned) connection array
		if ((tinStruct->connectTo) && (tinStruct->numberConnections)) {
			//  Create and copy the double dimensioned array
			jnumber = (jint)tinStruct->numberPoints;
			intArrayClass = (*env)->FindClass(env, "[I");
			doubleDim = (*env)->NewObjectArray(env, jnumber, intArrayClass, 0);
			count = 0;
			for (i = 0; i < tinStruct->numberPoints; i++) {
				number = tinStruct->numberConnections[i];
				jintAry = (*env)->NewIntArray(env, (jsize)number);
				(*env)->SetIntArrayRegion(env, jintAry, 0, (jsize)number, &tinStruct->connectTo[count]);
				count += number;
				(*env)->SetObjectArrayElement(env, doubleDim, i, jintAry);
				(*env)->DeleteLocalRef(env, jintAry);
			}
			fid = (*env)->GetFieldID(env, cls, "connection", "[[I");
			if (fid) {
				(*env)->SetObjectField(env, j_tinContainer, fid, doubleDim);
			}
		}

		if ((tinStruct->pointLabel > 0) && (tinStruct->pointLabelLen > 0)) {
			//  Count the number of labels
			count = 0;
			for (i = 0; i < tinStruct->pointLabelLen; i++) {
				if (tinStruct->pointLabel[i] == '\0') {
					count++;  //  For "\0" end of each string
				}
			}
			stringClass = (*env)->FindClass(env, "Ljava/lang/String;");
			stringArray = (*env)->NewObjectArray(env, (jint)count, stringClass, 0);
			ipos = 0;
			lenLabels = tinStruct->pointLabelLen;
			for (i = 0; i < count; i++) {
				len = (int)strnlen_hec(&tinStruct->pointLabel[ipos], lenLabels);
				jstr = (*env)->NewStringUTF(env, &tinStruct->pointLabel[ipos]);
				(*env)->SetObjectArrayElement(env, stringArray, i, jstr);
				(*env)->DeleteLocalRef(env, jstr);
				ipos += len + 1;
				lenLabels -= len + 1;
				if (lenLabels <= 0) {
					break;
				}
			}
			fid = (*env)->GetFieldID(env, cls, "label", "[Ljava/lang/String;");
			if ((*env)->ExceptionOccurred(env)) {
				(*env)->ExceptionClear(env);
			}
			else if (fid) {
				(*env)->SetObjectField(env, j_tinContainer, fid, stringArray);
			}
		}
	}
	

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After tin data retrieve.  Status: ", status);
	}

	zstructFree(tinStruct);	
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}
