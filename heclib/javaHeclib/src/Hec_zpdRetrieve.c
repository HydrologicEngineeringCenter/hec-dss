#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "javaHeclib.h"
#include "verticalDatum.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zpdRetrieve(
	JNIEnv       *env, 
	jobject       obj, 
	jintArray    j_ifltab,		                                              
	jobject		 j_pairedDataContainer)
	                               
{
const char *cpath;

	jclass cls;
    jfieldID fid;
    jstring j_cpath;
	jlong jlongNumber;
	jsize size;
	jstring jstr;	
	jobjectArray doubleDim;	
	jdoubleArray values;
	jclass doubleArrayClass;
	jclass stringClass;
	jobjectArray stringArray;

	int ipos;
	int i, j;
	int lenLabels;
	int number;
	jint jnumber;
	int len;
	int status;
	char *dash;
	int idx;
	char cpart[MAX_PART_SIZE];
	int *ifltab;

	zStructPairedData *pdc;
	

	
	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	status = 0;
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_pairedDataContainer);
    fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_cpath = (*env)->GetObjectField(env, j_pairedDataContainer, fid); 
        cpath = (*env)->GetStringUTFChars(env, j_cpath,  0);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zpdRetrieve; Pathname: ", cpath);
		}
    }
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PATHNAME,
			0, 0, zdssErrorSeverity.WARNING, "", "zpdRetrieve");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	pdc = zstructPdNew(cpath); 
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);

	if (!pdc) {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
			0, 0, zdssErrorSeverity.MEMORY_ERROR, cpath, "zpdRetrieve");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	//  The following are new parameters and  not required
	fid = (*env)->GetFieldID (env, cls, "startingCurve", "I");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
	}
	else if (fid) {		
		number = (int)(*env)->GetIntField(env, j_pairedDataContainer, fid);
		pdc->startingCurve = number;		
	}

	fid = (*env)->GetFieldID (env, cls, "endingCurve", "I");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
	}
	else if (fid) {		
		number = (int)(*env)->GetIntField(env, j_pairedDataContainer, fid);
		pdc->endingCurve = number;
	}

	fid = (*env)->GetFieldID (env, cls, "startingOrdinate", "I");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
	}
	else if (fid) {	
		number = (int)(*env)->GetIntField(env, j_pairedDataContainer, fid);
		pdc->startingOrdinate = number;
	}

	fid = (*env)->GetFieldID (env, cls, "endingOrdinate", "I");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
	}
	else if (fid) {	
		number = (int)(*env)->GetIntField(env, j_pairedDataContainer, fid);
		pdc->endingOrdinate = number;
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Just before call to zpdRetrieve, startingCurve: ", pdc->startingCurve);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "endingCurve: ", pdc->endingCurve);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "startingOrdinate: ", pdc->startingOrdinate);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "endingOrdinate: ", pdc->endingOrdinate);
	}

	
	//  Retrieve the data set, getting data as doubles
	status = zpdRetrieve((long long *)ifltab, pdc, 2);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After paired data retrieve.  Status: ", status);
	}

	if (zisError(status)) {
		status = zerrorUpdate((long long*)ifltab, status, DSS_FUNCTION_javaNativeInterface_ID);
		zstructFree(pdc);
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;	
	}


	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Record does not exist; Pathname: ", pdc->pathname);
		}
		zstructFree(pdc);
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "numberOrdinates: ", pdc->numberOrdinates);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "numberCurves: ", pdc->numberCurves);
	}

	//  Start to fill in parts
	fid = (*env)->GetFieldID (env, cls, "dataType", "I");
	if (fid) {
		jnumber = (jint)pdc->dataType;
		(*env)->SetIntField(env, j_pairedDataContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "numberCurves", "I");
	if (fid) {
		jnumber = (jint)pdc->numberCurvesInStruct;
		(*env)->SetIntField(env, j_pairedDataContainer, fid, jnumber);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, pdc->pathname, "zpdRetrieve, numberCurves");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "numberOrdinates", "I");
	if (fid) {
		jnumber = (jint)pdc->numberOrdinatesInStruct;
		(*env)->SetIntField(env, j_pairedDataContainer, fid, jnumber);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, pdc->pathname, "zpdRetrieve, numberOrdinates");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	
	//  Set the ordinates array
	if (pdc->doubleOrdinates) {				
		jnumber = (jint)pdc->numberOrdinatesInStruct;
		values = (*env)->NewDoubleArray(env, jnumber);
		(*env)->SetDoubleArrayRegion(env, values, 0, jnumber, pdc->doubleOrdinates); 
		fid = (*env)->GetFieldID (env, cls, "xOrdinates", "[D");
		if (fid) {
			(*env)->SetObjectField (env, j_pairedDataContainer, fid, values);
		}
		(*env)->DeleteLocalRef(env, values);
	}

	//  Set the (doubly-dimensioned) curves array (y ordinates)
	if (pdc->doubleValues) {
		//  Create and copy the double dimensioned array
		jnumber = (jint)pdc->numberCurvesInStruct;
		doubleArrayClass = (*env)->FindClass(env, "[D");
		doubleDim = (*env)->NewObjectArray(env, jnumber, doubleArrayClass, 0);
		size = (jsize)pdc->numberOrdinatesInStruct;
		for (i=0; i<pdc->numberCurvesInStruct; i++) {
			j = i * pdc->numberOrdinatesInStruct;
			values = (*env)->NewDoubleArray(env, size);
			(*env)->SetDoubleArrayRegion(env, values, 0, size, &pdc->doubleValues[j]);
			(*env)->SetObjectArrayElement(env, doubleDim, i, values);
			(*env)->DeleteLocalRef(env, values);
		}
		fid = (*env)->GetFieldID (env, cls, "yOrdinates", "[[D");
		if (fid) {
			(*env)->SetObjectField (env, j_pairedDataContainer, fid, doubleDim);
		}
		(*env)->DeleteLocalRef(env, doubleDim);
	}

	//   Units and Type
	fid = (*env)->GetFieldID (env, cls, "xunits", "Ljava/lang/String;");
	if (fid) {
		if (pdc->unitsIndependent) {
			jstr = (*env)->NewStringUTF(env, pdc->unitsIndependent);
		}
		else {
			jstr = (*env)->NewStringUTF(env, "");
		}
		(*env)->SetObjectField (env, j_pairedDataContainer, fid, jstr);
		(*env)->DeleteLocalRef(env, jstr);
	}

	fid = (*env)->GetFieldID (env, cls, "xtype", "Ljava/lang/String;");
	if (fid) {
		if (pdc->typeIndependent) {
			jstr = (*env)->NewStringUTF(env, pdc->typeIndependent);
		}
		else {
			jstr = (*env)->NewStringUTF(env, "");
		}
		(*env)->SetObjectField (env, j_pairedDataContainer, fid, jstr);	
		(*env)->DeleteLocalRef(env, jstr);
	}

	fid = (*env)->GetFieldID (env, cls, "yunits", "Ljava/lang/String;");
	if (fid) {
		if (pdc->unitsDependent) {
			jstr = (*env)->NewStringUTF(env, pdc->unitsDependent);
		}
		else {
			jstr = (*env)->NewStringUTF(env, "");
		}		
		(*env)->SetObjectField(env, j_pairedDataContainer, fid, jstr);
		(*env)->DeleteLocalRef(env, jstr);
	}

	fid = (*env)->GetFieldID (env, cls, "ytype", "Ljava/lang/String;");
	if (fid) {
		if (pdc->typeDependent) {
			jstr = (*env)->NewStringUTF(env, pdc->typeDependent);
		}
		else {
			jstr = (*env)->NewStringUTF(env, "");
		}	
		(*env)->SetObjectField(env, j_pairedDataContainer, fid, jstr);
		(*env)->DeleteLocalRef(env, jstr);
	}


	if (pdc->labelsLength > 0) {
		fid = (*env)->GetFieldID (env, cls, "labelsUsed", "Z");
		if (fid) {
			(*env)->SetBooleanField(env, j_pairedDataContainer, fid, JNI_TRUE);
		}
		jnumber = (jint)pdc->numberCurvesInStruct;				
		stringClass = (*env)->FindClass(env, "Ljava/lang/String;");
		stringArray = (*env)->NewObjectArray(env, jnumber, stringClass, 0);	
		ipos = 0;
		lenLabels = pdc->labelsLength;
		for (i=0; i<pdc->numberCurvesInStruct; i++) {
			len = (int)strnlen_hec(&pdc->labels[ipos], lenLabels);
			jstr = (*env)->NewStringUTF(env, &pdc->labels[ipos]);
			(*env)->SetObjectArrayElement(env, stringArray, i, jstr);
			(*env)->DeleteLocalRef(env, jstr);				
			ipos += len + 1;
			lenLabels -= len + 1;
			if (lenLabels <= 0) {
				break;
			}
		}
		fid = (*env)->GetFieldID (env, cls, "labels", "[Ljava/lang/String;");
		if ((*env)->ExceptionOccurred(env)) {
				(*env)->ExceptionClear(env);			
		}
		else if (fid) {
			(*env)->SetObjectField (env, j_pairedDataContainer, fid, stringArray);
		}	
		(*env)->DeleteLocalRef(env, stringArray);
	}
	else {
		fid = (*env)->GetFieldID (env, cls, "labelsUsed", "Z");
		if (fid) {
			(*env)->SetBooleanField(env, j_pairedDataContainer, fid, JNI_FALSE);
		}
	}


	//  Precision
	fid = (*env)->GetFieldID (env, cls, "xprecision", "I");
	if (fid) {
		jnumber = (jint)pdc->xprecision;
		(*env)->SetIntField(env, j_pairedDataContainer, fid, jnumber);
	}
	fid = (*env)->GetFieldID (env, cls, "yprecision", "I");
	if (fid) {
		jnumber = (jint)pdc->yprecision;
		(*env)->SetIntField(env, j_pairedDataContainer, fid, jnumber);
	}

	//  User header (supplemental)
	if (pdc->userHeaderNumber > 0) {
		fid = (*env)->GetFieldID (env, cls, "supplementalInfo", "Ljava/lang/String;");
		if (fid) {
			char *headerString = NULL;
			if (zgetVersion((long long *)ifltab) == 7) {
				headerString = userHeaderToString(pdc->userHeader, pdc->userHeaderNumber);
			}
			else {
				headerString = (char *)pdc->userHeader;
			}
			jstr = (*env)->NewStringUTF(env, (const char *)headerString);
			(*env)->SetObjectField (env, j_pairedDataContainer, fid, jstr);
			(*env)->DeleteLocalRef(env, jstr);
			if (zgetVersion((long long *)ifltab) == 7) {
				free(headerString);
			}
		}
	}


	//  Now fill in extra container values
	//  watershed
	len = zpathnameGetPart (pdc->pathname, 1, cpart, sizeof(cpart));
	if (len > 0) {
		fid = (*env)->GetFieldID (env, cls, "watershed", "Ljava/lang/String;");
		if (fid) {
			jstr = (*env)->NewStringUTF(env, cpart);
			(*env)->SetObjectField (env, j_pairedDataContainer, fid, jstr);
			(*env)->DeleteLocalRef(env, jstr);
		}
	}

	//  location
	len = zpathnameGetPart (pdc->pathname, 2, cpart, sizeof(cpart));
	if (len > 0) {
		fid = (*env)->GetFieldID (env, cls, "location", "Ljava/lang/String;");
		if (fid) {
			jstr = (*env)->NewStringUTF(env, cpart);
			(*env)->SetObjectField (env, j_pairedDataContainer, fid, jstr);
			(*env)->DeleteLocalRef(env, jstr);
		}
	}

	//  parameter
	len = zpathnameGetPart (pdc->pathname, 3, cpart, sizeof(cpart));	
	if (len > 0) {
		dash = strchr(cpart, '-');
		if (dash > 0) {
			idx = (int)(dash - cpart);
			fid = (*env)->GetFieldID(env, cls, "xparameter", "Ljava/lang/String;");
			if (fid) {
				cpart[idx] = '\0';
				jstr = (*env)->NewStringUTF(env, cpart);
				(*env)->SetObjectField(env, j_pairedDataContainer, fid, jstr);
				(*env)->DeleteLocalRef(env, jstr);
			}
			fid = (*env)->GetFieldID(env, cls, "yparameter", "Ljava/lang/String;");
			if (fid) {
				idx++;
				jstr = (*env)->NewStringUTF(env, &cpart[idx]);
				(*env)->SetObjectField(env, j_pairedDataContainer, fid, jstr);
				(*env)->DeleteLocalRef(env, jstr);
			}
		}
	}
	
	//  version
	len = zpathnameGetPart (pdc->pathname, 6, cpart, sizeof(cpart));
	if (len > 0) {
		fid = (*env)->GetFieldID (env, cls, "version", "Ljava/lang/String;");
		if (fid) {
			jstr = (*env)->NewStringUTF(env, cpart);
			(*env)->SetObjectField (env, j_pairedDataContainer, fid, jstr);
			(*env)->DeleteLocalRef(env, jstr);
		}
	}

	//  Last write time
	fid = (*env)->GetFieldID (env, cls, "lastWriteTimeMillis", "J");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		jlongNumber = (jlong)pdc->lastWrittenTime;
		(*env)->SetLongField(env, j_pairedDataContainer, fid, jlongNumber);
	}	

	//  File last write time
	fid = (*env)->GetFieldID (env, cls, "fileLastWriteTimeMillis", "J");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		jlongNumber = (jlong)pdc->fileLastWrittenTime;
		(*env)->SetLongField(env, j_pairedDataContainer, fid, jlongNumber);
	}


	if (pdc->locationStruct) {						
		Hec_zlocationFromStruct(env, obj, j_pairedDataContainer, pdc->locationStruct);
	}
	

	zstructFree(pdc);	
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}
