#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"
#include "javaHeclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zpdStore(
	JNIEnv       *env, 
	jobject       obj, 
	jintArray    j_ifltab,		                                              
	jobject		 j_pairedDataContainer)
	                               
{

	const char *cpath;

	jclass cls;
    jfieldID fid;
	jboolean jbool;
    jstring j_cpath;
	jboolean jboolLabels;
	
	jstring jstr;	
	jobjectArray doubleDim;	
	jobjectArray objectArray;
	jdoubleArray values;
	jdoubleArray	jordinateArray;
	double			*ordinates;
	double *vals;


	int i, j, k;
	int storageFlag;
	int number;
	int count;
	int len;
	int status;
	int numberCurves;
	int numberOrdinates;
	int *ifltab;

	const char *cstr;

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
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zpdStore; Pathname: ", cpath);
		}
    }
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PATHNAME,
			0, 0, zdssErrorSeverity.WARNING, "", "zpdStore");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	pdc = zstructPdNew(cpath); 	
	if (!pdc) {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
			0, 0, zdssErrorSeverity.MEMORY_ERROR, cpath, "zpdStore");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);

	fid = (*env)->GetFieldID (env, cls, "numberCurves", "I");
	if (fid) {
		number = (int)(*env)->GetIntField(env, j_pairedDataContainer, fid);
		pdc->numberCurves = number;
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, pdc->pathname, "zpdStore, numberCurves");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "numberOrdinates", "I");
	if (fid) {
		number = (int)(*env)->GetIntField(env, j_pairedDataContainer, fid);
		pdc->numberOrdinates = number;
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, pdc->pathname, "zpdStore, numberOrdinates");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "numberOrdinates: ", pdc->numberOrdinates);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "numberCurves: ", pdc->numberCurves);
	}
	fid = (*env)->GetFieldID (env, cls, "xOrdinates", "[D");
	if (fid) {
		jordinateArray = (*env)->GetObjectField (env, j_pairedDataContainer, fid);
		if (jordinateArray) {
			ordinates = (*env)->GetDoubleArrayElements(env, jordinateArray, 0);
			if (ordinates) {
				numberOrdinates = (int)(*env)->GetArrayLength(env, jordinateArray);
				if (numberOrdinates < 1) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
						numberOrdinates, 0, zdssErrorSeverity.WARNING, pdc->pathname, "zpdStore, no ordinates");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					(*env)->ReleaseDoubleArrayElements (env, jordinateArray, ordinates, 0);
					return status;
				}
				pdc->doubleOrdinates = (double *)calloc(numberOrdinates, 8);
				if (!pdc->doubleOrdinates) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
						numberOrdinates, 0, zdssErrorSeverity.MEMORY_ERROR, pdc->pathname, "zpdStore");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					return status;
				}
				pdc->allocated[zSTRUCT_PD_doubleOridnates] = 1;			
				for (j=0; j<numberOrdinates; j++) {
					pdc->doubleOrdinates[j] = ordinates[j];
				}
				(*env)->ReleaseDoubleArrayElements(env, jordinateArray, ordinates, 0);
				(*env)->DeleteLocalRef(env, jordinateArray);				
			}
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, pdc->pathname, "zpdStore, no Ordinates");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}
	
	fid = (*env)->GetFieldID (env, cls, "yOrdinates", "[[D");
	if (fid) {
		doubleDim = (*env)->GetObjectField (env, j_pairedDataContainer, fid);
		if (doubleDim) {
			numberCurves = (int)(*env)->GetArrayLength(env, doubleDim);
			for (i=0; i<numberCurves; i++) {
				values = (*env)->GetObjectArrayElement(env, doubleDim, i);
				numberOrdinates = (int)(*env)->GetArrayLength(env, values);		
				if (i == 0) {
					number = numberCurves * numberOrdinates;
					pdc->doubleValues = (double *)calloc(number, 8);
					pdc->allocated[zSTRUCT_PD_doubleValues] = 1;
				}
				vals = (*env)->GetDoubleArrayElements(env, values, 0);
				for (j=0; j<numberOrdinates; j++) {
					k = (i * numberOrdinates) + j;
					pdc->doubleValues[k] = vals[j];
				}
				(*env)->ReleaseDoubleArrayElements(env, values, vals, 0);
				(*env)->DeleteLocalRef(env, values);
			}
			(*env)->DeleteLocalRef(env, doubleDim);
		}
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

	fid = (*env)->GetFieldID (env, cls, "xunits", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_pairedDataContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				pdc->unitsIndependent = mallocAndCopy(cstr);
				pdc->allocated[zSTRUCT_unitsIndependent] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	fid = (*env)->GetFieldID (env, cls, "yunits", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_pairedDataContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				pdc->unitsDependent = mallocAndCopy(cstr);
				pdc->allocated[zSTRUCT_unitsDependent] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	fid = (*env)->GetFieldID (env, cls, "xtype", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_pairedDataContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				pdc->typeIndependent = mallocAndCopy(cstr);
				pdc->allocated[zSTRUCT_typeIndependent] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	fid = (*env)->GetFieldID (env, cls, "ytype", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_pairedDataContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				pdc->typeDependent = mallocAndCopy(cstr);
				pdc->allocated[zSTRUCT_typeDependent] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	fid = (*env)->GetFieldID (env, cls, "labelsUsed", "Z");
	if (fid) {
		jboolLabels = (*env)->GetBooleanField(env, j_pairedDataContainer, fid);
	}
	else {
		jboolLabels = 0;
	}

	if(jboolLabels) {
		fid = (*env)->GetFieldID (env, cls, "labels", "[Ljava/lang/String;");
		if (fid) {
			objectArray = (*env)->GetObjectField (env, j_pairedDataContainer, fid);
			if (objectArray) {
				numberCurves = (int)(*env)->GetArrayLength(env, objectArray);
				//  First commpute the amount of space needed for the lables
				count = 0;
				for (i=0; i<numberCurves; i++) {
					jstr = (jstring) (*env)->GetObjectArrayElement(env, objectArray, i);
					if (jstr) {
						cstr = (*env)->GetStringUTFChars(env, jstr, 0);
						if (cstr) {
							count += (int)strlen(cstr);																	
							(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
						}
					}
				}
				if (count > 0) {
					count += numberCurves;  //  For "\0" end of each string
					//  Now create the label array and fill it
					pdc->labelsLength = count;
					pdc->labels = (char *)calloc(count, 1);
					count = 0;
					for (i=0; i<numberCurves; i++) {
						jstr = (jstring) (*env)->GetObjectArrayElement(env, objectArray, i);
						if (jstr) {
							cstr = (*env)->GetStringUTFChars(env, jstr, 0);
							if (cstr) {
								len = (int)strlen(cstr);
								for (j=0; j<len; j++) {
									pdc->labels[count++] = cstr[j];
								}
								(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
							}
						}
						pdc->labels[count++] = '\0';
					}
				}
			}
		}
	}
	else {
		pdc->labelsLength = 0;
	}

	fid = (*env)->GetFieldID (env, cls, "xprecision", "I");
	if (fid) {
		number = (int)(*env)->GetIntField(env, j_pairedDataContainer, fid);
		pdc->xprecision = number;
	}
	fid = (*env)->GetFieldID (env, cls, "yprecision", "I");
	if (fid) {
		number = (int)(*env)->GetIntField(env, j_pairedDataContainer, fid);
		pdc->yprecision = number;
	}
		
	fid = (*env)->GetFieldID (env, cls, "storedAsdoubles", "Z");
	if (fid) {
		jbool = (*env)->GetBooleanField(env, j_pairedDataContainer, fid);
		if (jbool) {
			storageFlag = 2;
		}
		else {
			storageFlag = 1;
		}
	}
	else {
		storageFlag = 1;
	}

	fid = (*env)->GetFieldID (env, cls, "writeMethod", "I");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
	}
	else if (fid) {			
		number = (int)(*env)->GetIntField(env, j_pairedDataContainer, fid);
		if (number > 0) {
			if (number == 10) {
				storageFlag += 10;
			}
			else if (number == 11) {
				pdc->startingCurve = pdc->numberCurves;
				pdc->endingCurve = pdc->numberCurves;
			}
		}
	}

	
	//  User header
	fid = (*env)->GetFieldID (env, cls, "supplementalInfo", "Ljava/lang/String;");
	if (fid) {
		jstr = (*env)->GetObjectField(env, j_pairedDataContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				len = (int)strlen(cstr);
				if (len > 0) {
					pdc->userHeaderNumber = numberIntsInBytes(len);					
					pdc->userHeader = (int *)calloc(pdc->userHeaderNumber, 4);
					charInt ((void *)cstr, pdc->userHeader, len, (pdc->userHeaderNumber * 4), 1, 1, 0);
					pdc->allocated[zSTRUCT_userHeader] = 1;
				}
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	//  Time zone
/*	fid = (*env)->GetFieldID (env, cls, "timeZoneID", "Ljava/lang/String;");
	if (fid) {
		jstr = (*env)->GetObjectField(env, j_pairedDataContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tss->timeZoneName = mallocAndCopy(cstr);
				tss->allocated[zSTRUCT_timeZoneName] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }
	*/

	Hec_zlocationToStruct(env, obj, j_pairedDataContainer, pdc->locationStruct);


	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Before paired data store.  storageFlag: ", storageFlag);
	}

	status = zpdStore((long long *)ifltab, pdc, storageFlag);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After paired data store.  Status: ", status);
	}

	zstructFree(pdc);
	
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}
