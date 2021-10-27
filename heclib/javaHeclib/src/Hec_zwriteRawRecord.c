#include <jni.h>
#include <string.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zwriteRawRecord(
	JNIEnv       *env,
	jobject       obj, 
	jintArray    j_ifltab,		                                                
	jobject		 j_rawContainer)  
{

	const char *cpath;
	zStructTransfer *ztransfer;
	int status;	
	int number;
	int len;
	const char *cstr;
	int *intAry;
	int *ifltab;

	jclass cls;
    jfieldID fid;
    jstring j_cpath;
	jstring jstr;
	jintArray jintAry;

	
//
	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	

	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_rawContainer);
    fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_cpath = (*env)->GetObjectField(env, j_rawContainer, fid); 
        cpath = (*env)->GetStringUTFChars(env, j_cpath,  0);
    }
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PATHNAME,
			0, 0, zdssErrorSeverity.WARNING, "", "");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	ztransfer = zstructTransferNew(cpath, 1);	
	if (!ztransfer) {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
			0, 0, zdssErrorSeverity.MEMORY_ERROR, cpath, "zwriteRawRecord");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);
		return status;
	}
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);

	fid = (*env)->GetFieldID (env, cls, "dataType", "I");
	if (fid) {		
		ztransfer->dataType = (int)(*env)->GetIntField(env, j_rawContainer, fid);
	}

	fid = (*env)->GetFieldID (env, cls, "numberValues", "I");
	if (fid) {		
		ztransfer->numberValues = (int)(*env)->GetIntField(env, j_rawContainer, fid);
	}

	fid = (*env)->GetFieldID (env, cls, "logicalNumberValues", "I");
	if (fid) {		
		ztransfer->logicalNumberValues = (int)(*env)->GetIntField(env, j_rawContainer, fid);
	}

	fid = (*env)->GetFieldID (env, cls, "version", "I");
	if (fid) {		
		ztransfer->version = (int)(*env)->GetIntField(env, j_rawContainer, fid);
	}

	fid = (*env)->GetFieldID (env, cls, "programName", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_rawContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				len = (int)strlen(cstr);
				if (len > sizeof(ztransfer->programName)) {
					len = sizeof(ztransfer->programName);
				}
				stringCopy(ztransfer->programName, sizeof(ztransfer->programName), cstr, len);
			}
			else {
				ztransfer->programName[0] = '\0';
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	fid = (*env)->GetFieldID (env, cls, "internalHeader", "[I");
	if (fid) {
		jintAry = (*env)->GetObjectField (env, j_rawContainer, fid);
		if (jintAry) {
			intAry = (*env)->GetIntArrayElements(env, jintAry, 0);
			if (intAry) {
				number = (int)(*env)->GetArrayLength(env, jintAry);
				ztransfer->internalHeaderNumber = number;
				if (number > 0) {
					ztransfer->internalHeader = (int *)calloc(number, 4);
					if (!ztransfer->internalHeader) {
						status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
							number, 0, zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "zwriteRawRecord, internalHeader");
						(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
						return status;
					}
					ztransfer->allocated[zSTRUCT_TRANS_internalHeader] = 1;	
					convertDataArray(intAry, ztransfer->internalHeader, number, 1, 1);
					(*env)->ReleaseIntArrayElements(env, jintAry, intAry, 0);
					(*env)->DeleteLocalRef(env, jintAry);	
				}
			}
		}
	}

	fid = (*env)->GetFieldID (env, cls, "header2", "[I");
	if (fid) {
		jintAry = (*env)->GetObjectField (env, j_rawContainer, fid);
		if (jintAry) {
			intAry = (*env)->GetIntArrayElements(env, jintAry, 0);
			if (intAry) {
				number = (int)(*env)->GetArrayLength(env, jintAry);
				ztransfer->header2Number = number;
				if (number > 0) {
					ztransfer->header2 = (int *)calloc(number, 4);
					if (!ztransfer->header2) {
						status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
							number, 0, zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "zwriteRawRecord, header2");
						(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
						return status;
					}
					ztransfer->allocated[zSTRUCT_TRANS_header2] = 1;	
					convertDataArray(intAry, ztransfer->header2, number, 1, 1);
					(*env)->ReleaseIntArrayElements(env, jintAry, intAry, 0);
					(*env)->DeleteLocalRef(env, jintAry);	
				}
			}
		}
	}

	fid = (*env)->GetFieldID (env, cls, "userHeader", "[I");
	if (fid) {
		jintAry = (*env)->GetObjectField (env, j_rawContainer, fid);
		if (jintAry) {
			intAry = (*env)->GetIntArrayElements(env, jintAry, 0);
			if (intAry) {
				number = (int)(*env)->GetArrayLength(env, jintAry);
				ztransfer->userHeaderNumber = number;
				if (number > 0) {
					ztransfer->userHeader = (int *)calloc(number, 4);
					if (!ztransfer->userHeader) {
						status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
							number, 0, zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "zwriteRawRecord, userHeader");
						(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
						return status;
					}
					ztransfer->allocated[zSTRUCT_userHeader] = 1;	
					convertDataArray(intAry, ztransfer->userHeader, number, 1, 1);
					(*env)->ReleaseIntArrayElements(env, jintAry, intAry, 0);
					(*env)->DeleteLocalRef(env, jintAry);	
				}
			}
		}
	}

	fid = (*env)->GetFieldID (env, cls, "values1", "[I");
	if (fid) {
		jintAry = (*env)->GetObjectField (env, j_rawContainer, fid);
		if (jintAry) {
			intAry = (*env)->GetIntArrayElements(env, jintAry, 0);
			if (intAry) {
				number = (int)(*env)->GetArrayLength(env, jintAry);
				ztransfer->values1Number = number;
				if (number > 0) {
					ztransfer->values1 = (int *)calloc(number, 4);
					if (!ztransfer->values1) {
						status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
							number, 0, zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "zwriteRawRecord, values1");
						(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
						return status;
					}
					ztransfer->allocated[zSTRUCT_TRANS_values1] = 1;	
					convertDataArray(intAry, ztransfer->values1, number, 1, 1);
					(*env)->ReleaseIntArrayElements(env, jintAry, intAry, 0);
					(*env)->DeleteLocalRef(env, jintAry);	
				}
			}
		}
	}

	fid = (*env)->GetFieldID (env, cls, "values2", "[I");
	if (fid) {
		jintAry = (*env)->GetObjectField (env, j_rawContainer, fid);
		if (jintAry) {
			intAry = (*env)->GetIntArrayElements(env, jintAry, 0);
			if (intAry) {
				number = (int)(*env)->GetArrayLength(env, jintAry);
				ztransfer->values2Number = number;
				if (number > 0) {
					ztransfer->values2 = (int *)calloc(number, 4);
					if (!ztransfer->values2) {
						status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
							number, 0, zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "zwriteRawRecord, values2");
						(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
						return status;
					}
					ztransfer->allocated[zSTRUCT_TRANS_values2] = 1;	
					convertDataArray(intAry, ztransfer->values2, number, 1, 1);
					(*env)->ReleaseIntArrayElements(env, jintAry, intAry, 0);
					(*env)->DeleteLocalRef(env, jintAry);	
				}
			}
		}
	}

	fid = (*env)->GetFieldID (env, cls, "values3", "[I");
	if (fid) {
		jintAry = (*env)->GetObjectField (env, j_rawContainer, fid);
		if (jintAry) {
			intAry = (*env)->GetIntArrayElements(env, jintAry, 0);
			if (intAry) {
				number = (int)(*env)->GetArrayLength(env, jintAry);
				ztransfer->values3Number = number;
				if (number > 0) {
					ztransfer->values3 = (int *)calloc(number, 4);
					if (!ztransfer->values3) {
						status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
							number, 0, zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "zwriteRawRecord, values3");
						(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
						return status;
					}
					ztransfer->allocated[zSTRUCT_TRANS_values3] = 1;	
					convertDataArray(intAry, ztransfer->values3, number, 1, 1);
					(*env)->ReleaseIntArrayElements(env, jintAry, intAry, 0);
					(*env)->DeleteLocalRef(env, jintAry);	
				}
			}
		}
	}

	status = zwrite((long long *)ifltab, ztransfer);
	zstructFree(ztransfer);

	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}
