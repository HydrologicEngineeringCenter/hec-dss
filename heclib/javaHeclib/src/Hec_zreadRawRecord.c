#include <jni.h>
#include <string.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zreadRawRecord(
	JNIEnv       *env,
	jobject       obj, 
	jintArray    j_ifltab,		                                                
	jobject		 j_rawContainer,
	jboolean	 j_getLengthsOnly)  
{

	const char *cpath;
	zStructRecordSize *recordSize;
	zStructTransfer *ztransfer;
	int status;		
	int *ifltab;

	jclass cls;
    jfieldID fid;
    jstring j_cpath;
	jstring jstr;
	jint jnumber;
	jlong jlongNumber;
	jintArray intArray;

	
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
		
	recordSize = zstructRecordSizeNew(cpath);
	status = zgetRecordSize((long long *)ifltab, recordSize);
	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Record does not exist; Pathname: ", cpath);
		}
		zstructFree(recordSize);
		(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "dataType", "I");
	if (fid) {
		jnumber = (jint)recordSize->dataType;
		(*env)->SetIntField(env, j_rawContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "internalHeaderNumber", "I");
	if (fid) {
		jnumber = (jint)recordSize->internalHeaderNumber;
		(*env)->SetIntField(env, j_rawContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "userHeaderNumber", "I");
	if (fid) {
		jnumber = (jint)recordSize->userHeaderNumber;
		(*env)->SetIntField(env, j_rawContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "values1Number", "I");
	if (fid) {
		jnumber = (jint)recordSize->values1Number;
		(*env)->SetIntField(env, j_rawContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "values2Number", "I");
	if (fid) {
		jnumber = (jint)recordSize->values2Number;
		(*env)->SetIntField(env, j_rawContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "values3Number", "I");
	if (fid) {
		jnumber = (jint)recordSize->values3Number;
		(*env)->SetIntField(env, j_rawContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "numberValues", "I");
	if (fid) {
		jnumber = (jint)recordSize->numberValues;
		(*env)->SetIntField(env, j_rawContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "logicalNumberValues", "I");
	if (fid) {
		jnumber = (jint)recordSize->logicalNumberValues;
		(*env)->SetIntField(env, j_rawContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "allocatedSize", "I");
	if (fid) {
		jnumber = (jint)recordSize->allocatedSize;
		(*env)->SetIntField(env, j_rawContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "version", "I");
	if (fid) {
		jnumber = (jint)recordSize->version;
		(*env)->SetIntField(env, j_rawContainer, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "lastWriteTimeMillis", "J");
	if (fid) {
		jlongNumber = (jlong)recordSize->lastWriteTimeMillis;
		(*env)->SetLongField(env, j_rawContainer, fid, jlongNumber);
	}
	
	fid = (*env)->GetFieldID (env, cls, "fileLastWriteTimeMillis", "J");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		jlongNumber = (jlong)zgetLastWriteTimeFile((long long*)ifltab);
		(*env)->SetLongField(env, j_rawContainer, fid, jlongNumber);
	}

	fid = (*env)->GetFieldID (env, cls, "programName", "Ljava/lang/String;");
	if (fid) {
		if (recordSize->programLastWrite) {
			jstr = (*env)->NewStringUTF(env, recordSize->programLastWrite);
		}
		else {
			jstr = (*env)->NewStringUTF(env, "");
		}
		(*env)->SetObjectField (env, j_rawContainer, fid, jstr);
	}
	zstructFree(recordSize);

	if (!(int)j_getLengthsOnly) {

		ztransfer = zstructTransferNew(cpath, 1);
		status = zread((long long *)ifltab, ztransfer);
		if (status != STATUS_RECORD_FOUND) {
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Record does not exist; Pathname: ", cpath);
			}
			zstructFree(ztransfer);
			(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);
			(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
			return status;
		}

		if (ztransfer->internalHeader) {
			jnumber = (jint)ztransfer->internalHeaderNumber;
			fid = (*env)->GetFieldID (env, cls, "internalHeader", "[I");
			if ((*env)->ExceptionOccurred(env)) {
					(*env)->ExceptionClear(env);			
			}
			else if (fid) {
				intArray = (*env)->NewIntArray(env, jnumber);
				(*env)->SetIntArrayRegion(env, intArray, 0, jnumber, ztransfer->internalHeader); 
				(*env)->SetObjectField (env, j_rawContainer, fid, intArray);	
				(*env)->DeleteLocalRef(env, intArray);
			}
		}

		if (ztransfer->header2) {
			jnumber = (jint)ztransfer->header2Number;
			fid = (*env)->GetFieldID (env, cls, "header2", "[I");
			if ((*env)->ExceptionOccurred(env)) {
					(*env)->ExceptionClear(env);			
			}
			else if (fid) {
				intArray = (*env)->NewIntArray(env, jnumber);
				(*env)->SetIntArrayRegion(env, intArray, 0, jnumber, ztransfer->header2); 
				(*env)->SetObjectField (env, j_rawContainer, fid, intArray);	
				(*env)->DeleteLocalRef(env, intArray);
			}
		}
	
		if (ztransfer->userHeader) {
			jnumber = (jint)ztransfer->userHeaderNumber;
			fid = (*env)->GetFieldID (env, cls, "userHeader", "[I");
			if ((*env)->ExceptionOccurred(env)) {
					(*env)->ExceptionClear(env);			
			}
			else if (fid) {
				intArray = (*env)->NewIntArray(env, jnumber);
				(*env)->SetIntArrayRegion(env, intArray, 0, jnumber, ztransfer->userHeader); 
				(*env)->SetObjectField (env, j_rawContainer, fid, intArray);	
				(*env)->DeleteLocalRef(env, intArray);
			}
		}

		if (ztransfer->values1) {
			jnumber = (jint)ztransfer->values1Number;
			fid = (*env)->GetFieldID (env, cls, "values1", "[I");
			if ((*env)->ExceptionOccurred(env)) {
					(*env)->ExceptionClear(env);			
			}
			else if (fid) {
				intArray = (*env)->NewIntArray(env, jnumber);
				(*env)->SetIntArrayRegion(env, intArray, 0, jnumber, ztransfer->values1); 
				(*env)->SetObjectField (env, j_rawContainer, fid, intArray);	
				(*env)->DeleteLocalRef(env, intArray);
			}
		}

		if (ztransfer->values2) {
			jnumber = (jint)ztransfer->values2Number;
			fid = (*env)->GetFieldID (env, cls, "values2", "[I");
			if ((*env)->ExceptionOccurred(env)) {
					(*env)->ExceptionClear(env);			
			}
			else if (fid) {
				intArray = (*env)->NewIntArray(env, jnumber);
				(*env)->SetIntArrayRegion(env, intArray, 0, jnumber, ztransfer->values2); 
				(*env)->SetObjectField (env, j_rawContainer, fid, intArray);	
				(*env)->DeleteLocalRef(env, intArray);
			}
		}

		if (ztransfer->values3) {
			jnumber = (jint)ztransfer->values3Number;
			fid = (*env)->GetFieldID (env, cls, "values3", "[I");
			if ((*env)->ExceptionOccurred(env)) {
					(*env)->ExceptionClear(env);			
			}
			else if (fid) {
				intArray = (*env)->NewIntArray(env, jnumber);
				(*env)->SetIntArrayRegion(env, intArray, 0, jnumber, ztransfer->values3); 
				(*env)->SetObjectField (env, j_rawContainer, fid, intArray);	
				(*env)->DeleteLocalRef(env, intArray);
			}
		}

	}

	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);

	return (jint)status;
}
