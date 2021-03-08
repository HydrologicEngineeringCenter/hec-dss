#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "zerrorCodes.h"
#include "javaHeclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zarrayRetrieve
    (JNIEnv       *env, 
	jobject       obj, 
	jintArray     j_ifltab,
	jobject		  j_arrayContainer) 
{
	
	jsize size;	
	jclass cls;
    jfieldID fid;
	jstring j_pathname;

    int *ifltab;
	int status;
	
	zStructArray *arrayStruct;
    char *pathname;

	jintArray		jintArray;
	jfloatArray		jfloatArray;
	jdoubleArray	jdoubleArray;
	jlong jlongNumber;


	//int zarrayRetrieve(long long *ifltab, zStructArray *arrayStruct);

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab =  (*env)->GetIntArrayElements (env, j_ifltab, 0);
	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_arrayContainer);
    fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_pathname = (*env)->GetObjectField(env, j_arrayContainer, fid); 
        pathname = (char *)(*env)->GetStringUTFChars(env, j_pathname,  0);
    }

	arrayStruct = zstructArrayNew(pathname);
	if (!arrayStruct) {
		return (jint)-1;
	}
	(*env)->ReleaseStringUTFChars (env, j_pathname, pathname);

	status = zarrayRetrieve((long long *)ifltab, arrayStruct);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_1zarrayRetrieve, Enter.  Pathname: ", pathname);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_1zarrayRetrieve Record found status: ", status);
	}
	

	if (status == STATUS_RECORD_FOUND) {
		//  Yes, we have it.
		//  Copy appropiate array to arrayContainer
		if ((arrayStruct->intArray) && (arrayStruct->numberIntArray > 0)) {
			//  ints			
			size = (jsize)arrayStruct->numberIntArray;
			jintArray = (*env)->NewIntArray(env, size);
			(*env)->SetIntArrayRegion(env, jintArray, 0, size, arrayStruct->intArray); 
			cls = (*env)->GetObjectClass (env, j_arrayContainer);
			fid = (*env)->GetFieldID (env, cls, "intArray", "[I");
			if (fid) {
				(*env)->SetObjectField (env, j_arrayContainer, fid, jintArray);
			}			
		}
		if ((arrayStruct->floatArray) && (arrayStruct->numberFloatArray > 0)) {
			//  floats
			size = (jsize)arrayStruct->numberFloatArray;
			jfloatArray = (*env)->NewFloatArray(env, size);
			(*env)->SetFloatArrayRegion(env, jfloatArray, 0, size, arrayStruct->floatArray); 
			cls = (*env)->GetObjectClass (env, j_arrayContainer);
			fid = (*env)->GetFieldID (env, cls, "floatArray", "[F");
			if (fid) {
				(*env)->SetObjectField (env, j_arrayContainer, fid, jfloatArray);
			}						
		}
		if ((arrayStruct->doubleArray) && (arrayStruct->numberDoubleArray > 0)) {
			//  doubles
			size = (jsize)arrayStruct->numberDoubleArray;
			jdoubleArray = (*env)->NewDoubleArray(env, size);
			(*env)->SetDoubleArrayRegion(env, jdoubleArray, 0, size, arrayStruct->doubleArray); 
			cls = (*env)->GetObjectClass (env, j_arrayContainer);
			fid = (*env)->GetFieldID (env, cls, "doubleArray", "[D");
			if (fid) {
				(*env)->SetObjectField (env, j_arrayContainer, fid, jdoubleArray);
			}				
		}

		fid = (*env)->GetFieldID (env, cls, "dataType", "I");
		if (fid) {
			(*env)->SetIntField(env, j_arrayContainer, fid, (jint)arrayStruct->dataType);
		}
	
		//  Last write time
		fid = (*env)->GetFieldID (env, cls, "lastWriteTimeMillis", "J");
		if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);
			zmessage((long long *)ifltab, "Exception in get fieldID lastWriteTimeMillis");
		}
		else if (fid) {
			jlongNumber = (jlong)arrayStruct->lastWrittenTime;
			(*env)->SetLongField(env, j_arrayContainer, fid, jlongNumber);			
		}

		//  File last write time
		fid = (*env)->GetFieldID (env, cls, "fileLastWriteTimeMillis", "J");
		if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
		}
		else if (fid) {
			jlongNumber = (jlong)arrayStruct->fileLastWrittenTime;
			(*env)->SetLongField(env, j_arrayContainer, fid, jlongNumber);
		}
		
	}

	zstructFree(arrayStruct);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_1zarrayRetrieve", "Exit");
	}

			
    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);

	return (jint)status;
}
