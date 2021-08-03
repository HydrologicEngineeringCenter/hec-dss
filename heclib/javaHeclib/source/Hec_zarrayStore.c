#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "zerrorCodes.h"
#include "javaHeclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zarrayStore
    (JNIEnv       *env, 
	jobject       obj, 
	jintArray    j_ifltab,
	jobject		  j_arrayContainer) 
{
	
	jclass cls;
    jfieldID fid;
	jstring j_pathname;

    int *ifltab;
	int status;
	
	zStructArray *arrayStruct;
	
	int numberValues;
    const char *pathname;

	jintArray		jintArray;
	jfloatArray		jfloatArray;
	jdoubleArray	jdoubleArray;

	int				*intArray;
	float			*floatArray;
	double			*doubleArray;


	jintArray = 0;
	jfloatArray = 0;
	jdoubleArray = 0;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_arrayContainer);
    fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_pathname = (*env)->GetObjectField(env, j_arrayContainer, fid); 
        pathname = (*env)->GetStringUTFChars(env, j_pathname,  0);
    }
	
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_1zarrayStore, Enter.  Pathname: ", pathname);		
	}

	arrayStruct = zstructArrayNew(pathname);
	if (!arrayStruct) {
		(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
		return (jint)-1;
	}
	(*env)->ReleaseStringUTFChars (env, j_pathname, pathname);


	//  ints?
	cls = (*env)->GetObjectClass (env, j_arrayContainer);
	fid = (*env)->GetFieldID (env, cls, "intArray", "[I");
	if (fid) {
		jintArray = (*env)->GetObjectField (env, j_arrayContainer, fid);
		if (jintArray) {
			intArray = (*env)->GetIntArrayElements(env, jintArray, 0);
			if (intArray) {
				numberValues = (int)(*env)->GetArrayLength(env, jintArray);
				if (numberValues > 0) {
					if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
						zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_1zarrayStore, Storing ints, number: ", numberValues);		
					}
					arrayStruct->intArray = intArray;
					arrayStruct->numberIntArray = numberValues;					
				}
			}
		}
	}

	fid = (*env)->GetFieldID (env, cls, "floatArray", "[F");
	if (fid) {
		jfloatArray = (*env)->GetObjectField (env, j_arrayContainer, fid);
		if (jfloatArray) {
			floatArray = (*env)->GetFloatArrayElements(env, jfloatArray, 0);
			if (floatArray) {
				numberValues = (int)(*env)->GetArrayLength(env, jfloatArray);
				if (numberValues > 0) {
					if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
						zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_1zarrayStore, Storing floats, number: ", numberValues);		
					}
					arrayStruct->floatArray = floatArray;
					arrayStruct->numberFloatArray = numberValues;
				}
			}
		}
	}

	fid = (*env)->GetFieldID (env, cls, "doubleArray", "[D");
	if (fid) {
		jdoubleArray = (*env)->GetObjectField (env, j_arrayContainer, fid);
		if (jdoubleArray) {
			doubleArray = (*env)->GetDoubleArrayElements(env, jdoubleArray, 0);
			if (doubleArray) {
				numberValues = (int)(*env)->GetArrayLength(env, jdoubleArray);
				if (numberValues > 0) {
					if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
						zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_1zarrayStore, Storing doubles, number: ", numberValues);		
					}
					arrayStruct->doubleArray = doubleArray;
					arrayStruct->numberDoubleArray = numberValues;
				}
			}
		}
	}
	
	status = zarrayStore((long long *)ifltab, arrayStruct);

	if (jintArray) {
		(*env)->ReleaseIntArrayElements(env, jintArray, intArray, 0);
	}
	if (jfloatArray) {
		(*env)->ReleaseFloatArrayElements(env, jfloatArray, floatArray, 0);
	}
	if (jdoubleArray) {
		(*env)->ReleaseDoubleArrayElements(env, jdoubleArray, doubleArray, 0);
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_1zarrayStore, Exit.  Status: ", status);
	}

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);

	return (jint)status;
}
