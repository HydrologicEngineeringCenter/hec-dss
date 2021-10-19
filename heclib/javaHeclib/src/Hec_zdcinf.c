#include <string.h>
#include <jni.h>
#include "heclib.h"

//  DSS Version 6 only.  Data compression parameters.

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zdcinf
    (JNIEnv *env, jobject obj, jintArray j_method, jfloatArray j_baseValue,
	jintArray j_baseSet, jintArray j_deltaSize, jintArray j_precision,
    jintArray j_status)
{
    
    int *method;
	float *baseValue;
    int *baseSet;
    int *deltaSize;
	int *precision;
	int *status;

 
    method    = (*env)->GetIntArrayElements (env, j_method,  0);
	baseValue = (*env)->GetFloatArrayElements (env, j_baseValue, 0);
    baseSet   = (*env)->GetIntArrayElements (env, j_baseSet,  0);
    deltaSize = (*env)->GetIntArrayElements (env, j_deltaSize,  0);
	precision = (*env)->GetIntArrayElements (env, j_precision,  0);
    status    = (*env)->GetIntArrayElements (env, j_status,  0);

    zdcinf_ (method, baseValue, baseSet, deltaSize,
			 precision, status);

    (*env)->ReleaseIntArrayElements (env, j_method, method, 0);
    (*env)->ReleaseFloatArrayElements (env, j_baseValue, baseValue, 0);
    (*env)->ReleaseIntArrayElements (env, j_baseSet, baseSet, 0);
    (*env)->ReleaseIntArrayElements (env, j_deltaSize, deltaSize, 0);
    (*env)->ReleaseIntArrayElements (env, j_precision, precision, 0);
	(*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}

