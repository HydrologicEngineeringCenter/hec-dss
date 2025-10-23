#include <string.h>
#include <jni.h>
#include <stdio.h>
#include "heclib.h"

JNIEXPORT int JNICALL Java_hec_heclib_util_Heclib_Hec_1zopen
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_name,
     jintArray j_status)
{
    int *ifltab;
    int *status;
    const char *fileName;


    fileName = (const char *) (*env)->GetStringUTFChars (env, j_name, 0);
    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    status = (*env)->GetIntArrayElements (env, j_status, 0); 

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_1zopen, Enter.  filename: ", fileName);
	}

	*status = hec_dss_zopen ((long long*)ifltab, fileName);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, " Heclib_Hec_1zopen, exit status: ", *status);
	}

    /* Release the file name */
    (*env)->ReleaseStringUTFChars (env, j_name, fileName);
    /* Make sure the file table and status are returned! */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    int ret = status[0];
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
	  return ret;
}

