#include <string.h>
#include <jni.h>
#include <stdio.h>
#include "heclib.h"

//  Opens either version 6 or 7.
//  To create a new version 6, pass "6" in status
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

    if (*status == 0)
    {
        char* dssVersion = getenv("DEFAULT_DSS_VERSION");
        //printf("\ndssVersion = %s", dssVersion);
        if (dssVersion && strcmp(dssVersion, "6") == 0)
        {
            *status = 6;
        }
        else if (dssVersion && strcmp(dssVersion, "7") == 0)
        {
            *status = 7;
        }
    }


	if (*status == 6) {
		*status = zopen6((long long*)ifltab, fileName);
	}
	else {
		*status = hec_dss_zopen ((long long*)ifltab, fileName);
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, " Heclib_Hec_1zopen, exit status: ", *status);
	}

    /* Release the file name */
    (*env)->ReleaseStringUTFChars (env, j_name, fileName);
    /* Make sure the file table and status are returned! */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
	return *status;
}

JNIEXPORT int JNICALL Java_hec_heclib_util_Heclib_Hec_1zopen6
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

    *status = zopen6((long long*)ifltab, fileName);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_1zopen, exit status: ", *status);
	}

    /* Release the file name */
    (*env)->ReleaseStringUTFChars (env, j_name, fileName);
    /* Make sure the file table and status are returned! */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
	return *status;
}


JNIEXPORT int JNICALL Java_hec_heclib_util_Heclib_Hec_1zopen7
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

    *status = zopen7((long long*)ifltab, fileName);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_1zopen, status: ", *status);
	}

    /* Release the file name */
    (*env)->ReleaseStringUTFChars (env, j_name, fileName);
    /* Make sure the file table and status are returned! */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
	return *status;
}

