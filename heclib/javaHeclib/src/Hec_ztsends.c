#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1ztsends
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jint j_searchOption, jintArray j_startJulian, jintArray j_startMinutes,
     jintArray j_endJulian, jintArray j_endMinutes, jintArray j_exists)
{

    int* ifltab = (*env)->GetIntArrayElements (env, j_ifltab,  0);
    const char* path = (*env)->GetStringUTFChars (env, j_pathname,  0);
    int searchOption = (int) j_searchOption; // ignored
    int* startJulian  = (*env)->GetIntArrayElements (env, j_startJulian,  0);
    int* startMinutes = (*env)->GetIntArrayElements (env, j_startMinutes, 0);
    int* endJulian    = (*env)->GetIntArrayElements (env, j_endJulian,    0);
    int* endMinutes   = (*env)->GetIntArrayElements (env, j_endMinutes,   0);
    int* exists       = (*env)->GetIntArrayElements (env, j_exists     ,  0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_ztsends; Pathname: ", path);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "searchOption: ", searchOption);
	}

	int startSeconds;
	int endSeconds;
	
	int status = ztsGetDateTimeRange((long long*)ifltab, path, 1, startJulian, &startSeconds, endJulian, &endSeconds);
	if (status == STATUS_RECORD_FOUND) {
		*exists = 1;
		*startMinutes = startSeconds / SECS_IN_1_MINUTE;
		*endMinutes = endSeconds / SECS_IN_1_MINUTE;
	}
	else {
		*exists = 0;
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {				
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsends, startJulian: ", startJulian[0]);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsends, startMinutes: ", startMinutes[0]);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsends, endJulian: ", endJulian[0]);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsends, endMinutes: ", endMinutes[0]);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_ztsends, exists: ", exists[0]);
	}

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars   (env, j_pathname, path);
    (*env)->ReleaseIntArrayElements (env, j_startJulian, startJulian, 0);
    (*env)->ReleaseIntArrayElements (env, j_startMinutes, startMinutes, 0);
    (*env)->ReleaseIntArrayElements (env, j_endJulian, endJulian, 0);
    (*env)->ReleaseIntArrayElements (env, j_endMinutes, endMinutes, 0);
    (*env)->ReleaseIntArrayElements (env, j_exists, exists, 0);

}
