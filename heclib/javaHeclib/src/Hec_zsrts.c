#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsrts
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jstring j_startDate, jstring j_startTime, jint j_number,
     jfloatArray j_data, jstring j_units, jstring j_type,
     jintArray j_offset, jintArray j_status)
{
    int *ifltab;
    int    number;
    float *data;
    int  *offset;
    int  *status;
    const char *pathname;
    const char *startDate;
    const char *startTime;
    const char *units;
    const char *type;

    ifltab    = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname  = (*env)->GetStringUTFChars (env, j_pathname, 0);
    startDate = (*env)->GetStringUTFChars (env, j_startDate, 0);
    startTime = (*env)->GetStringUTFChars (env, j_startTime, 0);
    units     = (*env)->GetStringUTFChars (env, j_units, 0);
    type      = (*env)->GetStringUTFChars (env, j_type, 0);
    number    = (int) j_number;
    data      = (*env)->GetFloatArrayElements (env, j_data, 0);
    offset    = (*env)->GetIntArrayElements (env, j_offset, 0);
    status    = (*env)->GetIntArrayElements (env, j_status, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zsrts, pathname: ", pathname);
	}

    zsrts_ ((long long*)ifltab, pathname, startDate, startTime, &number, data,
            units, type, offset, status,
            strlen (pathname), strlen (startDate), strlen (startTime),
            strlen (units), strlen (type));

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseStringUTFChars (env, j_startDate, startDate);
    (*env)->ReleaseStringUTFChars (env, j_startTime, startTime);
    (*env)->ReleaseStringUTFChars (env, j_units, units);
    (*env)->ReleaseStringUTFChars (env, j_type, type);
    (*env)->ReleaseFloatArrayElements (env, j_data, data, 0);
    (*env)->ReleaseIntArrayElements (env, j_offset, offset, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
}
