#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsrtsxd
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jstring j_startDate, jstring j_startTime, jint j_number,
     jdoubleArray j_data,
     jintArray j_flags, jint j_storeFlags,
     jstring j_units, jstring j_type,
     jintArray j_userHeader, jint j_numberHeader,
     jint j_plan, jint j_compression, jfloat j_baseValue,
     jint j_setBase, jint j_setDeltaHigh, jint j_deltaPrec,
     jintArray j_status)
{
    int *ifltab;
    const char *pathname;
    const char *startDate;
    const char *startTime;
    int    number;
    double *data;
    int  *flags;
    int    storeFlags;
    const char *units;
    const char *type;
    int  *userHeader;
    int    numberHeader;
    int   plan;
    int   compression;
    float baseValue;
    int   setBase;
    int   setDeltaHigh;
    int   deltaPrec;
    int  *status;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);


    ifltab    = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname  = (*env)->GetStringUTFChars (env, j_pathname, 0);
    startDate = (*env)->GetStringUTFChars (env, j_startDate, 0);
    startTime = (*env)->GetStringUTFChars (env, j_startTime, 0);
    number    = (int) j_number;
    data      = (*env)->GetDoubleArrayElements (env, j_data, 0);
    flags     = (*env)->GetIntArrayElements (env, j_flags, 0);
    storeFlags = (int) j_storeFlags;
    units     = (*env)->GetStringUTFChars (env, j_units, 0);
    type      = (*env)->GetStringUTFChars (env, j_type, 0);
    userHeader= (*env)->GetIntArrayElements (env, j_userHeader, 0);
    numberHeader = (int) j_numberHeader;
    plan      = (int) j_plan;
    compression=(int) j_compression;
    baseValue  = (float) j_baseValue;
    setBase    = (int) j_setBase;
    setDeltaHigh = (int) j_setDeltaHigh;
    deltaPrec  = (int) j_deltaPrec;
    status    = (*env)->GetIntArrayElements (env, j_status, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zsrtsxd, pathname: ", pathname);
	}

    zsrtsxd_ ((long long*)ifltab, pathname, startDate, startTime, &number, data,
             flags, &storeFlags,
             units, type,
             userHeader, &numberHeader,
             &plan, &compression, &baseValue, &setBase, &setDeltaHigh,
             &deltaPrec, status,
             strlen (pathname), strlen (startDate), strlen (startTime),
             strlen (units), strlen (type));

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseStringUTFChars (env, j_startDate, startDate);
    (*env)->ReleaseStringUTFChars (env, j_startTime, startTime);
    (*env)->ReleaseDoubleArrayElements (env, j_data, data, 0);
    (*env)->ReleaseIntArrayElements (env, j_flags, flags, 0);
    (*env)->ReleaseStringUTFChars (env, j_units, units);
    (*env)->ReleaseStringUTFChars (env, j_type, type);
    (*env)->ReleaseIntArrayElements (env, j_userHeader, userHeader, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}
