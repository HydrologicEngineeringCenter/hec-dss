#include <jni.h>
#include "heclib.h"
#include <string.h>

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsitsxd
   (JNIEnv *env, jobject obj, jintArray j_ifltab,
   jstring j_pathname, jintArray j_times,
   jdoubleArray j_values, jint j_numberValues, jint j_startDate,
   jintArray j_flags, jint j_storeFlags,
   jstring j_units, jstring j_type,
   jintArray j_userHeader, jint j_numberHeader, jint j_inFlag,
   jintArray j_status)

{
    int *ifltab;
    const char *pathname;
    int *times;
    double *values;
    int numberValues;
    int startDate;
    int  *flags;
    int    storeFlags;
    const char *units;
    const char *type;
    int  *userHeader;
    int    numberHeader;
    int inFlag;
    int *status;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab   = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname = (*env)->GetStringUTFChars (env, j_pathname, 0);
    times    = (*env)->GetIntArrayElements (env, j_times, 0);
    values   = (*env)->GetDoubleArrayElements (env, j_values, 0);
    numberValues = (int)j_numberValues;
    startDate = (int)j_startDate;
    flags     = (*env)->GetIntArrayElements (env, j_flags, 0);
    storeFlags = (int) j_storeFlags;
    units     = (*env)->GetStringUTFChars (env, j_units, 0);
    type      = (*env)->GetStringUTFChars (env, j_type, 0);
    userHeader= (*env)->GetIntArrayElements (env, j_userHeader, 0);
    numberHeader = (int) j_numberHeader;
    inFlag    = (int)j_inFlag;
    status    = (*env)->GetIntArrayElements (env, j_status, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zsitsxd, pathname: ", pathname);
	}

    zsitsxd_ ((long long*)ifltab, pathname, times, values,
            &numberValues, &startDate, flags, &storeFlags,
            units, type, userHeader, &numberHeader,
            &inFlag, status, strlen(pathname),
            strlen(units), strlen(type));

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_times, times, 0);
    (*env)->ReleaseDoubleArrayElements (env, j_values, values, 0);
    (*env)->ReleaseIntArrayElements (env, j_flags, flags, 0);
    (*env)->ReleaseStringUTFChars (env, j_units, units);
    (*env)->ReleaseStringUTFChars (env, j_type, type);
    (*env)->ReleaseIntArrayElements (env, j_userHeader, userHeader, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
}
