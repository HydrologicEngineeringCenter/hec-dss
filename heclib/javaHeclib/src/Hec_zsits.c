#include <jni.h>
#include "heclib.h"
#include <string.h>

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsits
   (JNIEnv *env, jobject obj, jintArray j_ifltab,
   jstring j_pathname, jintArray j_times,
   jfloatArray j_values, jint j_numberValues, jint j_startDate,
   jstring j_unitsX, jstring j_typeX, jint j_inFlag,
   jintArray j_status)

{
    int *ifltab;
    const char *pathname;
    int *times;
    float *values;
    int numberValues;
    int startDate;
    const char *unitsX;
    const char *typeX;
    int inFlag;
    int *status;

    ifltab   = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname = (*env)->GetStringUTFChars (env, j_pathname, 0);
    times    = (*env)->GetIntArrayElements (env, j_times, 0);
    values   = (*env)->GetFloatArrayElements (env, j_values, 0);
    numberValues = (int)j_numberValues;
    startDate = (int)j_startDate;
    unitsX   = (*env)->GetStringUTFChars (env, j_unitsX, 0);
    typeX    = (*env)->GetStringUTFChars (env, j_typeX, 0);
    inFlag   = (int)j_inFlag;
    status   = (*env)->GetIntArrayElements (env, j_status, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zsits; Pathname: ", pathname);
	}

    zsits_ ((long long*)ifltab, pathname, times, values,
            &numberValues, &startDate, unitsX, typeX,
            &inFlag, status, strlen(pathname),
            strlen(unitsX), strlen(typeX));

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_times, times, 0);
    (*env)->ReleaseFloatArrayElements (env, j_values, values, 0);
    (*env)->ReleaseStringUTFChars (env, j_unitsX, unitsX);
    (*env)->ReleaseStringUTFChars (env, j_typeX, typeX);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
}
