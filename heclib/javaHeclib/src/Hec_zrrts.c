#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zrrts
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jstring j_startDate, jstring j_startTime, jint j_number,
     jfloatArray j_data, jobject j_units, jobject j_type,
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

    char units[50];
    char type[50];
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;

	ifltab    = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname  = (*env)->GetStringUTFChars (env, j_pathname, 0);
    startDate = (*env)->GetStringUTFChars (env, j_startDate, 0);
    startTime = (*env)->GetStringUTFChars (env, j_startTime, 0);
    number    = (int) j_number;
    data      = (*env)->GetFloatArrayElements (env, j_data, 0);
    offset    = (*env)->GetIntArrayElements (env, j_offset, 0);
    status    = (*env)->GetIntArrayElements (env, j_status, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zrrts; Pathname: ", pathname);
	}

    zrrts_ ((long long*)ifltab, pathname, startDate, startTime, &number, data,
            units, type, offset, status,
            strlen (pathname), strlen (startDate), strlen (startTime),
            sizeof (units) -1, sizeof (type) -1);

    /* Set the units and type */
    cls = (*env)->GetObjectClass (env, j_units);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (units, &n, sizeof(units)-1);
		if ((n < 0) || (n > (sizeof(units)-1))) n = 0;
        units[n] = '\0';
        jstr = (*env)->NewStringUTF(env, units);
        (*env)->SetObjectField (env, j_units, fid, jstr);
    }

    cls = (*env)->GetObjectClass (env, j_type);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (type, &n, sizeof(type)-1);
		if ((n < 0) || (n > (sizeof(type)-1))) n = 0;
        type[n] = '\0';
        jstr = (*env)->NewStringUTF(env, type);
        (*env)->SetObjectField (env, j_type, fid, jstr);
    }

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_zrrts; status: ", status[0]);
	}

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseStringUTFChars (env, j_startDate, startDate);
    (*env)->ReleaseStringUTFChars (env, j_startTime, startTime);
    (*env)->ReleaseFloatArrayElements (env, j_data, data, 0);
    (*env)->ReleaseIntArrayElements (env, j_offset, offset, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}
