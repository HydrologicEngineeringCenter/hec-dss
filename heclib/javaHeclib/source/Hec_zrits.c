#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zrits
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jint j_startJulian, jint j_startMinutes,
     jint j_endJulian, jint j_endMinutes,
     jintArray j_timeBuffer, jfloatArray j_data, jint j_dataSize,
     jintArray j_numberRead, jintArray j_beginJulian,
     jobject j_units, jobject j_type,
     jintArray j_status)
{
    int *ifltab;
    const char *pathname;
    int startJulian;
    int startMinutes;
    int endJulian;
    int endMinutes;
    int *timeBuffer;
    float *data;
    int dataSize;
    int *numberRead;
    int *beginJulian;
    int *status;

    char units[9];
    char type[9];
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab       = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname     = (*env)->GetStringUTFChars (env, j_pathname, 0);
    startJulian  = (int) j_startJulian;
    startMinutes = (int) j_startMinutes;
    endJulian    = (int) j_endJulian;
    endMinutes   = (int) j_endMinutes;
    timeBuffer   = (*env)->GetIntArrayElements (env, j_timeBuffer, 0);
    data         = (*env)->GetFloatArrayElements (env, j_data, 0);
    dataSize     = (int) j_dataSize;
    numberRead   = (*env)->GetIntArrayElements (env, j_numberRead, 0);
    beginJulian  = (*env)->GetIntArrayElements (env, j_beginJulian, 0);
    status       = (*env)->GetIntArrayElements (env, j_status, 0);

    zrits_ ((long long*)ifltab, pathname, &startJulian, &startMinutes,
            &endJulian, &endMinutes, timeBuffer, data,
            &dataSize, numberRead, beginJulian,
            units, type, status,
            strlen (pathname), sizeof (units) -1, sizeof (type) -1);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_zrits, numberRead: ", numberRead[0]);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zrits, startJulian: ", startJulian);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zrits, endJulian: ", endJulian);
	}
    

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

	/* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_timeBuffer, timeBuffer, 0);
    (*env)->ReleaseFloatArrayElements (env, j_data, data, 0);
    (*env)->ReleaseIntArrayElements (env, j_numberRead, numberRead, 0);
    (*env)->ReleaseIntArrayElements (env, j_beginJulian, beginJulian, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}
