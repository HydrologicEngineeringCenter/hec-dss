#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zrrtsxd
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jstring j_startDate, jstring j_startTime, jint j_number,
     jdoubleArray j_data,
     jintArray j_flags, jint j_readFlags, jintArray j_flagsRead,
     jobject j_units, jobject j_type,
     jintArray j_userHeader, jint j_userHeaderMax, jintArray j_userHeaderRead,
     jintArray j_offset, jintArray j_compression, jintArray j_status)
{
    int *ifltab;
    const char *pathname;
    const char *startDate;
    const char *startTime;
    int    number;
    double *data;
    int  *flags;
    int    readFlags;
    int  *flagsRead;
    int  *userHeader;
    int    userHeaderMax;
    int  *userHeaderRead;
    int  *offset;
    int  *compression;
    int  *status;

    char units[50];
    char type[50];
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
//	printf("Enter Heclib_Hec_zrrtsxd\n ");

    ifltab    = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname  = (*env)->GetStringUTFChars (env, j_pathname, 0);
    startDate = (*env)->GetStringUTFChars (env, j_startDate, 0);
    startTime = (*env)->GetStringUTFChars (env, j_startTime, 0);
    number    = (int) j_number;
    data      = (*env)->GetDoubleArrayElements (env, j_data, 0);
    flags     = (*env)->GetIntArrayElements (env, j_flags, 0);
    readFlags = (int) j_readFlags;
    flagsRead = (*env)->GetIntArrayElements (env, j_flagsRead, 0);
    userHeader= (*env)->GetIntArrayElements (env, j_userHeader, 0);
    userHeaderMax = (int) j_userHeaderMax;
    userHeaderRead = (*env)->GetIntArrayElements (env, j_userHeaderRead, 0);
    offset    = (*env)->GetIntArrayElements (env, j_offset, 0);
    compression=(*env)->GetIntArrayElements (env, j_compression, 0);
    status    = (*env)->GetIntArrayElements (env, j_status, 0);
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zrrtsxd, pathname: ", pathname);
	}

   zrrtsxd_ ((long long*)ifltab, pathname, startDate, startTime, &number, data,
             flags, &readFlags, flagsRead,
             units, type,
             userHeader, &userHeaderMax, userHeaderRead,
             offset, compression, status,
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
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_zrrtsd; status: ", status[0]);
	}

	/* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseStringUTFChars (env, j_startDate, startDate);
    (*env)->ReleaseStringUTFChars (env, j_startTime, startTime);
    (*env)->ReleaseDoubleArrayElements (env, j_data, data, 0);
    (*env)->ReleaseIntArrayElements (env, j_flags, flags, 0);
    (*env)->ReleaseIntArrayElements (env, j_flagsRead, flagsRead, 0);
    (*env)->ReleaseIntArrayElements (env, j_userHeader, userHeader, 0);
    (*env)->ReleaseIntArrayElements (env, j_userHeaderRead, userHeaderRead, 0);
    (*env)->ReleaseIntArrayElements (env, j_offset, offset, 0);
    (*env)->ReleaseIntArrayElements (env, j_compression, compression, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}
