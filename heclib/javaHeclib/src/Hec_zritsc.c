#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zritsc
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jint j_startJulian, jint j_startMinutes,
     jint j_endJulian, jint j_endMinutes,
	 jint j_lGetDoubles, jintArray j_lReadDoubles, 
     jintArray j_timeBuffer, jfloatArray j_floatValues, jdoubleArray j_doubleValues, 
	 jint j_maxValues, jintArray j_numberRead, jintArray j_beginJulian,
     jintArray j_flags, jint j_readFlags, jintArray j_flagsRead,
     jobject j_units, jobject j_type, jobject j_supplementalInfo,
	 jintArray j_timezoneOffset, jobject j_timezoneName, 
	 jdoubleArray j_coordinates, jintArray j_coordinateDescription, jintArray j_coordinatesUsed,
     jint j_inflag, jintArray j_status)
{
    int *ifltab;
    const char *pathname;
    int startJulian;
    int startMinutes;
    int endJulian;
    int endMinutes;
	int    lGetDoubles;
	int   *lReadDoubles;
    int *timeBuffer;
	float  *floatValues;
    double *doubleValues;
    int maxValues;
    int *numberRead;
    int *beginJulian;
    int  *flags;
    int    readFlags;
    int  *flagsRead;
	int  *timezoneOffset;
	double *coordinates;
	int  *coordinateDescription;
	int  *coordinatesUsed;
    int   inflag;
    int *status;

    char units[12];
    char type[12];
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;
	char timezoneName[50];
	char supplementalInfo[2001];

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab       = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname     = (*env)->GetStringUTFChars (env, j_pathname, 0);
    startJulian  = (int) j_startJulian;
    startMinutes = (int) j_startMinutes;
    endJulian    = (int) j_endJulian;
    endMinutes   = (int) j_endMinutes;
	lGetDoubles = (int) j_lGetDoubles;
	lReadDoubles = (*env)->GetIntArrayElements (env, j_lReadDoubles, 0);
    timeBuffer   = (*env)->GetIntArrayElements (env, j_timeBuffer, 0);
	floatValues = (*env)->GetFloatArrayElements (env, j_floatValues, 0);
    doubleValues         = (*env)->GetDoubleArrayElements (env, j_doubleValues, 0);
    maxValues     = (int) j_maxValues;
    numberRead   = (*env)->GetIntArrayElements (env, j_numberRead, 0);
    beginJulian  = (*env)->GetIntArrayElements (env, j_beginJulian, 0);
    flags        = (*env)->GetIntArrayElements (env, j_flags, 0);
    readFlags    = (int) j_readFlags;
    flagsRead    = (*env)->GetIntArrayElements (env, j_flagsRead, 0);
	timezoneOffset=(*env)->GetIntArrayElements (env, j_timezoneOffset, 0);
	coordinates = (*env)->GetDoubleArrayElements (env, j_coordinates, 0);
    coordinateDescription = (*env)->GetIntArrayElements (env, j_coordinateDescription, 0);
	coordinatesUsed = (*env)->GetIntArrayElements (env, j_coordinatesUsed, 0);
    inflag       = (int) j_inflag;
    status       = (*env)->GetIntArrayElements (env, j_status, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zritsc; Pathname: ", pathname);
	}

   zritsc_ ((long long*)ifltab, pathname, &startJulian, &startMinutes,
            &endJulian, &endMinutes, &lGetDoubles, lReadDoubles, 
			timeBuffer, floatValues, doubleValues,
            &maxValues, numberRead, beginJulian,
            flags, &readFlags, flagsRead,
            units, type, supplementalInfo,
            timezoneOffset, timezoneName,
			coordinates, coordinateDescription, coordinatesUsed,
            &inflag, status,
            strlen (pathname), sizeof(units), sizeof(type), sizeof(supplementalInfo),
			sizeof(timezoneName));

   if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After zritsc.  Status: ", status[0]);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zritsc; numberValues: ", numberRead[0]);
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

	cls = (*env)->GetObjectClass (env, j_supplementalInfo);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {       
        chrlnb_ (supplementalInfo, &n, sizeof(supplementalInfo)-1);
		if ((n < 0) || (n > (sizeof(supplementalInfo)-1))) n = 0;
        supplementalInfo[n] = '\0';
        jstr = (*env)->NewStringUTF(env, supplementalInfo);
        (*env)->SetObjectField (env, j_supplementalInfo, fid, jstr);
    }

	cls = (*env)->GetObjectClass (env, j_timezoneName);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (timezoneName, &n, sizeof(timezoneName)-1);
		if ((n < 0) || (n > (sizeof(timezoneName)-1))) n = 0;
        timezoneName[n] = '\0';
        jstr = (*env)->NewStringUTF(env, timezoneName);
        (*env)->SetObjectField (env, j_timezoneName, fid, jstr);
    }

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_zritsc, status: ", status[0]);
	}

	  /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
	(*env)->ReleaseIntArrayElements (env, j_lReadDoubles, lReadDoubles, 0);
    (*env)->ReleaseIntArrayElements (env, j_timeBuffer, timeBuffer, 0);
	(*env)->ReleaseFloatArrayElements (env, j_floatValues, floatValues, 0);
    (*env)->ReleaseDoubleArrayElements (env, j_doubleValues, doubleValues, 0);
    (*env)->ReleaseIntArrayElements (env, j_numberRead, numberRead, 0);
    (*env)->ReleaseIntArrayElements (env, j_beginJulian, beginJulian, 0);
    (*env)->ReleaseIntArrayElements (env, j_flags, flags, 0);
    (*env)->ReleaseIntArrayElements (env, j_flagsRead, flagsRead, 0);
    (*env)->ReleaseIntArrayElements (env, j_timezoneOffset, timezoneOffset, 0);
	(*env)->ReleaseDoubleArrayElements (env, j_coordinates, coordinates, 0);
    (*env)->ReleaseIntArrayElements (env, j_coordinateDescription, coordinateDescription, 0);
	(*env)->ReleaseIntArrayElements (env, j_coordinatesUsed, coordinatesUsed, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}
