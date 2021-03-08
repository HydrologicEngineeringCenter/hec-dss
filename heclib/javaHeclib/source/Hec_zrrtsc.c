#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zrrtsc
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jstring j_startDate, jstring j_startTime, jint j_maxNumber, jintArray j_numberRead,
     jint j_lGetDoubles, jintArray j_lReadDoubles, jfloatArray j_floatValues, jdoubleArray j_doubleValues,
     jintArray j_flags, jint j_readFlags, jintArray j_flagsRead,
     jobject j_units, jobject j_type,
     jobject j_supplementalInfo, jintArray j_offset, jintArray j_compression, 
	 jintArray j_timezoneOffset, jobject j_timezoneName, 
	 jdoubleArray j_coordinates, jintArray j_coordinateDescription, jintArray j_coordinatesUsed,
	 jintArray j_status)
{
    int *ifltab;
    const char *pathname;
    const char *startDate;
    const char *startTime;
	int    maxNumber;
    int  *numberRead;
	int    lGetDoubles;
	int   *lReadDoubles;
	float  *floatValues;
    double *dataValues;
    int  *flags;
    int    readFlags;
    int  *flagsRead;
    
    int  *offset;
    int  *compression;
	int  *timezoneOffset;
	double *coordinates;
	int  *coordinateDescription;
	int  *coordinatesUsed;
    int  *status;

    char units[50];
    char type[50];
	int i;
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;

	char timezoneName[50];
	char supplementalInfo[2001];

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
//	printf("Enter Hec_zrrtsc\n ");
    ifltab    = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname  = (*env)->GetStringUTFChars (env, j_pathname, 0);
    startDate = (*env)->GetStringUTFChars (env, j_startDate, 0);
    startTime = (*env)->GetStringUTFChars (env, j_startTime, 0);
	maxNumber = (int) j_maxNumber;
    numberRead = (*env)->GetIntArrayElements (env, j_numberRead, 0);
	lGetDoubles = (int) j_lGetDoubles;
	lReadDoubles = (*env)->GetIntArrayElements (env, j_lReadDoubles, 0);
	floatValues = (*env)->GetFloatArrayElements (env, j_floatValues, 0);
    dataValues = (*env)->GetDoubleArrayElements (env, j_doubleValues, 0);
    flags     = (*env)->GetIntArrayElements (env, j_flags, 0);
    readFlags = (int) j_readFlags;
    flagsRead = (*env)->GetIntArrayElements (env, j_flagsRead, 0);
    
    offset    = (*env)->GetIntArrayElements (env, j_offset, 0);
    compression=(*env)->GetIntArrayElements (env, j_compression, 0);
	timezoneOffset=(*env)->GetIntArrayElements (env, j_timezoneOffset, 0);
	coordinates = (*env)->GetDoubleArrayElements (env, j_coordinates, 0);
    coordinateDescription = (*env)->GetIntArrayElements (env, j_coordinateDescription, 0);
	coordinatesUsed = (*env)->GetIntArrayElements (env, j_coordinatesUsed, 0);
    status    = (*env)->GetIntArrayElements (env, j_status, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zrrtsc, pathname: ", pathname);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_zrrtsc, startDate: ", startDate);
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_zrrtsc, startTime: ", startTime);
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_zrrtsc, maxNumber: ", maxNumber);
		}
	}

	for (i=0; i<sizeof(timezoneName); i++) {
		timezoneName[i] = ' ';
	}

	for (i=0; i<sizeof(supplementalInfo); i++) {
		supplementalInfo[i] = ' ';
	}

	zrrtsc_ ((long long*)ifltab, pathname, startDate, startTime, &maxNumber, numberRead, 
				&lGetDoubles, lReadDoubles, floatValues, dataValues,
				flags, &readFlags, flagsRead,
				units, type, supplementalInfo,           
				offset, compression, timezoneOffset, timezoneName,
				coordinates, coordinateDescription, coordinatesUsed,
				status,
				strlen (pathname), strlen (startDate), strlen (startTime),
				(sizeof(units)-1), (sizeof(type)-1), (sizeof(supplementalInfo)-1),
				(sizeof(timezoneName)-1));


	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Completed Heclib_Hec_zrrtsc, pathname: ", pathname);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_zrrtsc, status: ", status[0]);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_zrrtsc, numberRead: ", numberRead[0]);
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

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_zrrtsc, units: ", units);
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_zrrtsc, type: ", type);
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
		if (n > 0) {
			jstr = (*env)->NewStringUTF(env, timezoneName);
		}
		else {
			jstr = (*env)->NewStringUTF(env, "");
		}
        (*env)->SetObjectField (env, j_timezoneName, fid, jstr);
    }

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_zrrtsc; status: ", status[0]);
	}

	/* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseStringUTFChars (env, j_startDate, startDate);
    (*env)->ReleaseStringUTFChars (env, j_startTime, startTime);
	(*env)->ReleaseIntArrayElements (env, j_numberRead, numberRead, 0);
	(*env)->ReleaseIntArrayElements (env, j_lReadDoubles, lReadDoubles, 0);
    (*env)->ReleaseFloatArrayElements (env, j_floatValues, floatValues, 0);
    (*env)->ReleaseDoubleArrayElements (env, j_doubleValues, dataValues, 0);
    (*env)->ReleaseIntArrayElements (env, j_flags, flags, 0);
    (*env)->ReleaseIntArrayElements (env, j_flagsRead, flagsRead, 0);   
    (*env)->ReleaseIntArrayElements (env, j_offset, offset, 0);
    (*env)->ReleaseIntArrayElements (env, j_compression, compression, 0);
	(*env)->ReleaseIntArrayElements (env, j_timezoneOffset, timezoneOffset, 0);
	(*env)->ReleaseDoubleArrayElements (env, j_coordinates, coordinates, 0);
    (*env)->ReleaseIntArrayElements (env, j_coordinateDescription, coordinateDescription, 0);
	(*env)->ReleaseIntArrayElements (env, j_coordinatesUsed, coordinatesUsed, 0);
	(*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}
