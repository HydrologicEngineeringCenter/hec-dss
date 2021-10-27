#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsrtsc
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jstring j_startDate, jstring j_startTime, jint j_number,
     jint j_lDouble, jfloatArray j_fData, jdoubleArray j_dData,
     jintArray j_flags, jint j_storeFlags,
     jstring j_units, jstring j_type,
	 jdoubleArray j_coordinates, jint j_numberCoordinates,
     jintArray j_coordDescription, jint j_numbCoordDescription,
	 jstring j_supplementary, 
	 jint j_timezoneOffset, jstring j_timezoneName,
     jint j_plan, jint j_compression, jfloat j_baseValue,
     jint j_setBase, jint j_setDeltaHigh, jint j_deltaPrec,
     jintArray j_status)
{
    int *ifltab;
    const char *pathname;
    const char *startDate;
    const char *startTime;
    int    number;
	int		lDouble;
	float  *fData;
    double *dData;
    int  *flags;
    int    storeFlags;
    const char *units;
    const char *type;
	double *coordinates;
	int   numberCoordinates;
	int  *coordDescription;
	int   numbCoordDescription;
	const char *supplementary;
	int	  timezoneOffset;
	const char *timezoneName;   
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
	lDouble	  = (int) j_lDouble;
    fData      = (*env)->GetFloatArrayElements (env, j_fData, 0);
	dData      = (*env)->GetDoubleArrayElements (env, j_dData, 0);
    flags     = (*env)->GetIntArrayElements (env, j_flags, 0);
    storeFlags = (int) j_storeFlags;
    units     = (*env)->GetStringUTFChars (env, j_units, 0);
    type      = (*env)->GetStringUTFChars (env, j_type, 0);
	coordinates = (*env)->GetDoubleArrayElements (env, j_coordinates, 0);
	numberCoordinates = (int) j_numberCoordinates;
	coordDescription = (*env)->GetIntArrayElements (env, j_coordDescription, 0);
	numbCoordDescription = (int) j_numbCoordDescription;
	supplementary = (*env)->GetStringUTFChars (env, j_supplementary, 0);
	timezoneOffset = (int) j_timezoneOffset;
	timezoneName = (*env)->GetStringUTFChars (env, j_timezoneName, 0);    
    plan      = (int) j_plan;
    compression=(int) j_compression;
    baseValue  = (float) j_baseValue;
    setBase    = (int) j_setBase;
    setDeltaHigh = (int) j_setDeltaHigh;
    deltaPrec  = (int) j_deltaPrec;
    status    = (*env)->GetIntArrayElements (env, j_status, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zsrtsc, pathname: ", pathname);		
	}

	zsrtsc_  ((long long*)ifltab, pathname, startDate, startTime, &number, 
		&lDouble, fData, dData,
            flags, &storeFlags,
            units, type,
			coordinates, &numberCoordinates,
            coordDescription, &numbCoordDescription,
			supplementary, &timezoneOffset, timezoneName,
            &plan, &compression, &baseValue, &setBase, &setDeltaHigh,
            &deltaPrec, status,
            strlen(pathname), strlen(startDate), strlen(startTime),
            strlen(units), strlen(type), strlen(supplementary), strlen(timezoneName));


    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseStringUTFChars (env, j_startDate, startDate);
    (*env)->ReleaseStringUTFChars (env, j_startTime, startTime);
    (*env)->ReleaseFloatArrayElements (env, j_fData, fData, 0);
	(*env)->ReleaseDoubleArrayElements (env, j_dData, dData, 0);
    (*env)->ReleaseIntArrayElements (env, j_flags, flags, 0);
    (*env)->ReleaseStringUTFChars (env, j_units, units);
    (*env)->ReleaseStringUTFChars (env, j_type, type);
	(*env)->ReleaseDoubleArrayElements (env, j_coordinates, coordinates, 0);
	(*env)->ReleaseIntArrayElements (env, j_coordDescription, coordDescription, 0);
	(*env)->ReleaseStringUTFChars (env, j_supplementary, supplementary);
	(*env)->ReleaseStringUTFChars (env, j_timezoneName, timezoneName);  
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}
