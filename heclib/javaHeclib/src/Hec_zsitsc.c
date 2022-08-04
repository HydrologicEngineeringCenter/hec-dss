#include <jni.h>
#include "heclib.h"
#include <string.h>

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsitsc
   (JNIEnv *env, jobject obj, jintArray j_ifltab,
   jstring j_pathname, jintArray j_times, jfloatArray j_floatValues,
   jdoubleArray j_doubleValues, jint j_lDouble, jint j_numberValues, jint j_startDate,
   jintArray j_flags, jint j_storeFlags,
   jstring j_units, jstring j_type,
   jdoubleArray j_coordinates, jint j_numberCoordinates,
   jintArray j_coordDescription, jint j_numbCoordDescription,
	jstring j_supplementary,
	jint j_timezoneOffset, jstring j_timezoneName,
   jint j_inFlag, jintArray j_status)

{
    int *ifltab;
    const char *pathname;
    int *times;
	float  *floatValues;
    double *doubleValues;
	int		lDouble;
    int numberValues;
    int startDate;
    int  *flags;
    int    storeFlags;
    const char *units;
    const char *type;
    double *coordinates;
	int   numberCoordinates;
	int  *coordDescription;
	int   numbCoordDescription;
	const char *supplementary;
    const char* supp;
	int	  timezoneOffset;
	const char *timezoneName;
    int inFlag;
    int *status;
    const char buff = '\0';

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab   = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname = (*env)->GetStringUTFChars (env, j_pathname, 0);
    times    = (*env)->GetIntArrayElements (env, j_times, 0);
	floatValues    = (*env)->GetFloatArrayElements (env, j_floatValues, 0);
    doubleValues   = (*env)->GetDoubleArrayElements (env, j_doubleValues, 0);
	lDouble	  = (int) j_lDouble;
    numberValues = (int)j_numberValues;
    startDate = (int)j_startDate;
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
    inFlag    = (int)j_inFlag;
    status    = (*env)->GetIntArrayElements (env, j_status, 0);

    if (supplementary == NULL) {
        supp = &buff;
    }
    else
    {
        supp = supplementary;
    }

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zsitsc, pathname: ", pathname);
	}

    zsitsc_ ((long long*)ifltab, pathname, times, floatValues, doubleValues,
            &lDouble, &numberValues, &startDate, flags, &storeFlags,
            units, type, 
			coordinates, &numberCoordinates,
             coordDescription, &numbCoordDescription,
			 supp, &timezoneOffset, timezoneName,
            &inFlag, status, strlen(pathname),
            strlen(units), strlen(type), strlen(supp), strlen(timezoneName));


    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_times, times, 0);
	(*env)->ReleaseFloatArrayElements (env, j_floatValues, floatValues, 0);
    (*env)->ReleaseDoubleArrayElements (env, j_doubleValues, doubleValues, 0);
    (*env)->ReleaseIntArrayElements (env, j_flags, flags, 0);
    (*env)->ReleaseStringUTFChars (env, j_units, units);
    (*env)->ReleaseStringUTFChars (env, j_type, type);
    (*env)->ReleaseDoubleArrayElements (env, j_coordinates, coordinates, 0);
	(*env)->ReleaseIntArrayElements (env, j_coordDescription, coordDescription, 0);
	(*env)->ReleaseStringUTFChars (env, j_supplementary, supplementary);
	(*env)->ReleaseStringUTFChars (env, j_timezoneName, timezoneName);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
}
