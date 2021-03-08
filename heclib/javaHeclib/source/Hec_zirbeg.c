#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zirbeg
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jint j_julian,
     jstring j_ePart, jintArray j_year, jintArray j_month,
     jintArray j_day, jintArray j_block, jintArray j_minutesInBlock,
     jintArray j_incrementBlock)
{

    //  int *ifltab;  No longer used
    int julian;
    const char *ePart;
    int *year;
    int *month;
    int *day;
    int *block;
    int *minutesInBlock;
    int *incrementBlock;
	long long itmp[2];
	


    julian         = j_julian;
    ePart          = (*env)->GetStringUTFChars (env, j_ePart, 0);
    year           = (*env)->GetIntArrayElements (env, j_year, 0);
    month          = (*env)->GetIntArrayElements (env, j_month, 0);
    day            = (*env)->GetIntArrayElements (env, j_day, 0);
    block          = (*env)->GetIntArrayElements (env, j_block, 0);
    minutesInBlock = (*env)->GetIntArrayElements (env, j_minutesInBlock, 0);
    incrementBlock = (*env)->GetIntArrayElements (env, j_incrementBlock, 0);


	if ((ePart) && (strlen(ePart) > 3)) {
		zirbeg_(&julian, ePart, year, month,
			day, block, minutesInBlock,
			incrementBlock, strlen(ePart));
		if (minutesInBlock < 0) {
			if (zmessageLevel((long long*)itmp, MESS_METHOD_JNI_ID, MESS_LEVEL_GENERAL)) {
				zmessageDebug(itmp, DSS_FUNCTION_javaNativeInterface_ID, "Invalid E part given for zirbeg: ", ePart);
			}
		}
	}
	else {
		itmp[0] = 0;
		itmp[1] = 0;
		if (zmessageLevel((long long*)itmp, MESS_METHOD_JNI_ID, MESS_LEVEL_GENERAL)) {
			zmessageDebug(itmp, DSS_FUNCTION_javaNativeInterface_ID, "Null or invalid E part given for zirbeg ", "");
			if (strlen(ePart) > 0) zmessageDebug(itmp, DSS_FUNCTION_javaNativeInterface_ID, "E part given: ", ePart);
		}
	}

    (*env)->ReleaseStringUTFChars (env, j_ePart, ePart);
    (*env)->ReleaseIntArrayElements (env, j_year, year, 0);
    (*env)->ReleaseIntArrayElements (env, j_month, month, 0);
    (*env)->ReleaseIntArrayElements (env, j_day, day, 0);
    (*env)->ReleaseIntArrayElements (env, j_block, block, 0);
    (*env)->ReleaseIntArrayElements (env, j_minutesInBlock, minutesInBlock, 0);
    (*env)->ReleaseIntArrayElements (env, j_incrementBlock, incrementBlock, 0);
}
