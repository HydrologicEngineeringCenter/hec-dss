#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zbegdt
    (JNIEnv *env, jobject obj, jint j_julian, jint j_interval,
     jintArray j_year, jintArray j_month, jintArray j_day,
     jintArray j_block, jint j_dssVersion)
{
    int julian;
    int interval;
    int *year;
    int *month;
    int *day;
    int *block;
    int dssVersion;

	int jul;

    julian         = j_julian;
    interval       = j_interval;
    year           = (*env)->GetIntArrayElements (env, j_year, 0);
    month          = (*env)->GetIntArrayElements (env, j_month, 0);
    day            = (*env)->GetIntArrayElements (env, j_day, 0);
    block          = (*env)->GetIntArrayElements (env, j_block, 0);
    dssVersion     = j_dssVersion;
	interval *= SECS_IN_1_MINUTE;

	jul = ztsRegGetBlockStart(julian, interval, block);
	julianToYearMonthDay(jul, year, month, day);

    //zbegdt_ (&julian, &interval, year, month, day, block, &dssVersion);

    (*env)->ReleaseIntArrayElements (env, j_year, year, 0);
    (*env)->ReleaseIntArrayElements (env, j_month, month, 0);
    (*env)->ReleaseIntArrayElements (env, j_day, day, 0);
    (*env)->ReleaseIntArrayElements (env, j_block, block, 0);
}
