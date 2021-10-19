#include <jni.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1jliymd
  (JNIEnv *env, jobject obj, jint j_julian, jintArray j_yearMonthDay)
{
    int *yearMonthDay;
    int year;
    int month;
    int day;
    int julian;


    yearMonthDay = (*env)->GetIntArrayElements (env, j_yearMonthDay, 0);
    julian = (int) j_julian;

    jliymd_ (&julian, &year, &month, &day);

    yearMonthDay[0] = year;
    yearMonthDay[1] = month;
    yearMonthDay[2] = day;

    (*env)->ReleaseIntArrayElements (env, j_yearMonthDay, yearMonthDay, 0);
}
