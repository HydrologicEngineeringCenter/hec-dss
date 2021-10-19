#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zincbk
    (JNIEnv *env, jobject obj, jintArray j_block, jintArray j_julian,
     jintArray j_year, jintArray j_month, jintArray j_day)
{
    int *block;
    int *julian;
    int *year;
    int *month;
    int *day;

    block  = (*env)->GetIntArrayElements (env, j_block, 0);
    julian = (*env)->GetIntArrayElements (env, j_julian, 0);
    year   = (*env)->GetIntArrayElements (env, j_year, 0);
    month  = (*env)->GetIntArrayElements (env, j_month, 0);
    day    = (*env)->GetIntArrayElements (env, j_day, 0);

    zincbk_ (block, julian, year, month, day);

    (*env)->ReleaseIntArrayElements (env, j_block, block, 0);
    (*env)->ReleaseIntArrayElements (env, j_julian, julian, 0);
    (*env)->ReleaseIntArrayElements (env, j_year, year, 0);
    (*env)->ReleaseIntArrayElements (env, j_month, month, 0);
    (*env)->ReleaseIntArrayElements (env, j_day, day, 0);
}
