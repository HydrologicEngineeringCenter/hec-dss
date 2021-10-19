#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zofset
    (JNIEnv *env, jobject obj, jintArray j_julian, jintArray j_minutes,
     jint j_interval, jint j_flag, jintArray j_offset)
{
    int *julian;
    int *minutes;
    int   interval;
    int   flag;
    int *offset;

    julian   = (*env)->GetIntArrayElements (env, j_julian, 0);
    minutes  = (*env)->GetIntArrayElements (env, j_minutes, 0);
    interval = (int) j_interval;
    flag     = (int) j_flag;
    offset   = (*env)->GetIntArrayElements (env, j_offset, 0);

    zofset_ (julian, minutes, &interval, &flag, offset);

    (*env)->ReleaseIntArrayElements (env, j_julian, julian, 0);
    (*env)->ReleaseIntArrayElements (env, j_minutes, minutes, 0);
    (*env)->ReleaseIntArrayElements (env, j_offset, offset, 0);
}
