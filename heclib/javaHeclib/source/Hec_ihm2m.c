#include <string.h>
#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1ihm2m
  (JNIEnv *env, jobject obj, jstring j_time)
{
    char *time;
    int minutes;

    time = (char *) (*env)->GetStringUTFChars (env, j_time, 0);

    minutes = ihm2m_ (time, strlen(time));

    (*env)->ReleaseStringUTFChars (env, j_time, time);

    return minutes;
}

