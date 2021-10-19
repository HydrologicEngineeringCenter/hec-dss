#include <jni.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1systim
  (JNIEnv *env, jobject obj, jintArray j_julian, jintArray j_seconds)
{
    int  *julian, *seconds;

    julian  = (*env)->GetIntArrayElements (env, j_julian, 0);
    seconds = (*env)->GetIntArrayElements (env, j_seconds, 0);

    systim_ (julian, seconds);

    (*env)->ReleaseIntArrayElements (env, j_julian,  julian, 0);
    (*env)->ReleaseIntArrayElements (env, j_seconds, seconds, 0);

}
