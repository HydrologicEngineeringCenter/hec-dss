#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1getime
    (JNIEnv *env, jobject obj, jstring j_cline, jintArray j_juls,
     jintArray j_istime, jintArray j_jule, jintArray j_ietime,
     jintArray j_status)

{
    const char *cline;
    int *juls;
    int *istime;
    int *jule;
    int *ietime;
    int *status;

    int one;
    int length;

    cline    = (*env)->GetStringUTFChars (env, j_cline, 0);
    juls     = (*env)->GetIntArrayElements (env, j_juls, 0);
    istime   = (*env)->GetIntArrayElements (env, j_istime, 0);
    jule     = (*env)->GetIntArrayElements (env, j_jule, 0);
    ietime   = (*env)->GetIntArrayElements (env, j_ietime, 0);
    status   = (*env)->GetIntArrayElements (env, j_status, 0);

    one = 1;
    length = (int)strlen (cline);

    getime_(cline, &one, &length, juls, istime, jule, ietime, status, strlen(cline));


    (*env)->ReleaseStringUTFChars (env, j_cline, cline);
    (*env)->ReleaseIntArrayElements (env, j_juls, juls, 0);
    (*env)->ReleaseIntArrayElements (env, j_istime, istime, 0);
    (*env)->ReleaseIntArrayElements (env, j_jule, jule, 0);
    (*env)->ReleaseIntArrayElements (env, j_ietime, ietime, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
}
