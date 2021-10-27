#include <jni.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1inctim
    (JNIEnv *env, jobject obj, jint j_intl, jint j_period,
     jint j_juls, jint j_istime, jintArray j_jule, jintArray j_ietime)
{

    int   intl;
    int   flag;
    int   period;
    int   juls;
    int   istime;
    int *jule;
    int *ietime;

    intl    = (int) j_intl;
    flag    = 0;
    period  = (int) j_period;
    juls    = (int) j_juls;
    istime  = (int) j_istime;
    jule    = (*env)->GetIntArrayElements (env, j_jule, 0);
    ietime  = (*env)->GetIntArrayElements (env, j_ietime, 0);

    inctim_ (&intl, &flag, &period, &juls, &istime, jule,
             ietime);

    (*env)->ReleaseIntArrayElements (env, j_jule, jule, 0);
    (*env)->ReleaseIntArrayElements (env, j_ietime, ietime, 0);
}
