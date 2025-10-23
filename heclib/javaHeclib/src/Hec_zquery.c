#include <jni.h>
#include <string.h>
#include "heclib.h"
#include <stdio.h>
#include "jni_utility.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zquery
    (JNIEnv *env, jobject obj, jstring j_param,
     jobject j_alpha, jintArray j_number)
{
    #define SIZE_ALPHA 200
    const char *param;
    int *number;

    char alpha[SIZE_ALPHA];
    jclass cls;

    param  = (*env)->GetStringUTFChars (env, j_param, 0);
    number = (*env)->GetIntArrayElements (env, j_number, 0);

    zquery(param, alpha, SIZE_ALPHA, number);

    (*env)->ReleaseStringUTFChars (env, j_param, param);
    (*env)->ReleaseIntArrayElements (env, j_number, number, 0);

    
    cls = (*env)->GetObjectClass (env, j_alpha);
    hec_dss_jni_setStringField(env, cls, j_alpha,"string", alpha);
    
}
