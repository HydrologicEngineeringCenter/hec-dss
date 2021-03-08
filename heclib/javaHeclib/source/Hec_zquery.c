#include <jni.h>
#include <string.h>
#include "heclib.h"
#include <stdio.h>


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zquery
    (JNIEnv *env, jobject obj, jstring j_param,
     jobject j_alpha, jintArray j_number)
{

    const char *param;
    int *number;

    char alpha[200];
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;
    int i;

    param  = (*env)->GetStringUTFChars (env, j_param, 0);
    number = (*env)->GetIntArrayElements (env, j_number, 0);

    for (i=0; i<199; i++)
        alpha[i] = ' ';
        alpha[199] = '\0';

    zquery_ (param, alpha, number,
             strlen(param), sizeof(alpha)-1);

    (*env)->ReleaseStringUTFChars (env, j_param, param);
    (*env)->ReleaseIntArrayElements (env, j_number, number, 0);

    /* Set the alpha return */
    cls = (*env)->GetObjectClass (env, j_alpha);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (alpha, &n, sizeof(alpha)-1);
		if ((n < 0) || (n > (sizeof(alpha)-1))) n = 0;
        alpha[n] = '\0';
        jstr = (*env)->NewStringUTF(env, alpha);
        (*env)->SetObjectField (env, j_alpha, fid, jstr);
    }
}
