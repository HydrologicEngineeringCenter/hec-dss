#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zupath
    (JNIEnv *env, jobject obj, jstring j_pathname, jintArray j_begPart,
     jintArray j_endPart, jintArray j_lenPart, jintArray j_status)
{
    const char *pathname;
    int *begPart;
    int *endPart;
    int *lenPart;
    int *status;

    pathname = (const char *) (*env)->GetStringUTFChars (env, j_pathname,  0);
    begPart = (*env)->GetIntArrayElements (env, j_begPart, 0);
    endPart = (*env)->GetIntArrayElements (env, j_endPart, 0);
    lenPart = (*env)->GetIntArrayElements (env, j_lenPart, 0);
    status  = (*env)->GetIntArrayElements (env, j_status,  0);

    zupath_ (pathname, begPart, endPart, lenPart,
             status, strlen(pathname));

    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_begPart, begPart, 0);
    (*env)->ReleaseIntArrayElements (env, j_endPart, endPart, 0);
    (*env)->ReleaseIntArrayElements (env, j_lenPart, lenPart, 0);
    (*env)->ReleaseIntArrayElements (env, j_status,  status,  0);

}
