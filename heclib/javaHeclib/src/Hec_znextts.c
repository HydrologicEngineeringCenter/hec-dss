#include <jni.h>
#include <string.h>
#include "heclib.h"
#include <stdio.h>


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1znextts
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jobject j_nextPath, jint j_lforward, jintArray j_status)
{

    int *ifltab;
    int *status;
    const char *pathname;
    int  lforward;

    char nextPath[392];
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;
    int i;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname  = (*env)->GetStringUTFChars (env, j_pathname, 0);
    lforward = (int)j_lforward;
    status = (*env)->GetIntArrayElements (env, j_status, 0);

    for (i=0; i<390; i++)
        nextPath[i] = ' ';
        nextPath[391] = '\0';

    znextts_ ((long long*)ifltab, pathname, nextPath, &lforward,
              status, strlen(pathname), sizeof(nextPath)-1);
  

    /* Set the nextPath return */
    cls = (*env)->GetObjectClass (env, j_nextPath);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (nextPath, &n, sizeof(nextPath)-1);
		if ((n < 0) || (n > (sizeof(nextPath)-1))) n = 0;
        nextPath[n] = '\0';
        jstr = (*env)->NewStringUTF(env, nextPath);
        (*env)->SetObjectField (env, j_nextPath, fid, jstr);
    }

	(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}
