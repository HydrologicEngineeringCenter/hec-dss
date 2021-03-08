#include <jni.h>
#include "heclib.h"


JNIEXPORT jstring JNICALL Java_hec_heclib_util_Heclib_Hec_1getEPartFromInterval
    (JNIEnv *env, jobject obj, jint j_interval, jintArray j_status)
{
    int interval;
    int *status;

    int nvals;
    int n;
    char epart[70];

    interval = j_interval;
    status   = (*env)->GetIntArrayElements (env, j_status, 0);
    status[0] = 2;

    zgintl_ (&interval, epart, &nvals, status, sizeof(epart)-1); 

    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

    chrlnb_ (epart, &n, sizeof(epart)-1);
	if ((n < 0) || (n > (sizeof(epart)-1))) n = 0;
    epart[n] = '\0';
    return (*env)->NewStringUTF (env, epart);
}
