#include <jni.h>
#include <string.h>
#include "heclib.h"


//  Returns standard interval in SECONDS (not minutes)
//
JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1ztsGetStandardInterval
    (JNIEnv *env, jobject obj, jint dssVersion, jintArray j_interval, jobject j_ePart, jintArray j_status)
{
    int *interval;
	int vers;
    const char *ePart;
    int *status;
    char cEpart[MAX_PART_SIZE];

	vers     = (int)dssVersion;
    interval = (*env)->GetIntArrayElements (env, j_interval, 0);
    ePart    = (*env)->GetStringUTFChars (env, j_ePart, 0);
    status   = (*env)->GetIntArrayElements (env, j_status, 0);

	stringCopy(cEpart, 65, ePart, strlen(ePart));

	ztsGetStandardInterval(vers, interval, cEpart, sizeof(cEpart)-1, status);

    (*env)->ReleaseIntArrayElements (env, j_interval, interval, 0);
    (*env)->ReleaseStringUTFChars (env, j_ePart, ePart);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
}

//  deprecated.  Use ztsGetStandardInterval instead.
JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1ztsGetStandardInterval7
    (JNIEnv *env, jobject obj, jintArray j_interval, jobject j_ePart, jintArray j_status)
{
    int *interval;
    const char *ePart;
    int *status;
    char cEpart[MAX_PART_SIZE];

    interval = (*env)->GetIntArrayElements (env, j_interval, 0);
    ePart    = (*env)->GetStringUTFChars (env, j_ePart, 0);
    status   = (*env)->GetIntArrayElements (env, j_status, 0);

	stringCopy(cEpart, MAX_PART_SIZE, ePart, strlen(ePart));

	*interval *= SECS_IN_1_MINUTE;
	ztsGetStandardInterval(7, interval, cEpart, sizeof(cEpart)-1, status);
	*interval /= SECS_IN_1_MINUTE;

    (*env)->ReleaseIntArrayElements (env, j_interval, interval, 0);
    (*env)->ReleaseStringUTFChars (env, j_ePart, ePart);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
}
