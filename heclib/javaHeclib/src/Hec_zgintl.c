#include <jni.h>
#include <string.h>
#include "heclib.h"

// Java: public static synchronized native void Hec_zgintl(int interval[], String ePart, int nvals[], int status[]);
// This method is similar to zgetInterval, except zgetInterval takes a stringContainer as input instead of String
JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zgintl
(JNIEnv* env, jobject obj, jintArray j_interval, jobject j_ePart,
    jintArray j_nvals, jintArray j_status)
{
    int* interval;
    const char* ePart;
    int* nvals;
    int* status;
    char cEpart[MAX_PART_SIZE] = "";

    interval = (*env)->GetIntArrayElements(env, j_interval, 0);
    ePart = (*env)->GetStringUTFChars(env, j_ePart, 0);
    nvals = (*env)->GetIntArrayElements(env, j_nvals, 0);
    status = (*env)->GetIntArrayElements(env, j_status, 0);

    if (strlen(ePart) >= MAX_PART_SIZE) {
        *status = -1;
    }
    else {

        stringCopy(cEpart, MAX_PART_SIZE, ePart, strlen(ePart));
        upcase_(cEpart, strlen(cEpart));

        zgintl_(interval, cEpart, nvals, status,
            sizeof(cEpart) - 1);
    }

    (*env)->ReleaseIntArrayElements (env, j_interval, interval, 0);
    (*env)->ReleaseStringUTFChars (env, j_ePart, ePart);
    (*env)->ReleaseIntArrayElements (env, j_nvals, nvals, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
}
