#include <string.h>
#include <jni.h>
#include "heclibDate.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1datjul
  (JNIEnv *env, jobject obj, jstring j_date, jintArray j_julian) {

    int *julian;
    char *date;
    int isUndefined;

    date = (char *) (*env)->GetStringUTFChars (env, j_date, 0);
    julian = (*env)->GetIntArrayElements (env, j_julian, 0);

	*julian = dateToJulian(date);
    isUndefined = *julian == UNDEFINED_TIME;

    (*env)->ReleaseStringUTFChars (env, j_date, date);
    (*env)->ReleaseIntArrayElements (env, j_julian, julian, 0);
    jint status = -isUndefined;
    return status;
}
