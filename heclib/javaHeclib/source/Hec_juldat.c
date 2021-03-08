#include <jni.h>
#include "heclib.h"

JNIEXPORT jstring JNICALL Java_hec_heclib_util_Heclib_Hec_1juldat
  (JNIEnv *env, jobject obj, jint j_julian, jint j_style) {

    int julian, style, date_len;
    char date[50];

    julian = (int) j_julian;
    style = (int) j_style;
    juldat_ (&julian, &style, date, &date_len, sizeof(date)-1);
    date[date_len] = '\0';

    return (*env)->NewStringUTF (env, date);
}

