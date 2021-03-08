#include <jni.h>
#include <string.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1StringHol
  (JNIEnv *env, jobject obj, jstring j_str, jint j_length, jintArray j_hol) {

    int     *hol;
    int     length;
    int     one;

    const char *str;

    str = (*env)->GetStringUTFChars (env, j_str, 0);
    length = (int) j_length;
    hol = (*env)->GetIntArrayElements (env, j_hol, 0);

    one = 1;

    chrhol_ (str, &one, &length, hol, &one, strlen(str));

    (*env)->ReleaseStringUTFChars (env, j_str, str);
    (*env)->ReleaseIntArrayElements (env, j_hol, hol, 0);
}
