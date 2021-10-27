#include <jni.h>
#include <stdlib.h>
#include "heclib.h"

JNIEXPORT jstring JNICALL Java_hec_heclib_util_Heclib_Hec_1holString
  (JNIEnv *env, jobject obj, jintArray j_hol, jint j_length) {

    int     *hol;
    int     length;
    int     one;
	 jstring jstr;

    char *str;

    hol  = (*env)->GetIntArrayElements (env, j_hol, 0);
    length = (int) j_length;
    
	str = (char*)malloc(length+1);
    one = 1;

    holchr_ (hol, &one, &length, str, &one, (size_t)length);

    (*env)->ReleaseIntArrayElements (env, j_hol, hol, 0);
    str[length] = '\0';

    jstr = (*env)->NewStringUTF (env, str);
	free(str);
	return jstr;
}
