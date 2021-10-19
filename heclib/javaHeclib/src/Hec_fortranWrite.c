#include <string.h>
#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1fortranWrite
  (JNIEnv *env, jobject obj, jstring j_string, jint j_unit) {

    int   unit;
    char *string;
    int   status;

    string = (char *) (*env)->GetStringUTFChars (env, j_string, 0);
    unit = (int) j_unit;

	status = fortranwrite_(&unit, string, strlen(string));
    
    (*env)->ReleaseStringUTFChars (env, j_string, string);
    
    return status;
}
