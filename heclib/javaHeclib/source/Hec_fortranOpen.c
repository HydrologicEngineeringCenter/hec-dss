#include <string.h>
#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1fortranOpen
  (JNIEnv *env, jobject obj, jstring j_filename, jint j_unit) {

    int   unit;
    char *filename;
    int   status;

    filename = (char *) (*env)->GetStringUTFChars (env, j_filename, 0);
    unit = (int) j_unit;

	status = fortranopen_ (&unit, filename, strlen(filename));
    
    (*env)->ReleaseStringUTFChars (env, j_filename, filename);
    
    return status;
}
