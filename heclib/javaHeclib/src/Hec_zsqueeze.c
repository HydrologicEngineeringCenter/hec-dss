#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1squeezeDSS
  (JNIEnv *env, jobject obj, jstring j_filename) {

   const char* filename = (*env)->GetStringUTFChars (env, j_filename, 0); 
	 int status = zsqueeze(filename);
	(*env)->ReleaseStringUTFChars (env, j_filename, filename);
  return status;
}
