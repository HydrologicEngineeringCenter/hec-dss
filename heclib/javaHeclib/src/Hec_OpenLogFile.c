#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1openLogFile
(JNIEnv* env, jobject obj, jstring j_filename) {

  char* filename;

  filename = (char*)(*env)->GetStringUTFChars(env, j_filename, 0);
  if (filename == NULL) {
    return STATUS_NOT_OKAY;
  }

  int status = zopenLog(filename);

  (*env)->ReleaseStringUTFChars(env, j_filename, filename);

  return status;
}