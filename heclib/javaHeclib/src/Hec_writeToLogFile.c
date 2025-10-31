#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1writeToLogFile
(JNIEnv* env, jobject obj, jstring j_message) {

  long long ifltab[250];

  const char* message = (*env)->GetStringUTFChars(env, j_message, 0);
  if (message == NULL) {
    return STATUS_NOT_OKAY;
  }

	if (zdssVals.messageHandle <= 0) {
		return STATUS_NOT_OKAY;
	}

  zmessageLen(ifltab, message, strlen(message));

  (*env)->ReleaseStringUTFChars(env, j_message, message);

  return STATUS_OKAY;
}