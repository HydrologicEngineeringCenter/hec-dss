#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1CloseLogFile
(JNIEnv* env, jobject obj) {

  zcloseLog();
}