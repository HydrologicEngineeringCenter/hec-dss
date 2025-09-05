#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1FlushLogFile
(JNIEnv* env, jobject obj) {

	if (zdssVals.messageHandle <= 0) {
		return;
	}
	flushFile(zdssVals.messageHandle);

}