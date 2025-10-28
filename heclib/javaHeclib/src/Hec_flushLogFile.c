#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1flushLogFile
(JNIEnv* env, jobject obj) {

	if (zdssVals.messageHandle <= 0) {
		return -1;
	}
	int status = flushFile(zdssVals.messageHandle);

	return status;

}