#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zgetMessageLevel
  (JNIEnv *env, jobject obj, jint j_methodID)
{

	int methodID;

	methodID = (int)j_methodID;

	return zgetMessageLevel(methodID);
}
