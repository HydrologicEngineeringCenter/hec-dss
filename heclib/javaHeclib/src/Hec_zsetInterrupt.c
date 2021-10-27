#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsetInterrupt
    (JNIEnv *env, jobject obj, jint j_handle)
{
    int handle;

    handle = (int) j_handle;
	zsetInterrupt(handle);
}
