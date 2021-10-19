#include <jni.h>
#include "heclib.h"


JNIEXPORT int JNICALL Java_hec_heclib_util_Heclib_Hec_1zgetProgress
    (JNIEnv *env, jobject obj, jint j_handle)
{
    int handle;

    handle = (int) j_handle;
	return (jint)zgetProgress(handle);
}
