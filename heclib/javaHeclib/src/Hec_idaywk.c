#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1idaywk
  (JNIEnv *env, jobject obj, jint j_julian)
{
    int julian;
    julian = (int) j_julian;

    return idaywk_ (&julian);
}
