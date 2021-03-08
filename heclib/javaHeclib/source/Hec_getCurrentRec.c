#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1getCurrentRec
  (JNIEnv *env, jobject obj)
{
    int currentRec[1];

    getcurrentrec_(currentRec);

	return (jint) currentRec[0];

}
