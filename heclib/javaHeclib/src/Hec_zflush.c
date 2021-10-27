#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zflush
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jint j_forceFlush)
{
    int *ifltab;
	int forceFlush;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	forceFlush = (int)j_forceFlush;

  
	if (zgetVersion((long long*)ifltab) == 7) {
		zflushToDisk ((long long*)ifltab, forceFlush);
		zmessageFlush((long long*)ifltab);
	}

    /* Release the file table */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
}
