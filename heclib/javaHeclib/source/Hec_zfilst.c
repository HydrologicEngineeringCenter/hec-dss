#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zfilst
    (JNIEnv *env, jobject obj, jintArray j_ifltab)
{
    int *ifltab;
	int iver;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);

	iver =  zgetVersion((long long*)ifltab);
	if (iver == 6) {
		zfilst6_ ((long long*)ifltab);
	}
	else {
		zprintFileInfo((long long*)ifltab);
	}

    /* Release the file table */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
}
