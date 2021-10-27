#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zckmul
    (JNIEnv *env, jobject obj, jintArray j_ifltab)
{
    int *ifltab;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);

	if (zgetVersion((long long*)ifltab) == 7) {
		zcheckMultiUser((long long*)ifltab);
	}
	else {
		zckmul6_ ((long long*)ifltab);
	}

	/* Release the file table */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
}
