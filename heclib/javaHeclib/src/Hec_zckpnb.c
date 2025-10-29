#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zckpnb
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jintArray j_nerror)
{

  int* ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	int* nerror = (*env)->GetIntArrayElements (env, j_nerror, 0);

	*nerror = zcheckHashTable ((long long*)ifltab);

    /* Release the file table */
  (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
	(*env)->ReleaseIntArrayElements (env, j_nerror, nerror, 0);
}
