#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zckpat
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jintArray j_nerror)
{
    int *ifltab;
	int *nerror;
	int iver;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	nerror = (*env)->GetIntArrayElements (env, j_nerror, 0);

	iver =  zgetVersion((long long*)ifltab);
	if (iver == 6) {
		zckpat6_ ((long long*)ifltab, nerror);
	}
	else {
		*nerror = zcheckPathnames((long long*)ifltab);
	}


    /* Release the file table */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
	(*env)->ReleaseIntArrayElements (env, j_nerror, nerror, 0);
}
