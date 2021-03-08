#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zloadcache
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jintArray j_istatus)
{ 
    int *ifltab;
	int *istatus;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	istatus = (*env)->GetIntArrayElements (env, j_istatus, 0);


	zloadcache_ ((long long*)ifltab, istatus);


    /* Release the file table */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
	(*env)->ReleaseIntArrayElements (env, j_istatus, istatus, 0);
}
