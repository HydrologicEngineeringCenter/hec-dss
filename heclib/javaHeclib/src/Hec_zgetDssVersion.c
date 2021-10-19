#include <jni.h>
#include "heclib.h"


JNIEXPORT int JNICALL Java_hec_heclib_util_Heclib_Hec_1zgetDssVersion
    (JNIEnv *env, jobject obj, jintArray j_ifltab)
{
    int *ifltab;
	int iver;

    ifltab	= (*env)->GetIntArrayElements(env, j_ifltab, 0);
    iver =  zgetVersion((long long*)ifltab);

    /* Release the file table */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);

	return (jint)iver;
}
