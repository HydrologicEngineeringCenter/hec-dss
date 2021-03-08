#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zmaxpart
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jintArray j_maxpart)
{
    int *ifltab;
    int *maxpart;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    maxpart = (*env)->GetIntArrayElements (env, j_maxpart, 0);
	
	zmaxPart((long long*)ifltab, maxpart);


    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseIntArrayElements (env, j_maxpart, maxpart, 0);
}
