#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zcopyfile
    (JNIEnv *env, jobject obj, jintArray j_ifltabFrom, jintArray j_ifltabTo)   
{
	int *ifltabFrom;
    int *ifltabTo; 
	int istat;

    ifltabFrom   = (*env)->GetIntArrayElements (env, j_ifltabFrom, 0);
	ifltabTo     = (*env)->GetIntArrayElements (env, j_ifltabTo, 0);     

	zcopyfile_ ((long long*)ifltabFrom, (long long*)ifltabTo, &istat);
 
    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltabFrom, ifltabFrom, 0);
    (*env)->ReleaseIntArrayElements (env, j_ifltabTo, ifltabTo, 0);   

    return istat;  
}
