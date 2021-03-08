#include <jni.h>
#include <string.h>
#include "heclib.h"

//  (Note version 6 spelling, version 7 call)
JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zundel
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname)	
{

	int *ifltab;   
    const char *pathname;	
	int status;


    ifltab   = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname = (*env)->GetStringUTFChars (env, j_pathname, 0);

	status = zundelete((long long*)ifltab, pathname);

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);    
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);    

    return (jint)status; 
}
