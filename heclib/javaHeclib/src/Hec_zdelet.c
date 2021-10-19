#include <jni.h>
#include <string.h>
#include "heclib.h"

//  (Note version 6 spelling, version 7 call)
JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zdelet
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname)	
{

	int *ifltab;   
    const char *pathname;	
	int status;


    ifltab   = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname = (*env)->GetStringUTFChars (env, j_pathname, 0);

	status = zdelete((long long*)ifltab, pathname);

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);    
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);    

	//  For compatibility, version 6 returned "true" (1) when deleted
	if (status == STATUS_OKAY) {
		return (jint)1;
	}
    return (jint)status; 
}
