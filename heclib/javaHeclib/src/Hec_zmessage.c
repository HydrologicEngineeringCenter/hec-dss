#include <jni.h>
#include <string.h>
#include "heclib.h"

// send message to DSS log
JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zmessage
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_message)	
{

	int *ifltab;   
    const char *message;	

    ifltab   = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	message = (*env)->GetStringUTFChars (env, j_message, 0);

	zmessage((long long*)ifltab, message);

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);    
    (*env)->ReleaseStringUTFChars (env, j_message, message);

}
