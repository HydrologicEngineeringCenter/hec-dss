#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zstxta
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jstring j_charString, jintArray j_userHeader, 
     jint j_nUserHeader, jintArray j_status)
{

	/*  Note - stores only a single string, not an array! */
	
    int *ifltab;
	const char *pathname;
	const char *charString;    
    int  *userHeader;
	int    nUserHeader;
    int  *status;

	int one=1;   

    ifltab      = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname    = (*env)->GetStringUTFChars   (env, j_pathname, 0);
    charString   = (*env)->GetStringUTFChars   (env, j_charString, 0);  
    userHeader  = (*env)->GetIntArrayElements (env, j_userHeader, 0);
	nUserHeader = (int) j_nUserHeader;
    status      = (*env)->GetIntArrayElements (env, j_status, 0);

   zstxta_ ((long long*)ifltab, pathname, charString, &one,
            userHeader, &nUserHeader, status,
            strlen (pathname), strlen (charString));

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars   (env, j_pathname, pathname);
    (*env)->ReleaseStringUTFChars   (env, j_charString, charString);        
    (*env)->ReleaseIntArrayElements (env, j_userHeader, userHeader, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
}
