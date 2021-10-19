#include <jni.h>
#include <string.h>
#include "heclib.h"

///   FIX ME - version 6 only!!!!

JNIEXPORT jstring JNICALL Java_hec_heclib_util_Heclib_Hec_1zpseudorts
    (JNIEnv *env, jobject obj, jstring j_fromPath,  
     jintArray j_interval, jint j_action)
{
    int *interval;
    const char *fromPath;
	int action[1];
    int status[1];
    char coutpath[392];
	int n;

	fromPath = (const char *) (*env)->GetStringUTFChars (env, j_fromPath, 0);
    interval = (*env)->GetIntArrayElements (env, j_interval, 0);
    action[0] = (int)j_action;  

	//  zpseudorts (CINPATH, COUTPATH, INTLPS, IACTION, ISTATUS)
    zpseudorts6_ (fromPath, coutpath, interval, action, status,
				strlen(fromPath), sizeof(coutpath)-1);
    	
	(*env)->ReleaseIntArrayElements (env, j_interval, interval, 0); 
	chrlnb_ (coutpath, &n, sizeof(coutpath)-1);
	if ((n < 0) || (n > (sizeof(coutpath)-1))) n = 0;
    coutpath[n] = '\0';
	return (*env)->NewStringUTF (env, coutpath);   
}
