#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zrenam
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
	jstring j_newPathname)
{

  int*  ifltab      = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	const char* pathname = (*env)->GetStringUTFChars (env, j_pathname, 0);
	const char* newPathname = (*env)->GetStringUTFChars (env, j_newPathname, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zrename.", "");
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "    Old Pathname: ", pathname);
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "    New Pathname: ", newPathname);
	}
	
	int status = zrename((long long*)ifltab, pathname, newPathname);
 
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Completed rename, status: ", status);
	}

  /* Release */
  (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);    
  (*env)->ReleaseStringUTFChars (env, j_pathname, pathname); 
	(*env)->ReleaseStringUTFChars (env, j_newPathname, newPathname);

	return status;
  
}
