#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zrenam
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
	jstring j_newPathname)
{

	int *ifltab;   
    const char *pathname;
	const char *newPathname;
	
	int npath;
	int nnewpath;
	int found=0;


    ifltab      = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname    = (*env)->GetStringUTFChars (env, j_pathname, 0);
	newPathname = (*env)->GetStringUTFChars (env, j_newPathname, 0);

	npath    = (int)strlen(pathname);
	nnewpath = (int)strlen(newPathname);
   
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zrename.", "");
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "    Old Pathname: ", pathname);
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "    New Pathname: ", newPathname);
	}
	
    zrenam_ ((long long*)ifltab, pathname, &npath, newPathname, &nnewpath,
	          &found, strlen(pathname), strlen(newPathname));	
 
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Completed rename, found: ", found);
	}

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);    
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname); 
	(*env)->ReleaseStringUTFChars (env, j_newPathname, newPathname);

    if (found==0)
		return -1;

	return 0;
  
}
