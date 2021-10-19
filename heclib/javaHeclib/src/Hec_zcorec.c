#include <jni.h>
#include <string.h>
#include "heclib.h"


//  Depreciated - use zcopyRecord instead

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zcorec
    (JNIEnv *env, jobject obj, jintArray j_ifltabFrom, jintArray j_ifltabTo,
	 jstring j_pathnameFrom, jstring j_pathnameTo,
     jintArray j_buffer1, jint j_buffer1Size, jintArray j_buffer2, jint j_buffer2Size)   
{

	int  *ifltabFrom;
    int  *ifltabTo;
    const char *pathnameFrom;
    const char *pathnameTo;
	int status;


    ifltabFrom   = (*env)->GetIntArrayElements (env, j_ifltabFrom, 0);
	ifltabTo     = (*env)->GetIntArrayElements (env, j_ifltabTo, 0);
    pathnameFrom = (*env)->GetStringUTFChars (env, j_pathnameFrom, 0);
    pathnameTo   = (*env)->GetStringUTFChars (env, j_pathnameTo, 0);

	if (zmessageLevel((long long*)ifltabFrom, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug((long long*)ifltabFrom, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zcorec.", "");
		zmessageDebug((long long*)ifltabFrom, DSS_FUNCTION_javaNativeInterface_ID, "   From Pathname: ", pathnameFrom);
		zmessageDebug((long long*)ifltabFrom, DSS_FUNCTION_javaNativeInterface_ID, "     To Pathname: ", pathnameTo);
	}

	if ((zhandle((long long*)ifltabFrom) == zhandle((long long*)ifltabTo)) ||
		(zhandle((long long*)ifltabTo) == 0)) {
		//  Java passes a copy of arrays... so if it is the same array, 
		//  they will be different at this point.  We have to make sure 
		//  we only pass one ifltab array!  (otherwise they will change separately)
		status = zduplicateRecord((long long*)ifltabFrom, pathnameFrom, pathnameTo);
	}
	else {
		status = zcopyRecord((long long*)ifltabFrom, (long long*)ifltabTo, pathnameFrom, pathnameTo);
	}

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltabFrom, ifltabFrom, 0);
    (*env)->ReleaseIntArrayElements (env, j_ifltabTo, ifltabTo, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathnameFrom, pathnameFrom);
    (*env)->ReleaseStringUTFChars (env, j_pathnameTo, pathnameTo);


    return (jint)status;
  
}
