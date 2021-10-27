#include <jni.h>
#include <ctype.h>
#include <string.h>

#include "heclib.h"
#include "zprogress.h"


/*
*	Inquire for a parameter that returns a number 
*	See zinqir for returning a string (and number)
*/
JNIEXPORT jlong JNICALL Java_hec_heclib_util_Heclib_Hec_1zinquire
			(JNIEnv *env, 
			jobject obj, 
			jintArray j_ifltab, 
			jstring j_param)
{

    int *ifltab;
    const char *param;   
	long long answer;

	param = (*env)->GetStringUTFChars(env, j_param, 0);

    ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);
    
//	printf("Enter Java_hec_heclib_util_Heclib_Hec_1zinquire,  param %s\n ", param);
	answer = zinquire ((long long*)ifltab, param);

	
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zinquire; param: ", param);
		zmessageDebugLong((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zinqir return; number: ", answer);
	}
	
	(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_param, param);
	return (jlong)answer;
}
