#include <jni.h>
#include <string.h>
#include "heclib.h"
#include "jni_utility.h"
#include <stdio.h>


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zinqir
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_param,
     jobject j_alpha, jintArray j_number)
{
	char alpha[300] = { 0 };

	if (!j_ifltab) return;
	if (!j_param) return;
	if (!j_number) return;
	
  int* ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);
  const char* param  = (*env)->GetStringUTFChars (env, j_param, 0);
  int* number = (*env)->GetIntArrayElements (env, j_number, 0);

	if (!ifltab) return;
	if (!param) return;
	if (!number) return;

	

		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zinqir; param: ", param);
		}

		zinquireChar((long long*)ifltab, param, alpha, sizeof(alpha)-1, number);
		jclass cls = (*env)->GetObjectClass(env, j_alpha);
		hec_dss_jni_setStringField(env, cls, j_alpha, "string", alpha);

			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zinqir vers 7 return; alpha: ", alpha);
				zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zinqir return; number: ", number[0]);
			}
		

  	(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_param, param);
    (*env)->ReleaseIntArrayElements (env, j_number, number, 0);
}
