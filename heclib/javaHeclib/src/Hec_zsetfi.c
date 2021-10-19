#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsetfi
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_parameter,
     jstring j_alpha, jint j_number, jintArray j_status)
{
    const char *parameter;
    const char *alpha;
    int *ifltab;
    int number;
    int *status;

    ifltab    = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    parameter = (*env)->GetStringUTFChars (env, j_parameter, 0);
    alpha     = (*env)->GetStringUTFChars (env, j_alpha, 0);
    number    = (int) j_number;
    status    = (*env)->GetIntArrayElements (env, j_status, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zsetfi, parameter: ", parameter);
	}

    zsetfi_ ((long long*)ifltab, parameter, alpha, &number, status,
             strlen(parameter), strlen(alpha));

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_parameter, parameter);
    (*env)->ReleaseStringUTFChars (env, j_alpha, alpha);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}
