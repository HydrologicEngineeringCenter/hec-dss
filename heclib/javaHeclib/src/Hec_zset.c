#include <jni.h>
#include <string.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zset
    (JNIEnv *env, jobject obj, jstring j_parameter, jstring j_alpha,
     jint j_number)
{
    const char *parameter;
    const char *alpha;
    int number;

	long long idum[1];

    parameter = (*env)->GetStringUTFChars (env, j_parameter, 0);
    alpha     = (*env)->GetStringUTFChars (env, j_alpha, 0);
    number    = (int) j_number;

	//zsetMessageLevel(MESS_METHOD_GLOBAL_ID, MESS_LEVEL_USER_DIAG); 
	//zsetMessageLevel(MESS_METHOD_GLOBAL_ID, MESS_LEVEL_GENERAL);
	//zsetMessageLevel(MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_2);
//	zsetMessageLevel(MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1);
	//zset("MLVL", "", 4);

    
    if (strncmp(parameter, "MLEVEL", 6) == 0) { // match
        char* jni_message_level = getenv("DSS_JNI_MESSAGE_LEVEL");
        if (jni_message_level) // override the message level request.
        {
            number = atoi(jni_message_level);
        }
    }

	idum[0] = 0;
	if (zmessageLevel(idum, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug((long long*)idum, DSS_FUNCTION_javaNativeInterface_ID, "enter Heclib_Hec_zset, parameter: ", parameter);
		zmessageDebugInt((long long*)idum, DSS_FUNCTION_javaNativeInterface_ID, " Heclib_Hec_zset, number: ", number);
	}

//	if (strncmp(parameter, "MLE", 3))
    zset_(parameter, alpha, &number, strlen(parameter), strlen(alpha));

	(*env)->ReleaseStringUTFChars (env, j_parameter, parameter);
    (*env)->ReleaseStringUTFChars (env, j_alpha, alpha);

}
