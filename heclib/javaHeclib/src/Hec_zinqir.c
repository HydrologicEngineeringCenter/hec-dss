#include <jni.h>
#include <string.h>
#include "heclib.h"
#include <stdio.h>


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zinqir
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_param,
     jobject j_alpha, jintArray j_number)
{

    int *ifltab;
    const char *param;
    int *number;

    char alpha[300];
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;
    int i;
	int len;


	if (!j_ifltab) return;
	if (!j_param) return;
	if (!j_number) return;
	
    ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);
    param  = (*env)->GetStringUTFChars (env, j_param, 0);
    number = (*env)->GetIntArrayElements (env, j_number, 0);

	if (!ifltab) return;
	if (!param) return;
	if (!number) return;

	int dss_version = zgetVersion((long long*)ifltab);
	len = (int)strnlen_hec(param, 4);
	if ((dss_version == 0) && (len == 4) && (!zstringCompare(param, "vers", 4))) {
		number[0] = 7;
		cls = (*env)->GetObjectClass(env, j_alpha);
		fid = (*env)->GetFieldID(env, cls, "string", "Ljava/lang/String;");
		if (fid != 0) {
			jstr = (*env)->NewStringUTF(env, zdssVals.czVersion);
			(*env)->SetObjectField(env, j_alpha, fid, jstr);
			(*env)->DeleteLocalRef(env, jstr);
		}
	}	
	else {

		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zinqir; param: ", param);
		}

		for (i = 0; i < (int)sizeof(alpha); i++) {
			alpha[i] = ' ';
		}
		alpha[299] = '\0';

		if ((dss_version == 7) || (!strcmp(param, "vers"))) {
			zinquireChar ((long long*)ifltab, param, alpha, sizeof(alpha)-1, number);
			/* Set the alpha return */
			cls = (*env)->GetObjectClass (env, j_alpha);
			fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
			if (fid != 0) {			
				jstr = (*env)->NewStringUTF(env, alpha);
				(*env)->SetObjectField (env, j_alpha, fid, jstr);
				(*env)->DeleteLocalRef(env, jstr);
			}
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zinqir vers 7 return; alpha: ", alpha);
				zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zinqir return; number: ", number[0]);
			}
		}
		else if ((dss_version == 6) || (strcmp(param, "vers"))) {
			zinqir_((long long*)ifltab, param, alpha, number, strlen(param), sizeof(alpha) - 1);
			/* Set the alpha return */
			cls = (*env)->GetObjectClass(env, j_alpha);
			fid = (*env)->GetFieldID(env, cls, "string", "Ljava/lang/String;");
			if (fid != 0) {
				chrlnb_(alpha, &n, sizeof(alpha) - 1);
				if ((n < 0) || (n > (sizeof(alpha) - 1))) n = 0;
				alpha[n] = '\0';
				jstr = (*env)->NewStringUTF(env, alpha);
				(*env)->SetObjectField(env, j_alpha, fid, jstr);
				(*env)->DeleteLocalRef(env, jstr);
			}
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zinqir vers 6 return; alpha: ", alpha);
				zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zinqir return; number: ", number[0]);
			}
		}
		else {
			printf("zinquir called without open file\n");
		}
	}

	(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_param, param);
    (*env)->ReleaseIntArrayElements (env, j_number, number, 0);
}
