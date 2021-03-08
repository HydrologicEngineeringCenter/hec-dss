//#include <io.h>
#include <jni.h>
#include <string.h>
#include "heclib.h"
#include <stdio.h>


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zcatalogFile
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_catalogName, jint j_boolSorted,jstring j_pathWithWild)
{

    int *ifltab;
    const char *catalogName;
	const char *pathWithWild;
    int status;
	int boolSorted;

//	printf("Enter Java_hec_heclib_util_Heclib_Hec_1zcatalogFile\n ");
    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);     
	catalogName  = (*env)->GetStringUTFChars (env, j_catalogName, 0);
	if (j_boolSorted == 1) {
		boolSorted = 1;
	}
	else {
		boolSorted = 0;
	}
	if (j_pathWithWild) {
		pathWithWild  = (*env)->GetStringUTFChars (env, j_pathWithWild, 0);
	}
	else {
		pathWithWild = (const char *)0;
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zcatalogFile ", "");
	}

	status = zcatalogFile((long long *)ifltab, catalogName, boolSorted, pathWithWild);

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_catalogName, catalogName);
	if (j_pathWithWild) {
		(*env)->ReleaseStringUTFChars (env, j_pathWithWild, pathWithWild);
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_zcatalogFile, status: ", status);
	}

    return (jint)status;
}
