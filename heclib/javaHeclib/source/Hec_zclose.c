#include <jni.h>
#include <stdio.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zclose
    (JNIEnv *env, jobject obj, jintArray j_ifltab)
{
    long long *ifltab;

    ifltab = (long long *)(*env)->GetIntArrayElements (env, j_ifltab, 0);

	if (zmessageLevel(ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_1zclose, Enter; handle: ", zhandle(ifltab));
	}
	
	zclose(ifltab);

    /* Release the file table */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, (jint *)ifltab, 0);
}
