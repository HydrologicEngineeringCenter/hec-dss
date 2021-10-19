#include <jni.h>

#include "heclib.h"
#include "zerrorCodes.h"
#include "hecdssInternal.h"



JNIEXPORT int JNICALL Java_hec_heclib_util_Heclib_Hec_1zcheckFile
    (JNIEnv *env, jobject obj, jintArray j_ifltab)
{
    int *ifltab;
	int ierr;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    ierr = zcheckFile((long long*)ifltab);   
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
	return ierr;
}
