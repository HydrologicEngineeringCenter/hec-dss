#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zrecadd
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jlongArray j_recAdds, jintArray j_status)
{
    int *ifltab;
    const char *path;
    long long *recAdds;
    int *status;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab,  0);
    path = (const char *) (*env)->GetStringUTFChars (env, j_pathname,  0);
    recAdds = (long long *)(*env)->GetLongArrayElements (env, j_recAdds,  0);
    status = (*env)->GetIntArrayElements (env, j_status,  0);

    zrecadd_ ((long long*)ifltab, path, recAdds, status, strlen(path));

	//SUBROUTINE ZRECADD ((long long*)ifltab, CPATH, ILADD, ISTAT)

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, path);
    (*env)->ReleaseLongArrayElements (env, j_recAdds, (jlong *)recAdds, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}

