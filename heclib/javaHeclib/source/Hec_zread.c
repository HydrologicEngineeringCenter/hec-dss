#include <jni.h>
#include <string.h>
#include "heclib.h"
#include "hecdssFort.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zread
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jint j_npathname, jintArray j_header, jintArray j_nheader,
     jintArray j_data, jintArray j_ndata, jint j_plan, jintArray j_exists) {


    int *ifltab;
    int    npathname;
    int  *header;
    int  *nheader;
    int  *data;
    int  *ndata;
    int    plan;
    int  *exists;

    const char *pathname;


    ifltab    = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname  = (*env)->GetStringUTFChars (env, j_pathname, 0);
    npathname = (int) j_npathname;
    header    = (*env)->GetIntArrayElements (env, j_header, 0);
    nheader   = (*env)->GetIntArrayElements (env, j_nheader, 0);
    data      = (*env)->GetIntArrayElements (env, j_data, 0);
    ndata     = (*env)->GetIntArrayElements (env, j_ndata, 0);
    plan      = (int) j_plan;
    exists    = (*env)->GetIntArrayElements (env, j_exists, 0);

    zread_ ((long long*)ifltab, pathname, &npathname, header, nheader,
            data, ndata, &plan, exists, strlen (pathname));

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_header, header, 0);
    (*env)->ReleaseIntArrayElements (env, j_nheader, nheader, 0);
    (*env)->ReleaseIntArrayElements (env, j_data, data, 0);
    (*env)->ReleaseIntArrayElements (env, j_ndata, ndata, 0);
    (*env)->ReleaseIntArrayElements (env, j_exists, exists, 0);

}
