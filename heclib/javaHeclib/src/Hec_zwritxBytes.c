#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zwritxBytes
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jint j_npathname, jintArray j_intheader, jint j_nintheader,
     jintArray j_compheader, jint j_ncompheader,
     jintArray j_userheader, jint j_nuserheader,
     jbyteArray j_data, jint j_ndata,
     jint j_type, jint j_plan, jintArray j_status, jintArray j_exists) {


    int *ifltab;
    int    npathname;
    int  *intheader;
    int    nintheader;
    int  *compheader;
    int    ncompheader;
    int  *userheader;
    int    nuserheader;
    char   *data;
    int    ndata;
    int    type;
    int    plan;
    int  *status;
    int  *exists;

    const char *pathname;

    ifltab      = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname    = (*env)->GetStringUTFChars (env, j_pathname, 0);
    npathname   = (int) j_npathname;
    intheader   = (*env)->GetIntArrayElements (env, j_intheader, 0);
    nintheader  = (int) j_nintheader;
    compheader  = (*env)->GetIntArrayElements (env, j_compheader, 0);
    ncompheader = (int) j_ncompheader;
    userheader  = (*env)->GetIntArrayElements (env, j_userheader, 0);
    nuserheader = (int) j_nuserheader;
    data        = (*env)->GetByteArrayElements (env, j_data, 0);
    ndata       = (int) j_ndata;
    type        = (int) j_type;
    plan        = (int) j_plan;
    status      = (*env)->GetIntArrayElements (env, j_status, 0);
    exists      = (*env)->GetIntArrayElements (env, j_exists, 0);

	/*  Determine if this is is to be written on a big endian machine
	and the bytes in the words need to be swapped */
	

    zwritex((long long*)ifltab, pathname, &npathname,
             intheader, &nintheader,
             compheader, &ncompheader,
             userheader, &nuserheader,
             (int *)data, &ndata, &type, &plan,
             status, exists);


    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_intheader, intheader, 0);
    (*env)->ReleaseIntArrayElements (env, j_compheader, compheader, 0);
    (*env)->ReleaseIntArrayElements (env, j_userheader, userheader, 0);
    (*env)->ReleaseByteArrayElements (env, j_data, data, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
    (*env)->ReleaseIntArrayElements (env, j_exists, exists, 0);

}