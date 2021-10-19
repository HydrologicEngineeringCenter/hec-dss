#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zreadx
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jintArray j_intheader, jint j_kintheader, jintArray j_nintheader,
     jintArray j_compheader, jint j_kcompheader, jintArray j_ncompheader,
     jintArray j_userheader, jint j_kuserheader, jintArray j_nuserheader,
     jshortArray j_data, jint j_kdata, jintArray j_ndata,
     jint j_plan, jintArray j_exists) {


    int *ifltab;
    int  *intheader;
    int    kintheader;
    int  *nintheader;
    int  *compheader;
    int    kcompheader;
    int  *ncompheader;
    int  *userheader;
    int    kuserheader;
    int  *nuserheader;
    short *data;
    int    kdata;
    int  *ndata;
    int    plan;
    int  *exists;

    const char *pathname;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab      = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname    = (*env)->GetStringUTFChars (env, j_pathname, 0);
    intheader   = (*env)->GetIntArrayElements (env, j_intheader, 0);
    kintheader  = (int) j_kintheader;
    nintheader  = (*env)->GetIntArrayElements (env, j_nintheader, 0);
    compheader  = (*env)->GetIntArrayElements (env, j_compheader, 0);
    kcompheader = (int) j_kcompheader;
    ncompheader = (*env)->GetIntArrayElements (env, j_ncompheader, 0);
    userheader  = (*env)->GetIntArrayElements (env, j_userheader, 0);
    kuserheader = (int) j_kuserheader;
    nuserheader = (*env)->GetIntArrayElements (env, j_nuserheader, 0);
    data        = (*env)->GetShortArrayElements (env, j_data, 0);
    kdata       = (int) j_kdata;
    ndata       = (*env)->GetIntArrayElements (env, j_ndata, 0);
    plan        = (int) j_plan;
    exists      = (*env)->GetIntArrayElements (env, j_exists, 0);

    zreadx_ ((long long*)ifltab, pathname,
             intheader, &kintheader, nintheader,
             compheader, &kcompheader, ncompheader,
             userheader, &kuserheader, nuserheader,
              (int *)data, &kdata, ndata,
             &plan, exists, strlen (pathname));

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_intheader, intheader, 0);
    (*env)->ReleaseIntArrayElements (env, j_nintheader, nintheader, 0);
    (*env)->ReleaseIntArrayElements (env, j_compheader, compheader, 0);
    (*env)->ReleaseIntArrayElements (env, j_ncompheader, ncompheader, 0);
    (*env)->ReleaseIntArrayElements (env, j_userheader, userheader, 0);
    (*env)->ReleaseIntArrayElements (env, j_nuserheader, nuserheader, 0);
    (*env)->ReleaseShortArrayElements (env, j_data, data, 0);
    (*env)->ReleaseIntArrayElements (env, j_ndata, ndata, 0);
    (*env)->ReleaseIntArrayElements (env, j_exists, exists, 0);

}
