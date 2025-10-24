#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zcheck
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jintArray j_numberHeader, jintArray j_numberData, jintArray j_exists)
{
    int *ifltab;
    const char *path;
    int *numberHeader;
	int *numberData;
    int *exists;   
  
	int npath;     

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab,  0);
    path = (const char *) (*env)->GetStringUTFChars (env, j_pathname,  NULL);
	npath = (int)strlen(path);
    numberHeader = (*env)->GetIntArrayElements (env, j_numberHeader,  0);
	numberData = (*env)->GetIntArrayElements (env, j_numberData,  0);
    exists = (*env)->GetIntArrayElements (env, j_exists,  0);   

    zcheck7((long long*)ifltab, path, &npath, numberHeader, numberData, exists);

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, path);
    (*env)->ReleaseIntArrayElements (env, j_numberHeader, numberHeader, 0);
	(*env)->ReleaseIntArrayElements (env, j_numberData, numberData, 0);
    (*env)->ReleaseIntArrayElements (env, j_exists, exists, 0);


}

