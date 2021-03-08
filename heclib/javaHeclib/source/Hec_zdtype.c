#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zdtype
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jintArray j_checkedNumber, jintArray j_exists, jobject j_type,
     jintArray j_dataType)
{
    int *ifltab;
    const char *path;
    int *number;
    int *exists;
    int *itype;

    int n;
    char ctype[50];
    jclass cls;
    jfieldID fid;
    jstring jstr;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab,  0);
    path = (const char *) (*env)->GetStringUTFChars (env, j_pathname,  0);
    number = (*env)->GetIntArrayElements (env, j_checkedNumber,  0);
    exists = (*env)->GetIntArrayElements (env, j_exists,  0);
    itype = (*env)->GetIntArrayElements (env, j_dataType,  0);

    zdtype_ ((long long*)ifltab, path, number, exists,
             ctype, itype,
             strlen(path), sizeof(ctype)-1);

    /* Send back the type as a string */
    cls = (*env)->GetObjectClass (env, j_type);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (ctype, &n, sizeof(ctype)-1);
		if ((n < 0) || (n > (sizeof(ctype)-1))) n = 0;
        ctype[n] = '\0';
        jstr = (*env)->NewStringUTF(env, ctype);
        (*env)->SetObjectField (env, j_type, fid, jstr);
    }

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, path);
    (*env)->ReleaseIntArrayElements (env, j_checkedNumber, number, 0);
    (*env)->ReleaseIntArrayElements (env, j_exists, exists, 0);
    (*env)->ReleaseIntArrayElements (env, j_dataType, itype, 0);

}

