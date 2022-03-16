#include <string.h>
#include <jni.h>
#include "heclib.h"
#include <stdio.h>

JNIEXPORT jstring JNICALL Java_hec_heclib_util_Heclib_Hec_1zpath
    (JNIEnv *env, jobject obj, jstring j_apart, jstring j_bpart,
     jstring j_cpart, jstring j_dpart, jstring j_epart, jstring j_fpart)
{
    const char *Apart;
    const char *Bpart;
    const char *Cpart;
    const char *Dpart;
    const char *Epart;
    const char *Fpart;

    char pathname[MAX_PATHNAME_LENGTH];
    int  npath;

    Apart = (const char *) (*env)->GetStringUTFChars (env, j_apart, 0);
    Bpart = (const char *) (*env)->GetStringUTFChars (env, j_bpart, 0);
    Cpart = (const char *) (*env)->GetStringUTFChars (env, j_cpart, 0);
    Dpart = (const char *) (*env)->GetStringUTFChars (env, j_dpart, 0);
    Epart = (const char *) (*env)->GetStringUTFChars (env, j_epart, 0);
    Fpart = (const char *) (*env)->GetStringUTFChars (env, j_fpart, 0);

    zpath_ (Apart, Bpart, Cpart, Dpart, Epart, Fpart, pathname, &npath,
            strlen(Apart), strlen(Bpart), strlen(Cpart), strlen(Dpart),
            strlen(Epart), strlen(Fpart), (int)sizeof(pathname)-1);

    pathname[npath] = '\0';

    (*env)->ReleaseStringUTFChars (env, j_apart, Apart);
    (*env)->ReleaseStringUTFChars (env, j_bpart, Bpart);
    (*env)->ReleaseStringUTFChars (env, j_cpart, Cpart);
    (*env)->ReleaseStringUTFChars (env, j_dpart, Dpart);
    (*env)->ReleaseStringUTFChars (env, j_epart, Epart);
    (*env)->ReleaseStringUTFChars (env, j_fpart, Fpart);
    
    return (*env)->NewStringUTF (env, pathname);
}
