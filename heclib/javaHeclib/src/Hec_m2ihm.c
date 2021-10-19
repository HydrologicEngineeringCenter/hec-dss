#include <string.h>
#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1m2ihm
    (JNIEnv *env, jobject obj, jint j_minute, jobject j_hourMinutes)
{
    int   ihm;
    int   minute;
    char  ctime[10];

    jclass cls;
    jfieldID fid;
    jstring jstr;
    int n;


    minute = (int) j_minute;

    ihm = m2ihm_ (&minute, ctime, sizeof(ctime)-1);

    cls = (*env)->GetObjectClass (env, j_hourMinutes);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (ctime, &n, sizeof(ctime)-1);
		if ((n < 0) || (n > (sizeof(ctime)-1))) n = 0;
        ctime[n] = '\0';
        jstr = (*env)->NewStringUTF(env, ctime);
        (*env)->SetObjectField (env, j_hourMinutes, fid, jstr);
    }

    return ihm;
}
