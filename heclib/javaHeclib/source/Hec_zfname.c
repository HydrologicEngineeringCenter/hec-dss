#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT jboolean JNICALL Java_hec_heclib_util_Heclib_Hec_1zfname
    (JNIEnv *env, jobject obj, jstring j_inName, jobject j_outName)
{
    const char *inName;

    char outName[260];
    int  nname;
    int  lexists;
	size_t len1, len2;

    jclass cls;
    jfieldID fid;
    jstring jstr;


    inName = (const char *) (*env)->GetStringUTFChars (env, j_inName, 0);
	stringFill(outName, ' ', sizeof(outName));
	len1 = strlen(inName);
	len2 = sizeof(outName);

	zfname (inName, outName, &nname, &lexists, len1, len2);
	
    (*env)->ReleaseStringUTFChars (env, j_inName, inName);

    cls = (*env)->GetObjectClass (env, j_outName);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        jstr = (*env)->NewStringUTF(env, outName);
        (*env)->SetObjectField (env, j_outName, fid, jstr);
    }

    if (lexists) {
        return JNI_TRUE;
    }
    else {
        return JNI_FALSE;
    }

}
