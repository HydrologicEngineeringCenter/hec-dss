#include <string.h>
#include <jni.h>
#include <stdio.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zdebug1
    (JNIEnv *env, jobject obj, jint j_ival, jlongArray j_ival8,
	 jobject j_crval, jobject j_cstrng, jintArray j_ibytes)
{
	// ival is input, rest are output
	int		ival;
	long long *ival8;
	char	crval[13];
	char	cstrng[5];
	int		*ibytes;

    jclass cls;
    jfieldID fid;
    jstring jstr;

	ival =		(int) j_ival;
	ival8 =		(long long *)(*env)->GetLongArrayElements (env, j_ival8, 0);
	ibytes =	(*env)->GetIntArrayElements (env, j_ibytes, 0);

	zdebug1_ (&ival, ival8, crval, cstrng, ibytes,
		      sizeof(crval)-1, sizeof(cstrng)-1);
    
    /* Release */
    (*env)->ReleaseLongArrayElements (env, j_ival8, (jlong *)ival8, 0); 
	(*env)->ReleaseIntArrayElements (env, j_ibytes, ibytes, 0); 
    
	/* Send back crval as a string */
    cls = (*env)->GetObjectClass (env, j_crval);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        crval[sizeof(crval)-1] = '\0';
        jstr = (*env)->NewStringUTF(env, crval);
        (*env)->SetObjectField (env, j_crval, fid, jstr);
    }

	/* Send back cstrng as a string */
    cls = (*env)->GetObjectClass (env, j_cstrng);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        cstrng[sizeof(cstrng)-1] = '\0';
        jstr = (*env)->NewStringUTF(env, cstrng);
        (*env)->SetObjectField (env, j_cstrng, fid, jstr);
    }
}



