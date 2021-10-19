#ifdef _MSC_VER
 
#else
#include <inttypes.h>
#endif

#include <string.h>
#include <jni.h>
#include <stdio.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zdebug7
    (JNIEnv *env, jobject obj, jintArray j_ival, jlongArray j_ival8,
	jintArray j_ival4a, jintArray j_ival4b, jobject j_cdval,
	 jobject j_crvala, jobject j_crvalb, jobject j_cstrng, jintArray j_ibytes)
{
	// ival is input, rest are output
	int		*ival;
#ifdef _MSC_VER
	__int64   *ival8;
#else 
	int64_t  *ival8;
#endif
	int    *ival4a;
	int    *ival4b;
	char	cdval[13];
	char	crvala[13];
	char	crvalb[13];
	char	cstrng[9];
	int		*ibytes;

    jclass cls;
    jfieldID fid;
    jstring jstr;

	ival =		(*env)->GetIntArrayElements (env, j_ival, 0);
	ival8 =		(*env)->GetLongArrayElements (env, j_ival8, 0);
	ival4a =	(*env)->GetIntArrayElements (env, j_ival4a, 0);
	ival4b =	(*env)->GetIntArrayElements (env, j_ival4b, 0);
	ibytes =	(*env)->GetIntArrayElements (env, j_ibytes, 0);

	zdebug7_ (ival, ival8, ival4a, ival4b, cdval, crvala, crvalb, cstrng, ibytes,
		      sizeof(cdval)-1, sizeof(crvala)-1, sizeof(crvalb)-1, sizeof(cstrng)-1);
    
    /* Release */
	(*env)->ReleaseIntArrayElements (env, j_ival, ival, 0);
    (*env)->ReleaseLongArrayElements (env, j_ival8, ival8, 0); 
	(*env)->ReleaseIntArrayElements (env, j_ival4a, ival4a, 0); 
	(*env)->ReleaseIntArrayElements (env, j_ival4b, ival4b, 0); 
	(*env)->ReleaseIntArrayElements (env, j_ibytes, ibytes, 0); 
    
	/* Send back cdval as a string */
    cls = (*env)->GetObjectClass (env, j_cdval);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        cdval[sizeof(cdval)-1] = '\0';
        jstr = (*env)->NewStringUTF(env, cdval);
        (*env)->SetObjectField (env, j_cdval, fid, jstr);
    }

	/* Send back crvala as a string */
    cls = (*env)->GetObjectClass (env, j_crvala);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        crvala[sizeof(crvala)-1] = '\0';
        jstr = (*env)->NewStringUTF(env, crvala);
        (*env)->SetObjectField (env, j_crvala, fid, jstr);
    }

	/* Send back crvalb as a string */
    cls = (*env)->GetObjectClass (env, j_crvalb);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        crvalb[sizeof(crvalb)-1] = '\0';
        jstr = (*env)->NewStringUTF(env, crvalb);
        (*env)->SetObjectField (env, j_crvalb, fid, jstr);
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



