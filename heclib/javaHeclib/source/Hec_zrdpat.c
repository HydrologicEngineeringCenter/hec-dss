#include <jni.h>
#include <stdio.h>
#include "heclib.h"


JNIEXPORT jboolean JNICALL Java_hec_heclib_util_Heclib_Hec_1zrdpat
    (JNIEnv *env, jobject obj, jintArray j_catalogUnit, jintArray j_position,
     jintArray j_referenceNumber, jobject j_tag, jobject j_pathname)
{
    int *catalogUnit;
    int *position;
    int *referenceNumber;

    char tag[10];
    char pathname[392];
    int  npath;
    int  lend=0;
    int  n;

    jclass cls;
    jfieldID fid;
    jstring jstr;
//	printf("Enter Java_hec_heclib_util_Heclib_Hec_1zrdpat\n ");
    catalogUnit     = (*env)->GetIntArrayElements (env, j_catalogUnit, 0);
    position        = (*env)->GetIntArrayElements (env, j_position, 0);
    referenceNumber = (*env)->GetIntArrayElements (env, j_referenceNumber, 0);
//	printf("Enter Hec_1zrdpat\n ");
	
    zrdpat_ ((int*)catalogUnit, (int*)position, (int*)referenceNumber,
             tag, pathname, &npath, &lend, sizeof(tag)-1,
             sizeof(pathname)-1);
   

    cls = (*env)->GetObjectClass (env, j_tag);
    if (cls != 0) {
        fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
        if (fid != 0) {
            chrlnb_ (tag, &n, sizeof(tag)-1);
			if ((n < 0) || (n > (sizeof(tag)-1))) n = 0;
            tag[n] = '\0';
            jstr = (*env)->NewStringUTF(env, tag);
            (*env)->SetObjectField (env, j_tag, fid, jstr);
        }
    }

    cls = (*env)->GetObjectClass (env, j_pathname);
    if (cls != 0) {
        fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
        if (fid != 0) {
            chrlnb_ (pathname, &n, sizeof(pathname)-1);
			if ((n < 0) || (n > (sizeof(pathname)-1))) n = 0;
            pathname[n] = '\0';
            jstr = (*env)->NewStringUTF(env, pathname);
            (*env)->SetObjectField (env, j_pathname, fid, jstr);
        }
    }

	 /* Release */
    (*env)->ReleaseIntArrayElements (env, j_catalogUnit, catalogUnit, 0);
    (*env)->ReleaseIntArrayElements (env, j_position, position, 0);
    (*env)->ReleaseIntArrayElements (env, j_referenceNumber,
                                     referenceNumber, 0);

    if (lend) {
        return JNI_TRUE;
    }
    else {
        return JNI_FALSE;
    }

}
