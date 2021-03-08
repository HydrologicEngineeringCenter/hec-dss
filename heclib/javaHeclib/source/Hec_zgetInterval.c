#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zgetInterval
    (JNIEnv *env, jobject obj, jintArray j_interval, jobject j_ePart,
     jintArray j_nvals, jintArray j_status)
{
    int *interval;
    const char *ePart;
    int *nvals;
    int *status;
    char cEpart[65];
	int n;
	jclass cls;
    jfieldID fid;
    jstring jstr;

    interval = (*env)->GetIntArrayElements (env, j_interval, 0);
    nvals    = (*env)->GetIntArrayElements (env, j_nvals, 0);
    status   = (*env)->GetIntArrayElements (env, j_status, 0);

	/* Get the E part */
    cls = (*env)->GetObjectClass (env, j_ePart);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField (env, j_ePart, fid);
        ePart = (*env)->GetStringUTFChars (env, jstr, 0);
		if (ePart) {
			stringCopy(cEpart, 64, ePart, strlen(ePart));
		}
		else
			cEpart[0] = '\0';
		(*env)->ReleaseStringUTFChars (env, jstr, ePart);
		upcase_ (cEpart, strlen(cEpart));
	}
	else {
		cEpart[0] = '\0';
	}    

    zgintl_ (interval, cEpart, nvals, status,
             sizeof(cEpart)-1);
    
	/* Set the E part */
    /*  the fid and cls vars have already been set above */
    if (fid != 0) {
        chrlnb_ (cEpart, &n, sizeof(cEpart)-1);
		if (n < 0)
			n = 0;	
        cEpart[n] = '\0';
        jstr = (*env)->NewStringUTF(env, cEpart);
        (*env)->SetObjectField (env, j_ePart, fid, jstr);
    }

	(*env)->ReleaseIntArrayElements (env, j_interval, interval, 0);    
    (*env)->ReleaseIntArrayElements (env, j_nvals, nvals, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);
}
