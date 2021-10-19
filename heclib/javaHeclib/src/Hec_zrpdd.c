#include <jni.h>
#include "heclib.h"
#include <stdio.h>
#include <string.h>

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zrpdd
    (JNIEnv *env, jobject obj, jintArray j_ifltab,
     jstring j_pathname, jintArray j_nord, jintArray j_ncurve,
     jintArray j_ihorz, jobject j_c1unit,
     jobject j_c1type, jobject j_c2unit, jobject j_c2type,
     jdoubleArray j_values,
     jint j_kvals, jintArray j_nvals,
     jobjectArray j_clabel, jint j_klabel, jobject j_label,
     jintArray j_headu, jint j_kheadu, jintArray j_nheadu,
     jintArray j_istat)
{

		//////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////
	//   FIX ME - LABELS DON'T WORK FOR BOTH V 7 AND 6  ????
	//////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////



    int *ifltab;
    const char *pathname;
    int *nord;
    int *ncurve;
    int *ihorz;
    char c1unit[50];
    char c1type[50];
    char c2unit[50];
    char c2type[50];
    double *values;
    int kvals;
    int *nvals;
   char *clabel;
	char *clab;
    int klabel;
    int label = 0;     /* boolean */
	int nlabel;
    int *headu;
    int kheadu;
    int *nheadu;
    int *istat;
    int i;
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;
	size_t clabelSize;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

	ifltab   = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname = (*env)->GetStringUTFChars (env, j_pathname, 0);
    nord     = (*env)->GetIntArrayElements (env, j_nord, 0);
    ncurve   = (*env)->GetIntArrayElements (env, j_ncurve, 0);
    ihorz    = (*env)->GetIntArrayElements (env, j_ihorz, 0);
    values   = (*env)->GetDoubleArrayElements (env, j_values, 0);
    kvals    = (int) j_kvals;
    nvals    = (*env)->GetIntArrayElements (env, j_nvals, 0);
    klabel   = (int) j_klabel;
    headu    = (*env)->GetIntArrayElements (env, j_headu, 0);
    kheadu   = (int) j_kheadu;
    nheadu   = (*env)->GetIntArrayElements (env, j_nheadu, 0);
    istat    = (*env)->GetIntArrayElements (env, j_istat, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zrpdd, pathname: ", pathname);
	}

	if (klabel > 0) {
		clabelSize = klabel * 100;
		clabel = (char *)malloc(clabelSize);
		stringFill (clabel, ' ', (size_t) clabelSize);
	}
	else {
		clabel = (char *)malloc(1);
		clabel[0] = '\0';
	}

    zrpdd_ ((long long*)ifltab, pathname, nord, ncurve,
           ihorz, c1unit, c1type, c2unit,
           c2type, values, &kvals, nvals, (char *)clabel,
           &klabel,
           &label,  headu, &kheadu,  nheadu,  istat,
           strlen(pathname), sizeof(c1unit)-1, sizeof(c1type)-1,
           sizeof(c2unit)-1, sizeof(c2type)-1, 100);


    /* Set the units and types */
    cls = (*env)->GetObjectClass (env, j_c1unit);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (c1unit, &n, sizeof(c1unit)-1);
		if ((n < 0) || (n > (sizeof(c1unit)-1))) n = 0;
        c1unit[n] = '\0';
        jstr = (*env)->NewStringUTF(env, c1unit);
        (*env)->SetObjectField (env, j_c1unit, fid, jstr);
    }

    cls = (*env)->GetObjectClass (env, j_c1type);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (c1type, &n, sizeof(c1type)-1);		
		if ((n < 0) || (n > (sizeof(c1type)-1))) n = 0;
        c1type[n] = '\0';
        jstr = (*env)->NewStringUTF(env, c1type);
        (*env)->SetObjectField (env, j_c1type, fid, jstr);
    }

    cls = (*env)->GetObjectClass (env, j_c2unit);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (c2unit, &n, sizeof(c2unit)-1);
		if ((n < 0) || (n > (sizeof(c2unit)-1))) n = 0;
        c2unit[n] = '\0';
        jstr = (*env)->NewStringUTF(env, c2unit);
        (*env)->SetObjectField (env, j_c2unit, fid, jstr);
    }

    cls = (*env)->GetObjectClass (env, j_c2type);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (c2type, &n, sizeof(c2type)-1);
		if ((n < 0) || (n > (sizeof(c2type)-1))) n = 0;
        c2type[n] = '\0';
        jstr = (*env)->NewStringUTF(env, c2type);
        (*env)->SetObjectField (env, j_c2type, fid, jstr);
    }

    cls = (*env)->GetObjectClass (env, j_label);
    fid = (*env)->GetFieldID (env, cls, "value", "Z");
    if (fid)  {
        if(label) {
            (*env)->SetBooleanField(env, j_label, fid, JNI_TRUE);
			nlabel = ncurve[0];
			if (nlabel > klabel) nlabel = klabel;
            for(i=0; i < nlabel; i++) {
				if (((i+1) * 100) >= clabelSize) break;
				clab = stringFortToC(&clabel[(i*100)], 99);;
                jstr = (*env)->NewStringUTF(env, clab);
                (*env)->SetObjectArrayElement(env, j_clabel, i, jstr);
				free(clab);
            }
        }
        else {
            (*env)->SetBooleanField(env, j_label, fid, JNI_FALSE);        
        }
    } 

	if (clabel) free(clabel);


	  /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_nord, nord, 0);
    (*env)->ReleaseIntArrayElements (env, j_ncurve, ncurve, 0);
    (*env)->ReleaseIntArrayElements (env, j_ihorz, ihorz, 0);
    (*env)->ReleaseDoubleArrayElements (env, j_values, values, 0);
    (*env)->ReleaseIntArrayElements (env, j_nvals, nvals, 0);
    (*env)->ReleaseIntArrayElements (env, j_headu, headu, 0);
    (*env)->ReleaseIntArrayElements (env, j_nheadu, nheadu, 0);
    (*env)->ReleaseIntArrayElements (env, j_istat, istat, 0);


}
