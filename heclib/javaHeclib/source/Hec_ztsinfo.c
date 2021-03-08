#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1ztsinfo
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jintArray j_startJulian, jintArray j_startMinutes,
     jintArray j_endJulian, jintArray j_endMinutes,
     jobject j_units, jobject j_type,
     jintArray j_lquality, jintArray j_ldouble, jintArray j_lfound)
{
    int *ifltab;
    const char *path;
    int *startJulian;
    int *startMinutes;
    int *endJulian;
    int *endMinutes;
    int *lquality;
    int *ldouble;
    int *lfound;

    char units[50];
    char type[50];
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;


    ifltab = (*env)->GetIntArrayElements (env, j_ifltab,  0);
    path = (const char *) (*env)->GetStringUTFChars (env, j_pathname,  0);
    startJulian  = (*env)->GetIntArrayElements (env, j_startJulian,  0);
    startMinutes = (*env)->GetIntArrayElements (env, j_startMinutes, 0);
    endJulian    = (*env)->GetIntArrayElements (env, j_endJulian,    0);
    endMinutes   = (*env)->GetIntArrayElements (env, j_endMinutes,   0);
    lquality     = (*env)->GetIntArrayElements (env, j_lquality   ,  0);
    ldouble      = (*env)->GetIntArrayElements (env, j_ldouble    ,  0);
    lfound       = (*env)->GetIntArrayElements (env, j_lfound     ,  0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_ztsinfo, pathname: ", path);
	}

		ztsinfo_ ((long long*)ifltab, path, startJulian, startMinutes,
				endJulian, endMinutes, units, type,
				lquality, ldouble, lfound,
				strlen(path), sizeof (units) -1, sizeof (type) -1);
			  
    

    /* Set the units and type */
    cls = (*env)->GetObjectClass (env, j_units);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (units, &n, sizeof(units)-1);
		if ((n < 0) || (n > (sizeof(units)-1))) n = 0;
        units[n] = '\0';
        jstr = (*env)->NewStringUTF(env, units);
        (*env)->SetObjectField (env, j_units, fid, jstr);
    }

    cls = (*env)->GetObjectClass (env, j_type);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (type, &n, sizeof(type)-1);
		if ((n < 0) || (n > (sizeof(type)-1))) n = 0;
        type[n] = '\0';
        jstr = (*env)->NewStringUTF(env, type);
        (*env)->SetObjectField (env, j_type, fid, jstr);
    }

	(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars   (env, j_pathname, path);
    (*env)->ReleaseIntArrayElements (env, j_startJulian, startJulian, 0);
    (*env)->ReleaseIntArrayElements (env, j_startMinutes, startMinutes, 0);
    (*env)->ReleaseIntArrayElements (env, j_endJulian, endJulian, 0);
    (*env)->ReleaseIntArrayElements (env, j_endMinutes, endMinutes, 0);
    (*env)->ReleaseIntArrayElements (env, j_lquality, lquality, 0);
    (*env)->ReleaseIntArrayElements (env, j_ldouble, ldouble, 0);
    (*env)->ReleaseIntArrayElements (env, j_lfound, lfound, 0);

}
