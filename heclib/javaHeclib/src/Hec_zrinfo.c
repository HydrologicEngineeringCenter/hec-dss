#include <jni.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zrinfo
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
	 jintArray j_found, jintArray j_type, jstring j_ctype, 
	jintArray j_ldoubles, jintArray j_lquality, jintArray j_precision, 
	jstring j_tag, jstring j_lwdate, jstring j_lwtime, jstring j_progName,
	jintArray j_version, jintArray j_numberData, jintArray j_spaceUsed,
	jintArray j_compression, jintArray j_password)
{
    int *ifltab;
	const char *pathname;
    int  *found;
	int  *type;
	char ctype[51];
	int  *ldoubles;
	int  *lquality;
	int  *precision;
	char tag[9];
	char lwdate[13];
	char lwtime[13];
	char progName[9];
	int password[8];
	int  *version;
	int  *numberData;
	int  *spaceUsed;
	int  *compression;
	

    jclass cls;
    jfieldID fid;
    jstring jstr;
	int n;

    ifltab      = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname    = (*env)->GetStringUTFChars   (env, j_pathname, 0);   
    found       = (*env)->GetIntArrayElements (env, j_found, 0);
    type        = (*env)->GetIntArrayElements (env, j_type, 0);
	ldoubles    = (*env)->GetIntArrayElements (env, j_ldoubles, 0);
	lquality    = (*env)->GetIntArrayElements (env, j_lquality, 0);
	precision   = (*env)->GetIntArrayElements (env, j_precision, 0);
	version     = (*env)->GetIntArrayElements (env, j_version, 0);
	numberData  = (*env)->GetIntArrayElements (env, j_numberData, 0);
	spaceUsed   = (*env)->GetIntArrayElements (env, j_spaceUsed, 0);
	compression = (*env)->GetIntArrayElements (env, j_compression, 0);
	

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zrinfo, pathname: ", pathname);
	}

	zrinfo_ ((long long*)ifltab, pathname, found, type,
			ctype, ldoubles, lquality, precision, 
			tag, lwdate, lwtime, progName, 
			version, numberData, spaceUsed, 
			compression, password, strlen (pathname),
			sizeof (ctype) -1, sizeof (tag) -1, sizeof (lwdate) -1, 
			sizeof (lwtime) -1, sizeof (progName) -1);
	
    /* Set the type */
    cls = (*env)->GetObjectClass (env, j_ctype);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (ctype, &n, sizeof(ctype)-1);
		if ((n < 0) || (n > (sizeof(ctype)-1))) n = 0;
        ctype[n] = '\0';
        jstr = (*env)->NewStringUTF(env, ctype);
        (*env)->SetObjectField (env, j_ctype, fid, jstr);
    }

    cls = (*env)->GetObjectClass (env, j_tag);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (tag, &n, sizeof(tag)-1);
		if ((n < 0) || (n > (sizeof(tag)-1))) n = 0;
        tag[n] = '\0';
        jstr = (*env)->NewStringUTF(env, tag);
        (*env)->SetObjectField (env, j_tag, fid, jstr);
    }

	cls = (*env)->GetObjectClass (env, j_lwdate);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (lwdate, &n, sizeof(lwdate)-1);
		if ((n < 0) || (n > (sizeof(lwdate)-1))) n = 0;
        lwdate[n] = '\0';
        jstr = (*env)->NewStringUTF(env, lwdate);
        (*env)->SetObjectField (env, j_lwdate, fid, jstr);
    }

	cls = (*env)->GetObjectClass (env, j_lwtime);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (lwtime, &n, sizeof(lwtime)-1);
		if ((n < 0) || (n > (sizeof(lwtime)-1))) n = 0;
        lwtime[n] = '\0';
        jstr = (*env)->NewStringUTF(env, lwtime);
        (*env)->SetObjectField (env, j_lwtime, fid, jstr);
    }

	cls = (*env)->GetObjectClass (env, j_progName);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (progName, &n, sizeof(progName)-1);
		if ((n < 0) || (n > (sizeof(progName)-1))) n = 0;
        progName[n] = '\0';
        jstr = (*env)->NewStringUTF(env, progName);
        (*env)->SetObjectField (env, j_progName, fid, jstr);
    }

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_found, found, 0);
    (*env)->ReleaseIntArrayElements (env, j_type, type, 0);
	(*env)->ReleaseIntArrayElements (env, j_ldoubles, ldoubles, 0);
	(*env)->ReleaseIntArrayElements (env, j_lquality, lquality, 0);
	(*env)->ReleaseIntArrayElements (env, j_precision, precision, 0);
	(*env)->ReleaseIntArrayElements (env, j_version, version, 0);
	(*env)->ReleaseIntArrayElements (env, j_numberData, numberData, 0);
	(*env)->ReleaseIntArrayElements (env, j_spaceUsed, spaceUsed, 0);
	(*env)->ReleaseIntArrayElements (env, j_compression, compression, 0);

}
