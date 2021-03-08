#include <string.h>
#include <jni.h>
#include "heclib.h"



JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zplist
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_instr,
     jintArray j_filePos, jobject j_pathname)
{

    int *ifltab;
    const char *instr;
    int *filePos;

    char pathname[392];
    jclass cls;
    jfieldID fid;
    jstring jstr;
//	int len;
	int jpos;
	char *pos;
	char *path;

    int nPathname=0;
    int status=-1;


    ifltab = (*env)->GetIntArrayElements (env, j_ifltab,  0);
	instr = (const char *) (*env)->GetStringUTFChars (env, j_instr, 0);
    filePos = (*env)->GetIntArrayElements (env, j_filePos,  0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zplist; file position: ", filePos[0]);
	}
	if (filePos[0] < 0) {
		filePos[0] = 0;
	}

	if (zgetVersion((long long*)ifltab) == 6) {	
		zplist6_ ((long long*)ifltab, instr, filePos, pathname, &nPathname, &status,
		        strlen(instr), sizeof(pathname)-1);
		if (status == 0) {
			pathname[nPathname] = '\0';
		}
	}
	else {
		zplist7 ((long long*)ifltab, instr, filePos, pathname, &nPathname, &status,
		        (int)strlen(instr), (int)sizeof(pathname)-1);
		if (status == 0) {
			/////////////////////////////////////////////////
			//  For compatibility purposes only...
			//  Make pathname uppercase and change "Minute" to "MIN"
			upperCase(pathname);
			pos = strstr(pathname, "MINUTE/");
			if (pos > 0) {
				path = pathname;
				pos += 3;
				jpos = (int)(pos - path);
				if (jpos > 0) {
					pathname[jpos] = '\0';
					pos += 3;
					//strcat_s(pathname, sizeof(pathname), pos);
					stringCat(pathname, sizeof(pathname), pos, _TRUNCATE);
				}
			}
		}
	}

	if (status) {
		filePos[0] = 0;
	}


    /* Send back the pathname as a string */
    cls = (*env)->GetObjectClass (env, j_pathname);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        jstr = (*env)->NewStringUTF(env, pathname);
        (*env)->SetObjectField (env, j_pathname, fid, jstr);		
    }
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_zplist; status: ", status);
		if (!status) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zplist; file position: ", filePos[0]);
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zplist; Pathname: ", pathname);
		}
	}

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_instr, instr);
    (*env)->ReleaseIntArrayElements (env, j_filePos, filePos, 0);

    return (jint)status;
}

