#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1ztsrange
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jint j_searchOption, jobject j_firstPath, jobject j_lastPath,
     jintArray j_numberFound)
{
    int *ifltab;
    const char *path;
    int  searchOption;
    int *numberFound;

    char firstPath[392];
    char lastPath[392];
    int n;
    jclass cls;
    jfieldID fid;
    jstring jstr;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab,  0);
    path = (const char *) (*env)->GetStringUTFChars (env, j_pathname,  0);
    searchOption = (int) j_searchOption;
    numberFound = (*env)->GetIntArrayElements (env, j_numberFound, 0);


    ztsrange_ ((long long*)ifltab, path, &searchOption,
              firstPath, lastPath, numberFound,
              strlen(path), sizeof(firstPath)-1, sizeof(lastPath)-1);
    
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_ztsranges, numberFound: ", numberFound[0]);
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsends, firstPath: ", firstPath);
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsends, lastPath: ", lastPath);
	}

    /* Set the firstPath and lastPath */
    cls = (*env)->GetObjectClass (env, j_firstPath);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (firstPath, &n, sizeof(firstPath)-1);
		if ((n < 0) || (n > (sizeof(firstPath)-1))) n = 0;
        firstPath[n] = '\0';
        jstr = (*env)->NewStringUTF(env, firstPath);
        (*env)->SetObjectField (env, j_firstPath, fid, jstr);
    }

    cls = (*env)->GetObjectClass (env, j_lastPath);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {
        chrlnb_ (lastPath, &n, sizeof(lastPath)-1);
		if ((n < 0) || (n > (sizeof(lastPath)-1))) n = 0;
        lastPath[n] = '\0';
        jstr = (*env)->NewStringUTF(env, lastPath);
        (*env)->SetObjectField (env, j_lastPath, fid, jstr);
    }

	(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars   (env, j_pathname, path);
    (*env)->ReleaseIntArrayElements (env, j_numberFound, numberFound, 0);

}
