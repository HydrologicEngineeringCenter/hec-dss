#include <jni.h>
#include <string.h>
#include <stdlib.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zrtxts
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jstring j_pathname,
     jobject j_cString, jint j_kCString, jintArray j_nCString, 
     jintArray j_userHeader, jint j_kUserHeader, jintArray j_nUserHeader,
	 jintArray j_status)
{	

	
    int *ifltab;
	const char *pathname;
	char  * cString;
	int    kCString;
    int  *nCString;   
    int  *userHeader;
	int    kUserHeader;
    int  *nUserHeader;
    int  *status;

	int maxCString;
	int npath;
	int nhead;
	int ndata;
	int lfound;

	jclass cls;
    jfieldID fid;
    jstring jstr;
        
    ifltab       = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname     = (*env)->GetStringUTFChars   (env, j_pathname, 0);
    
    kCString     = (int) j_kCString;
	nCString     = (*env)->GetIntArrayElements (env, j_nCString, 0);
    userHeader   = (*env)->GetIntArrayElements (env, j_userHeader, 0);
	kUserHeader  = (int) j_kUserHeader;
	nUserHeader  = (*env)->GetIntArrayElements (env, j_nUserHeader, 0);
    status       = (*env)->GetIntArrayElements (env, j_status, 0);
	
	//  Find out how much memory we need for this text record
	npath = (int)strlen(pathname);	
	lfound = 0;
	zcheck_ ((long long*)ifltab, pathname,  &npath, &nhead, &ndata, &lfound,
	         strlen(pathname));	

	if (lfound == 0)  {
		/* If the record does not exist, let zrtxt still try to read
		   it and generate the correct error message */
		ndata = 1;
	}
	maxCString = (ndata + 1) * 4 + 1;
	if (kCString < maxCString)
		maxCString = kCString;
	cString = (char *) calloc (maxCString, 1);
	if (cString == NULL) {
		fprintf (stderr, "Memory allocation failure in Hec_zrtxts\n");
		return;
	}
	
    zrtxts_ ((long long*)ifltab, pathname, cString, &maxCString, nCString, 
            userHeader, &kUserHeader, nUserHeader, status,		
            strlen (pathname), (maxCString-1));	

  
	/* Build the string to be returned */
	cls = (*env)->GetObjectClass (env, j_cString);
    fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
    if (fid != 0) {        
        cString[nCString[0]] = '\0';
        jstr = (*env)->NewStringUTF(env, cString);
        (*env)->SetObjectField (env, j_cString, fid, jstr);
    }

	free(cString);


       /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars   (env, j_pathname, pathname);
    (*env)->ReleaseIntArrayElements (env, j_nCString, nCString, 0);       
    (*env)->ReleaseIntArrayElements (env, j_userHeader, userHeader, 0);
	(*env)->ReleaseIntArrayElements (env, j_nUserHeader, nUserHeader, 0);
    (*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}
