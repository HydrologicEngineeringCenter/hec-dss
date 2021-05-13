#include <string.h>
#include <jni.h>
#include "heclib.h"
#include "zerrorCodes.h"



//  Get a collection list pathname
//  public static synchronized native String Hec_zcolist(int ifltab[],int filePos[], String seedPathname);

JNIEXPORT jstring JNICALL Java_hec_heclib_util_Heclib_Hec_1zcolist
    (JNIEnv *env, jobject obj, jintArray j_ifltab, 
     jintArray j_filePos, jstring j_pathname)
{

    int *ifltab;    
    int *filePos;

    char pathname[MAX_PATHNAME_LENGTH];
	const char *cpathname;

    int nPathname=0;
    int status=0;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab,  0);    
    filePos = (*env)->GetIntArrayElements (env, j_filePos,  0);
	cpathname = (const char *)(*env)->GetStringUTFChars(env, j_pathname, 0);

	if (cpathname) {
		stringCopy(pathname, MAX_PATHNAME_LENGTH, cpathname, strlen(cpathname));
	}
	else {
		pathname[0] = '\0';
	}
	(*env)->ReleaseStringUTFChars (env, j_pathname, cpathname);
	
	nPathname = (int)strlen(pathname);	

	if (zgetVersion((long long*)ifltab) == 6) {
		zcolist6_ ((long long*)ifltab, filePos, pathname, &nPathname, &status, sizeof(pathname));
		stringLastNonBlank(pathname, MAX_PATHNAME_LENGTH);
		if (status || (nPathname == 0)) {
			filePos[0] = -1;
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID,  
				zdssErrorCodes.INCOMPATIBLE_VERSION_6, zgetVersion((long long*)ifltab), 0, 
				zdssErrorSeverity.WARNING, "", "zcolist");
	} 

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseIntArrayElements (env, j_filePos, filePos, 0);

	/* Send back the pathname as a string */
	return (*env)->NewStringUTF(env, pathname);
}

