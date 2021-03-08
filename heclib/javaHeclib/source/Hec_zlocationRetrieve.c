#include <jni.h>

#include "heclib.h"
#include "javaHeclib.h"

/*
*	Reads a location record and returns information in a Java DataContainer
*	This function is not normally called -
*/

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zlocationRetrieve
	(JNIEnv       *env, 
	jobject       obj, 
	jintArray     j_ifltab,
	jobject		  j_dataContainer) 
{

	int status;
	const char *pathname;
	int *ifltab;
	long long lastWriteTime;
	long long fileLastWriteTime;

	jstring j_pathname;
	jclass cls;
    jfieldID fid;

	zStructLocation *locationStruct;


	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_dataContainer);
	fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_pathname = (*env)->GetObjectField(env, j_dataContainer, fid); 
        pathname = (*env)->GetStringUTFChars(env, j_pathname,  0);
    }
	
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zlocationRetrieve, Enter.  Pathname: ", pathname);		
	}

	locationStruct = zstructLocationNew(pathname);
	if (!locationStruct) {
		(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
		return (jint)-1;
	}
	(*env)->ReleaseStringUTFChars (env, j_pathname, pathname);

	status = zlocationRetrieve((long long *)ifltab, locationStruct);

	if (status != STATUS_RECORD_FOUND) {
		zstructFree(locationStruct);
		(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
		return status;
	}

	if (locationStruct->pathnameInternal) {
		lastWriteTime = zgetLastWriteTimeRec((long long *)ifltab, locationStruct->pathnameInternal);
	}
	else {
		lastWriteTime = zgetLastWriteTimeRec((long long *)ifltab, locationStruct->pathname);
	}
	fileLastWriteTime = zgetLastWriteTimeFile((long long *)ifltab);

	
	//  Save the location information in the data container
	status = Hec_zlocationFromStruct(env, obj, j_dataContainer, locationStruct);

	fid = (*env)->GetFieldID (env, cls, "lastWriteTimeMillis", "J");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		(*env)->SetLongField(env, j_dataContainer, fid, lastWriteTime);
	}

	//  File last write time
	fid = (*env)->GetFieldID (env, cls, "fileLastWriteTimeMillis", "J");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		(*env)->SetLongField(env, j_dataContainer, fid, fileLastWriteTime);
	}
	
	zstructFree(locationStruct);
	(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
	return status;
}
