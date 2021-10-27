#include <jni.h>

#include "heclib.h"
#include "javaHeclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zlocationStore(
	JNIEnv       *env,
	jobject       obj, 
	jintArray     j_ifltab,	
	jobject		  j_dataContainer)
{

	jstring j_pathname;
	jclass cls;
    jfieldID fid;
	int *ifltab;
	int status;
	char *pathname;
	zStructLocation *locationStruct;


	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	
	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_dataContainer);
    fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_pathname = (*env)->GetObjectField(env, j_dataContainer, fid); 
        pathname = (char *)(*env)->GetStringUTFChars(env, j_pathname,  0);
    }
	
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zlocationStore, Enter.  Pathname: ", pathname);		
	}

	locationStruct = zstructLocationNew(pathname);
	(*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
	if (!locationStruct) {
		(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
		return (jint)-1;
	}

	Hec_zlocationToStruct(env, obj, j_dataContainer, locationStruct);

	//  Do we have anything to store?
	if (zlocationStructValid(locationStruct) != STATUS_OKAY) {
		zstructFree(locationStruct);
		return STATUS_NOT_OKAY;
	}

	status = zlocationStore((long long*)ifltab, locationStruct, 1);

	zstructFree(locationStruct);
	(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
	return (jint)status;
}