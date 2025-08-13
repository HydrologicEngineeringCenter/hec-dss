#include <jni.h>

#include "heclib.h"
#include "zerrorCodes.h"
#include "hecdssInternal.h"

/*
Severity:
*		INFORMATION		1	An inconsequential action failed
*		WARNING			2	Unable to compete the request given
*		WARNING_NO_WRITE_ACCESS	3	You cannot write to the file
*		WARNING_NO_FILE_ACCESS	4	You cannot read or write to the file
*		WRITE_ERROR		5	An error was thrown on a write action.
*		READ_ERROR		6	An error was thrown on a read action.  The file maybe damaged.  It might be recovered by a squeeze, at the user's discretion.
*		CORRUPT_FILE	7	Flags and pointers are not correct, indicating that something has changed the file outside of DSS
*		MEMORY_ERROR	8	Flags and pointers in memory are not correct
*		CRITICAL_ERROR	9	Undefined severe error thrown
*/


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zerrorCheck
	(JNIEnv *env, jobject obj)   
{
	int status;
	status = zerrorCheck();	
	return (jint)status;
}


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zerrorClear
    (JNIEnv *env, jobject obj)
{
	zerrorStructClear();
}

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zerrorGet
	 (JNIEnv *env, jobject obj, jobject	j_dssErrorMessage)    
{
	jclass cls;
    jfieldID fid;

	jint jnumber;
	jlong j_long;
    jstring j_message;
	jstring j_dssFileName;
	jstring j_name;
	char *name;
	hec_zdssLastError errorStruct;

	
	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

	zerror(&errorStruct);

	cls = (*env)->GetObjectClass (env, j_dssErrorMessage);

	fid = (*env)->GetFieldID (env, cls, "errorCode", "I");
	if (fid) {
		jnumber = (jint)errorStruct.errorCode;
		(*env)->SetIntField(env, j_dssErrorMessage, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "severity", "I");
	if (fid) {
		jnumber = (jint)errorStruct.severity;
		(*env)->SetIntField(env, j_dssErrorMessage, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "dssError", "I");
	if (fid) {
		jnumber = (jint)errorStruct.errorNumber;
		(*env)->SetIntField(env, j_dssErrorMessage, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "errorType", "I");
	if (fid) {
		jnumber = (jint)errorStruct.errorType;
		(*env)->SetIntField(env, j_dssErrorMessage, fid, jnumber);
	}
	
	fid = (*env)->GetFieldID (env, cls, "systemError", "I");
	if (fid) {
		jnumber = (jint)errorStruct.systemError;
		(*env)->SetIntField(env, j_dssErrorMessage, fid, jnumber);
	}

	fid = (*env)->GetFieldID (env, cls, "lastAddress", "J");
	if (fid) {
		j_long = (jlong)errorStruct.lastAddress;
		(*env)->SetLongField(env, j_dssErrorMessage, fid, j_long);
	}


    fid = (*env)->GetFieldID (env, cls, "errorMessage", "Ljava/lang/String;");
    if (fid) {
		j_message = (*env)->NewStringUTF(env, errorStruct.errorMessage);
		(*env)->SetObjectField (env, j_dssErrorMessage, fid, j_message);
	}

	fid = (*env)->GetFieldID (env, cls, "systemErrorMessage", "Ljava/lang/String;");
    if (fid) {
		j_message = (*env)->NewStringUTF(env, errorStruct.systemErrorMessage);
		(*env)->SetObjectField (env, j_dssErrorMessage, fid, j_message);
	}

	fid = (*env)->GetFieldID (env, cls, "dssFileName", "Ljava/lang/String;");
    if (fid) {		
		j_dssFileName = (*env)->NewStringUTF(env, errorStruct.filename);
		(*env)->SetObjectField (env, j_dssErrorMessage, fid, j_dssFileName);
	}

	fid = (*env)->GetFieldID (env, cls, "lastPathname", "Ljava/lang/String;");
    if (fid) {		
		j_name = (*env)->NewStringUTF(env, errorStruct.lastPathname);
		(*env)->SetObjectField (env, j_dssErrorMessage, fid, j_name);
	}

	if (errorStruct.functionID > 0) {
		fid = (*env)->GetFieldID (env, cls, "functionName", "Ljava/lang/String;");
		if (fid) {
			name = zgetFunctionName(errorStruct.functionID);
			j_name = (*env)->NewStringUTF(env, name);
			(*env)->SetObjectField (env, j_dssErrorMessage, fid, j_name);
		}
	}
		
	if (errorStruct.calledByFunction > 0) {
		fid = (*env)->GetFieldID (env, cls, "calledByFunction", "Ljava/lang/String;");
		if (fid) {
			name = zgetFunctionName(errorStruct.calledByFunction);
			j_name = (*env)->NewStringUTF(env, name);
			(*env)->SetObjectField (env, j_dssErrorMessage, fid, j_name);
		}
	}

	//  Return error type
	return (jint)errorStruct.errorType;
}


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zerrorCode
    (JNIEnv *env, jobject obj, jintArray j_ifltab)
{
    int *ifltab;
	int ierr;

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    ierr = zerrorCode((long long*)ifltab);   
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
	return (jint)ierr;
}
/*
fix me - write a zerrorStatus()
int zerrorStatus(StringContainer sc)
returns error severity and puts message in sc
*/
