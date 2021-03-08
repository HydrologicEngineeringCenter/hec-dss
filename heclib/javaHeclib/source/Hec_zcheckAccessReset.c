#include <jni.h>
#include "heclib.h"


/*
*	The purpose of this function is to reset a DSS file's access to 
*	single-user advisory, if the file has gone into a multi-user mode.
*	This will allow other processes that are writing to go into 
*	single user mode and make their writes fast again.
*
*	If force is false, then it will not reset the file
*	if not in multi-user access mode, or it has been written to 
*	by you in the last j_quiescentTimeMills milliseconds.
*
*	This function should be called either at the end of a bunch of writes,
*	or on a thread that calls it not more often than once every 5 seconds.
*  
*/
JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zcheckAccessReset(
	JNIEnv       *env,
	jobject       obj,
	jintArray     j_ifltab,		                                              
	jboolean	  j_force,
	jint		  j_quiescentTimeMills)
{
    int *ifltab;
	int force;
	int status;
	int quiescentTimeMills;

	jint capacity = 40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	if ((int)j_force) {
		force = 1;
	}
	else {
		force = 0;
	}
	quiescentTimeMills = (int)j_quiescentTimeMills;

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
//		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_1zcheckAccessReset, Enter; handle: ", zhandle(ifltab));
	}
	
	status = zcheckAccessReset((long long*)ifltab, force, quiescentTimeMills);

    /* Release the file table */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);

	return (jint)status;
}
