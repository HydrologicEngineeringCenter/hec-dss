#include <jni.h>
#include "heclib.h"


/*
*	The purpose of zcloseReopen is to reset a DSS file's access to 
*	single-user advisory, if the file has gone into a multi-user mode.
*	This will allow other processes that are writing to go into 
*	single user mode and make their writes fast again.
*
*	If force is false, then it will not reset the file
*	if not in multi-user access mode, or it has been written to 
*	by you in the last 5 seconds.
*
*	This function should be called either at the end of a bunch of writes,
*	or on a thread that calls it not more often than once every 5 seconds.
*
*/
JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zcloseReopen(
	JNIEnv       *env,
	jobject       obj,
	jintArray     j_ifltab,		                                              
	jboolean	  j_force)
{
    int *ifltab;
	int force;
	int status;

	jint capacity = 40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
	if ((int)j_force) {
		force = 1;
	}
	else {
		force = 0;
	}
	
	status = zcloseReopen((long long*)ifltab, force);

    /* Release the file table */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);

	return (jint)status;
}
