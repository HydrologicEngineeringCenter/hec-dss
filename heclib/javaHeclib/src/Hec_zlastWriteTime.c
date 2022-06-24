#include <jni.h>
#include <string.h>
#include "heclib.h"


//  Gets the last write time for a single record, in milliseconds since 01 Jan 1970 (sys time)
//  Returns zero if not found, negative if error

JNIEXPORT jlong JNICALL Java_hec_heclib_util_Heclib_Hec_1zlastWriteTime(
	JNIEnv       *env,
	jobject       obj, 
	jintArray	j_ifltab,
	jobject		j_pathname)
{
    const char *pathname;
    long long longTime;
	int *ifltab;

	if (!j_ifltab) return - 1;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

	ifltab		= (*env)->GetIntArrayElements(env, j_ifltab, 0);	
    pathname    = (*env)->GetStringUTFChars (env, j_pathname, 0);

	longTime = zgetLastWriteTimeRec((long long *)ifltab, pathname);

	//printf("\n\n************* last write time = %I64d\n\n", longTime);

    (*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	(*env)->ReleaseStringUTFChars(env, j_pathname,  pathname);

	return longTime;
}