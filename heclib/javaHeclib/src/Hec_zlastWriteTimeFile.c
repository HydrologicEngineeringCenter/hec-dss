#include <jni.h>
#include <string.h>
#include "heclib.h"


//  Gets the last write time of the file, in milliseconds since 01 Jan 1970 (sys time)


JNIEXPORT jlong JNICALL Java_hec_heclib_util_Heclib_Hec_1zlastWriteTimeFile(
	JNIEnv       *env,
	jobject       obj, 
	jintArray	j_ifltab)
{
    long long longTime;
	int *ifltab;

	jint capacity=40;
	if (!j_ifltab) return -1;

	(*env)->EnsureLocalCapacity(env, capacity);

	ifltab		= (*env)->GetIntArrayElements(env, j_ifltab, 0);	

	longTime = zgetLastWriteTimeFile((long long *)ifltab);

    (*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return longTime;
}