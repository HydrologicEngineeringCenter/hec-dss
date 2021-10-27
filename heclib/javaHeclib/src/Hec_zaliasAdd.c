#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zaliasAdd(
	JNIEnv      *env, 
	jobject     obj, 
	jintArray   j_ifltab,
	jstring		j_primaryPath,
	jstring		j_aliasPath)

  {

    int status;
    int *ifltab;

	const char *primaryPath;
	const char *aliasPath;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	status = 0;
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);	
    primaryPath = (char *) (*env)->GetStringUTFChars (env, j_primaryPath, 0);
	aliasPath = (char *) (*env)->GetStringUTFChars (env, j_aliasPath, 0);

	status = zaliasAdd((long long*)ifltab, primaryPath, aliasPath);

	(*env)->ReleaseStringUTFChars(env, j_primaryPath,  primaryPath);
	(*env)->ReleaseStringUTFChars(env, j_aliasPath,  aliasPath);
    (*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}
