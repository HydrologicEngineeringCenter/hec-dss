#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zaliasRemove(
	JNIEnv      *env, 
	jobject     obj, 
	jintArray   j_ifltab,
	jstring		j_aliasPath,
	jboolean	j_removeAll)

  {

    int status;
    int *ifltab;

	const char *aliasPath;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	status = 0;
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);	
	aliasPath = (char *) (*env)->GetStringUTFChars (env, j_aliasPath, 0);

	if ((int)j_removeAll) {
		status = zaliasRemoveAll((long long *)ifltab, aliasPath);
	}
	else {
		status = zaliasRemove((long long *)ifltab, aliasPath);
	}

	(*env)->ReleaseStringUTFChars(env, j_aliasPath,  aliasPath);
    (*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}
