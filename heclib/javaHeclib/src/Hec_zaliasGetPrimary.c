#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zaliasGetPrimary(
	JNIEnv      *env, 
	jobject     obj, 
	jintArray   j_ifltab,
	jstring		j_aliasPath,
	jobject		j_primaryPath)

  {

    int status;
    int *ifltab;
	const char *aliasPath;
	char primaryPath[400];

	jclass cls;
    jfieldID fid;
    jstring jstr;


	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	status = 0;
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);	
	aliasPath = (char *) (*env)->GetStringUTFChars (env, j_aliasPath, 0);

	status = zaliasGetPrimary((long long *)ifltab, aliasPath, primaryPath, sizeof(primaryPath));

	if (status == STATUS_OKAY) {
		cls = (*env)->GetObjectClass (env, j_primaryPath);
		fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
		if (fid != 0) {        
			jstr = (*env)->NewStringUTF(env, primaryPath);
			(*env)->SetObjectField (env, j_primaryPath, fid, jstr);
			(*env)->DeleteLocalRef(env, jstr);
		}
	}

	(*env)->ReleaseStringUTFChars(env, j_aliasPath,  aliasPath);
    (*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}
