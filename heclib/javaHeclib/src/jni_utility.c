#include "jni_utility.h"

void hec_dss_jni_setStringField(JNIEnv *env, jclass cls, jobject obj, const char* name, const char* value) {
	jstring jstr;
	jfieldID fid = (*env)->GetFieldID(env, cls, name, "Ljava/lang/String;");
	if ((*env)->ExceptionOccurred(env)) { // field may not exist
		fprintf(stderr, "Error finding field name '%s' in JNI setStringField\n",name );
		(*env)->ExceptionClear(env);
		return;
	}

	if (fid) {
		if (value) {
			jstr = (*env)->NewStringUTF(env,value);
		}
		else {
			jstr = (*env)->NewStringUTF(env, "");
		}
		(*env)->SetObjectField(env, obj, fid, jstr);
		(*env)->DeleteLocalRef(env, jstr);

	}
}