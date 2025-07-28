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


/// <summary>
///  gets the boolean value from a field
/// </summary>
/// <param name="env">JNI environment</param>
/// <param name="cls">Java class </param>
/// <param name="obj">instance of Java Class</param>
/// <param name="name">name of field</param>
/// <param name="defaultValue">value to return in case of an error</param>
/// <returns></returns>
int hec_dss_jni_getBooleanFieldValue(JNIEnv* env, jclass cls, jobject obj, 
																		  const char* name, int defaultValue){
	
	jfieldID fid = (*env)->GetFieldID(env, cls, name, "Z");

	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		fprintf(stderr, "Error finding field name '%s' in JNI int hec_dss_jni_getBooleanFieldValue(JNIEnv* env, jclass cls, jobject obj, \n", name);
		return defaultValue;
	}
	else if (fid) {
		jboolean jbool = (*env)->GetBooleanField(env, obj, fid);
		if (jbool) {
			return 1;
		}
		else {
			return 0;
		}
	}
	return defaultValue;
}

void hec_dss_jni_setBooleanField(JNIEnv* env, jclass cls, 
		                           jobject obj, const char* name, int value) {

	jfieldID fid = (*env)->GetFieldID(env, cls, name, "Z");

	if (fid) {
		(*env)->SetBooleanField(env, obj, fid, (jboolean)value);
	}
}