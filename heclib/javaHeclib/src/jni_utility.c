#include "jni_utility.h"
#include <string.h>
#include "heclib.h"

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

void hec_dss_jni_getStringField(JNIEnv* env, jclass cls, jobject obj, const char* name,
	char* buffer, size_t bufferSize) {
	// Initialize to empty string by default
	buffer[0] = '\0';

	jfieldID fid = (*env)->GetFieldID(env, cls, name, "Ljava/lang/String;");
	if ((*env)->ExceptionOccurred(env)) { // field may not exist
		fprintf(stderr, "Error finding field name '%s' in JNI getStringField\n", name);
		(*env)->ExceptionClear(env);
		return;
	}

	if (!fid) {
		return;
	}

	jstring jstr = (*env)->GetObjectField(env, obj, fid);
	if (!jstr) {
		return;
	}

	const char* cstr = (*env)->GetStringUTFChars(env, jstr, NULL);
	if (cstr) {
		// Copy string to buffer with size limit
		stringCopy(buffer, bufferSize, cstr, strlen(cstr));
		buffer[bufferSize - 1] = '\0'; // Ensure null termination

		// Release resources
		(*env)->ReleaseStringUTFChars(env, jstr, cstr);
	}

	(*env)->DeleteLocalRef(env, jstr);
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
int hec_dss_jni_getBooleanField(JNIEnv* env, jclass cls, jobject obj, 
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
	if ((*env)->ExceptionOccurred(env)) { // field may not exist
		fprintf(stderr, "Error finding field name '%s' in hec_dss_jni_setBooleanField\n", name);
		(*env)->ExceptionClear(env);
		return;
	}
	if (fid) {
		(*env)->SetBooleanField(env, obj, fid, (jboolean)value);
	}
}

void hec_dss_jni_setIntField(JNIEnv* env, jclass cls, jobject obj, const char* name, int value) {
	jfieldID fid = (*env)->GetFieldID(env, cls, name, "I");
	if ((*env)->ExceptionOccurred(env)) { // field may not exist
		fprintf(stderr, "Error finding field name '%s' in hec_dss_jni_setIntField\n", name);
		(*env)->ExceptionClear(env);
		return;
	}

	if (fid) {
		(*env)->SetIntField(env, obj, fid, value);
	}
}