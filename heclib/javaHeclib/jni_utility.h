#pragma once
#include <jni.h>

void hec_dss_jni_setStringField(JNIEnv* env, jclass cls, jobject obj, const char* name, const char* value);
