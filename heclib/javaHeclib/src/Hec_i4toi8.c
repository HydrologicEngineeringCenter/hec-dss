#include <jni.h>
#include "heclib.h"

JNIEXPORT long long JNICALL Java_hec_heclib_util_Heclib_Hec_1i4toi8
  (JNIEnv *env, jobject obj, jint int4a, jint int4b)
{
	//_int64 i4toi8(int int4a, int int4b);
    return i4toi8(int4a, int4b);
}

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1i8toi4
  (JNIEnv *env, jobject obj, jlong integer8, jintArray j_int4a, jintArray j_int4b)
{
	int *int4a, *int4b;

    int4a = (*env)->GetIntArrayElements (env, j_int4a, 0);
    int4b = (*env)->GetIntArrayElements (env, j_int4b, 0);

	//void i8toi4(_int64 integer8, int *int4a, int *int4b);
    i8toi4(integer8, int4a, int4b);

	(*env)->ReleaseIntArrayElements (env, j_int4a,  int4a, 0);
	(*env)->ReleaseIntArrayElements (env, j_int4b,  int4b, 0);
}
