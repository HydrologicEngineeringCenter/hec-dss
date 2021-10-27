#include <jni.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1closescratchdsscatalog
    (JNIEnv *env, jobject obj, jintArray j_catalogUnit)
{
    int *catalogUnit;


    catalogUnit = (*env)->GetIntArrayElements (env, j_catalogUnit, 0);
	closescratchdsscatalog_ (catalogUnit);
    (*env)->ReleaseIntArrayElements (env, j_catalogUnit, catalogUnit, 0);
}
