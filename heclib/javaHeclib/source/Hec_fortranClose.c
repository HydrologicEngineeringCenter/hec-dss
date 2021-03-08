#include <string.h>
#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1fortranClose
  (JNIEnv *env, jobject obj, jint j_unit) {

    int   unit;    
    int   status;
    
    unit = (int) j_unit;
	status = fortranclose_ (&unit);       
    return status;
}
