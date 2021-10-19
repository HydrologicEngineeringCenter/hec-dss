#include <jni.h>
#include "heclib.h"

void flush_(int *iunit);

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1fortranFlush
  (JNIEnv *env, jobject obj, jint j_unit) {

    int   unit;        
    unit = (int) j_unit;
	flush_ (&unit);           
}
