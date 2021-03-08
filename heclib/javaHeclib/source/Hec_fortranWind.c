#include <jni.h>
#include "heclib.h"

void wind_(int *iunit);

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1fortranWind
  (JNIEnv *env, jobject obj, jint j_unit) {

    int   unit;
 
    unit = (int) j_unit;
	wind_ (&unit);
}
