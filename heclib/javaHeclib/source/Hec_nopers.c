#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1nopers
  (JNIEnv *env, jobject obj, jint j_intl, jint j_juls,
   jint j_istime, jint j_jule, jint j_ietime)
{

    int intl,flag,juls,istime,jule,ietime;

    intl = (int) j_intl;
    flag = 0;
    juls = (int) j_juls;
    istime = (int) j_istime;
    jule = (int) j_jule;
    ietime = (int) j_ietime;

    return nopers_ (&intl, &flag, &juls, &istime, &jule, &ietime);

}
