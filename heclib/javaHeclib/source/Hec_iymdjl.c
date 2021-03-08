#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1iymdjl
  (JNIEnv *env, jobject obj, jint j_year, jint j_month, jint j_day)
{
    int year, month, day;

    year = (int) j_year;
    month = (int) j_month;
    day = (int) j_day;

    return iymdjl_ (&year, &month, &day);
}
