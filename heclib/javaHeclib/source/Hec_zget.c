#include <jni.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zget
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jlong j_address,
     jint j_numberWords, jlongArray j_arrayVals)
{
	//  Only for debug, internal use!
	int	*ifltab;
	long long	address;
	int		numberWords;  //  8 byte words!
	long	*arrayVals;
	int		flag;
	int		iadd;
	

    ifltab		= (*env)->GetIntArrayElements (env, j_ifltab, 0);
	address		= (long long) j_address;
	numberWords = (int) j_numberWords;
	arrayVals	= (long *)(*env)->GetLongArrayElements (env, j_arrayVals, 0);
	flag		= 0;

	if (zgetVersion((long long*)ifltab) == 6) {
		iadd = (int)address;
		zgtrec6_((long long*)ifltab, (int *)arrayVals, &numberWords, &iadd, &flag);
	}
	else {
		zget((long long*)ifltab, address, (int *)arrayVals, numberWords, 2);		
	}

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseLongArrayElements (env, j_arrayVals, (jlong *)arrayVals, 0);
}
