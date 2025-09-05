#include <jni.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zgtrec
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jlong j_address,
     jint j_numberWords, jintArray j_arrayVals)
{
	//  Only for debug, internal use!
	int	*ifltab;
	long long	address;
	int		numberWords;  //  4 byte words!
	int		*arrayVals;
	int		flag;
	long long  nada=0;
	int		iadd;
	

  ifltab		= (*env)->GetIntArrayElements (env, j_ifltab, 0);
	address		= (long long) j_address;
	numberWords = (int) j_numberWords;
	arrayVals	= (*env)->GetIntArrayElements (env, j_arrayVals, 0);
	flag		= 0;

	zget((long long*)ifltab, address, arrayVals, numberWords, 1);		

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseIntArrayElements (env, j_arrayVals, arrayVals, 0);
}
