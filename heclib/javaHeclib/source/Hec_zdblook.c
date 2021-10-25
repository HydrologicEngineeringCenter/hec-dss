#include <string.h>
#include <jni.h>
#include <stdio.h>
#include <stdlib.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zdblook
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jlong j_address,
	 jint j_length)
{
    int *ifltab;
    int address;
	int length;
	long long *larray;
	long long laddress;
    
    ifltab   = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    address  = (int) j_address;
	laddress = (long long) j_address;
	length   = (int) j_length;

	if (zgetVersion((long long*)ifltab) == 6) {
		zdblook6_ ((long long*)ifltab, &address, &length);
	}
	else {
		//   FIX ME - This address is int 4, not int 8!!!!!
		larray = (long long*)malloc(length * 2 * sizeof(int));
		zget((long long*)ifltab, laddress, (int *)larray, length, 2);	
		zdebugout7_ ((long long*)ifltab, larray, (long long *)laddress, &length);	
		free(larray);
	}

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);   
}
