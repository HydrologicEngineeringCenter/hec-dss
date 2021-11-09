#if defined(__linux__)
#define _LARGEFILE64_SOURCE 
#endif 
#ifdef _MSC_VER
#include <io.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif
#include <jni.h>
#include <stdio.h>

#include "heclib.h"


JNIEXPORT int JNICALL Java_hec_heclib_util_Heclib_Hec_1readf
    (JNIEnv *env, jobject obj, jint j_handle, jintArray j_array, 
	jlong j_address, jint j_numberWords)
{
	int handle;
	void *iarray;
	long long address;
	long long jpos;	
	int numberWords;
	int ntrans;
	int nbytes;
	int istat;

	//   int  Hec_readf(int handle, int array[], long address, int numberWords);
    
    handle = (int) j_handle;
	address = (long long) j_address * (long long)8;
	numberWords = (int) j_numberWords;

#ifdef _MSC_VER
	jpos = _lseeki64(handle, address, 0);
#elif __APPLE__
    jpos = lseek(handle, address, 0);
#else
	jpos = lseek64(handle, address, 0);
#endif
    istat = ((jpos == -1) ? -1 : 0);
	if (istat != 0)
		return istat;

	iarray = (void *)(*env)->GetIntArrayElements (env, j_array, 0);

	nbytes = numberWords * 4;
#ifdef _MSC_VER
	ntrans = _read (handle, iarray, nbytes);
#else
	ntrans = read(handle, iarray, nbytes);
#endif
    istat  = ((ntrans >= 0) ? 0 : -1);
	
	(*env)->ReleaseIntArrayElements (env, j_array, iarray, 0);

	return istat;

}
