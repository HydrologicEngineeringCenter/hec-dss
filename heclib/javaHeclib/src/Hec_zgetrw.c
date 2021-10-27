#include <string.h>
#include <jni.h>
#include <stdio.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zgetrw
    (JNIEnv *env, jobject obj, jlong j_iadd, jintArray j_record,
	jintArray j_word)
{
	// iadd is input, rest are output
	long long  iaddLong;
	int		iadd;
	int		*record;
	int		*word;

	iaddLong =  (long long) j_iadd;
	record =	(*env)->GetIntArrayElements (env, j_record, 0);
	word =		(*env)->GetIntArrayElements (env, j_word, 0);

	iadd = (int)iaddLong;
	zgetrw6_ (&iadd, record, word);
    
    /* Release */
	(*env)->ReleaseIntArrayElements (env, j_record, record, 0);
	(*env)->ReleaseIntArrayElements (env, j_word, word, 0);

}
