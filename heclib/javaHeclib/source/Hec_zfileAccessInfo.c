#include <jni.h>
#include "heclib.h"

//	int zfileAccessInfo(long long *ifltab, int *accessMode, int *numberAccessing, int *numberWriting,
//					int *pidsArray, int arraySize, int *numberPids);

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zfileAccessInfo
			(JNIEnv *env, jobject obj, 
			jintArray j_ifltab, 
			jintArray j_accessMode,
			jintArray j_numberAccessing,
			jintArray j_numberWriting,
			jintArray j_pidsArray,
			jintArray j_modesArray,
			jint	  j_arraySize,
			jintArray j_numberPids)
			
{
    int *ifltab;
    int *accessMode;
	int *numberAccessing;
	int *numberWriting;
	int *pidsArray;
	int *modesArray;
	int  arraySize;
	int *numberPids;
	int status;


    ifltab =			(*env)->GetIntArrayElements (env, j_ifltab, 0);
    accessMode =		(*env)->GetIntArrayElements (env, j_accessMode, 0);
	numberAccessing =	(*env)->GetIntArrayElements (env, j_numberAccessing, 0);
	numberWriting =		(*env)->GetIntArrayElements (env, j_numberWriting, 0);
	pidsArray =			(*env)->GetIntArrayElements (env, j_pidsArray, 0);
	modesArray =		(*env)->GetIntArrayElements (env, j_modesArray, 0);
	numberPids =		(*env)->GetIntArrayElements (env, j_numberPids, 0);
	arraySize =			(int)j_arraySize;
	
	status = zfileAccessInfo((long long*)ifltab, accessMode, numberAccessing, numberWriting,
							pidsArray, modesArray, arraySize, numberPids);

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseIntArrayElements (env, j_accessMode, accessMode, 0);
	(*env)->ReleaseIntArrayElements (env, j_numberAccessing, numberAccessing, 0);
	(*env)->ReleaseIntArrayElements (env, j_numberWriting, numberWriting, 0);
	(*env)->ReleaseIntArrayElements (env, j_pidsArray, pidsArray, 0);
	(*env)->ReleaseIntArrayElements (env, j_modesArray, modesArray, 0);
	(*env)->ReleaseIntArrayElements (env, j_numberPids, numberPids, 0);

	return(jint)status;
}
