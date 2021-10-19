#include <jni.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1dssCopyStatus
  (JNIEnv *env, jobject obj, jintArray j_numberRecs, jintArray j_numberPaths,
   jintArray j_currentRec, jintArray j_currentPath) {

    long *numberRecs;
    long *numberPaths;
    long *currentRec;
    long *currentPath;

    numberRecs =  (*env)->GetIntArrayElements (env, j_numberRecs,  0);
	numberPaths = (*env)->GetIntArrayElements (env, j_numberPaths, 0);
	currentRec =  (*env)->GetIntArrayElements (env, j_currentRec,  0);
	currentPath = (*env)->GetIntArrayElements (env, j_currentPath, 0);

	dsscopystatus_((int *)numberRecs, (int *)numberPaths, (int *)currentRec, 
		           (int *)currentPath);

	(*env)->ReleaseIntArrayElements (env, j_numberRecs , numberRecs,  0);
	(*env)->ReleaseIntArrayElements (env, j_numberPaths, numberPaths, 0);
	(*env)->ReleaseIntArrayElements (env, j_currentRec,  currentRec,  0);
	(*env)->ReleaseIntArrayElements (env, j_currentPath, currentPath, 0);
	  
}
