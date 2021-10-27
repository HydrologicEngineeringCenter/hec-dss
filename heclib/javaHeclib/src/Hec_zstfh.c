#include <jni.h>
#include <stdio.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zstfh
    (JNIEnv *env, jobject obj, 
     jobjectArray j_labels, jobjectArray j_items, jint j_numberItems, 
     jfloatArray j_header, jint j_headerMax, jintArray j_numberHeader, 
     jintArray j_istat) {

	char clabels[30][25]; /* 30 labels, max 24 chars each */
    char citems[30][25];
    int  numberItems;
	float *header;
	int  headerMax;
	int *numberHeader;
	int *istat;
 
    char *temp;
    int i;
    int j;
	int max;
    jstring jstr;

	/*jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);*/


    numberItems = (int)j_numberItems;
    if (numberItems > 30)
        numberItems = 30;
    for  (i=0; i<numberItems; i++) {
        for (j=0; j<25; j++) clabels[i][j] = ' ';
        jstr = (*env)->GetObjectArrayElement(env, j_labels, i);
		if(jstr) {
            temp = (char *)(*env)->GetStringUTFChars (env, jstr, 0);
            max = (int)strlen(temp);
			if (max > 24)
				max = 24;
            strncpy(clabels[i], temp, max);
            (*env)->ReleaseStringUTFChars (env, jstr, temp);
		}

        for (j=0; j<25; j++) citems[i][j] = ' ';
        jstr = (*env)->GetObjectArrayElement(env, j_items, i);
		if(jstr) {
            temp = (char *)(*env)->GetStringUTFChars (env, jstr, 0);
			max = (int)strlen(temp);
			if (max > 24)
				max = 24;
            strncpy(citems[i], temp, max);
            (*env)->ReleaseStringUTFChars (env, jstr, temp);
		}
    }


    headerMax    = (int) j_headerMax;
    header       = (*env)->GetFloatArrayElements (env, j_header, 0);
    numberHeader = (*env)->GetIntArrayElements (env, j_numberHeader, 0);
    istat        = (*env)->GetIntArrayElements (env, j_istat, 0);

   zstfh_ ((const char *)clabels, (const char *)citems, &numberItems, 
            header, &headerMax, numberHeader, istat,
			sizeof(clabels[0]), sizeof(citems[0])); 

    /* Release */
    (*env)->ReleaseFloatArrayElements (env, j_header, header, 0);
    (*env)->ReleaseIntArrayElements (env, j_numberHeader, numberHeader, 0);
    (*env)->ReleaseIntArrayElements (env, j_istat, istat, 0);

}
