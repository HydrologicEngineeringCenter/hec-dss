#include <jni.h>
#include <stdio.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zustfh
    (JNIEnv *env, jobject obj, 
     jobjectArray j_labels, jobjectArray j_items, jint j_numberItems, 
     jintArray j_ipos, jintArray j_header, jint j_numberHeader, 
     jintArray j_istat) {

	char clabels[50][25]; /* 50 labels, max 24 chars each */
    char citems[50*25];
    int  numberItems;
    int *ipos;
	int *header;
	int  numberHeader;
	int *istat;
 
    char *temp;
    char ctemp[50];
    int i;
    int j;
    jstring jstr;

	/*jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);*/

    numberItems = (int)j_numberItems;
    if (numberItems > 50)
        numberItems = 50;
    for  (i=0; i<numberItems; i++) {
        for (j=0; j<25; j++) clabels[i][j] = ' ';
        jstr = (*env)->GetObjectArrayElement(env, j_labels, i);
        temp = (char *)(*env)->GetStringUTFChars (env, jstr, 0);
        if(jstr) strncpy(clabels[i], temp, strlen(temp));
        (*env)->ReleaseStringUTFChars (env, jstr, temp);
    }

    ipos         = (*env)->GetIntArrayElements (env, j_ipos, 0);    
    header       = (*env)->GetIntArrayElements (env, j_header, 0);
    numberHeader = (int)j_numberHeader;
    istat        = (*env)->GetIntArrayElements (env, j_istat, 0);

    zustfh_ ((char *)clabels, citems, &numberItems, 
             ipos, header, &numberHeader, istat,
			 sizeof(clabels[0]), 24);


    for (i=0; i<numberItems; i++) {
        strncpy(ctemp, &citems[i*24], 24);
        ctemp[24] = '\0';
        jstr = (*env)->NewStringUTF(env, ctemp);
        (*env)->SetObjectArrayElement(env, j_items, i, jstr); 
    }
     
    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_header, header, 0);
    (*env)->ReleaseIntArrayElements (env, j_ipos, ipos, 0);
    (*env)->ReleaseIntArrayElements (env, j_istat, istat, 0);

}
