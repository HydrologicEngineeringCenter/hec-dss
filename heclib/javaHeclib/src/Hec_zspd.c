#include <jni.h>
#include <stdio.h>
#include <string.h>
#include "heclib.h"


JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zspd
    (JNIEnv *env, jobject obj, jintArray j_ifltab,
     jstring j_pathname, jint j_nord, jint j_ncurve,
     jint j_ihorz, jobject j_c1unit,
     jobject j_c1type, jobject j_c2unit, jobject j_c2type,
     jfloatArray j_values,
     jobjectArray j_clabel, jboolean j_label, jintArray j_headu,
     jint j_nheadu, jint j_iplan, jintArray j_istat) {

    int *ifltab;
    const char *pathname;
    int nord;
    int ncurve;
    int ihorz;
    const char *c1unit;
    const char *c1type;
    const char *c2unit;
    const char *c2type;
    float *values;
    char *clabel;
	char clabela[1][100];
    char *temp;
    int label = 0;     /* boolean */
    int *headu=0;
    int nheadu;
    int iplan;
    int *istat;
	size_t clabelSize;
	size_t clabelLen;

    int i;
    int j;
	int len;
	int ipos;
    jstring jstr;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab   = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname = (*env)->GetStringUTFChars (env, j_pathname, 0);
    nord     = (int) j_nord;
    ncurve   = (int) j_ncurve;
    ihorz    = (int) j_ihorz;
	istat    = (*env)->GetIntArrayElements (env, j_istat, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zspd, pathname: ", pathname);
	}

	//  Initial error checking....
	if ((nord <= 0) || (ncurve <=0)
		|| j_c1unit == 0
		|| j_c1type == 0
		|| j_c2unit == 0
		|| j_c2type == 0

		) {
		if (zmessageLevel((long long *)ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_TERSE)) {
			zmessage2((long long *)ifltab, "zspd: No data; or null units or type. Can't store empty record.  Pathname: ", pathname);
		}
		istat[0] = -1;
		(*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
		(*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
		(*env)->ReleaseIntArrayElements (env, j_istat, istat, 0);
		return;
	}

	
    c1unit   = (*env)->GetStringUTFChars (env, j_c1unit, 0);
    c1type   = (*env)->GetStringUTFChars (env, j_c1type, 0);
    c2unit   = (*env)->GetStringUTFChars (env, j_c2unit, 0);
    c2type   = (*env)->GetStringUTFChars (env, j_c2type, 0);
    values   = (*env)->GetFloatArrayElements (env, j_values, 0);
    label    = (int) j_label;
    nheadu   = (int) j_nheadu;
    if(nheadu) headu = (*env)->GetIntArrayElements (env, j_headu, 0);
    iplan    = (int) j_iplan;
    

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zspd, pathname: ", pathname);
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "c1unit: ", c1unit);
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "c1type: ", c1type);
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "c2unit: ", c2unit);
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "c2type: ", c2type);
	}

    if(label) {
		if ((iplan == 10) || (iplan == 11)) {
			clabelSize = 100;
			clabel = (char *)malloc(clabelSize);
			jstr = (*env)->GetObjectArrayElement(env, j_clabel, 0);
			if (jstr) {
				temp = (char *)(*env)->GetStringUTFChars (env, jstr, 0);
				len = (int)strlen(temp);	
				stringFill (clabel, ' ', (size_t) clabelSize);
				for (j=0; j<len; j++) {
					clabel[j] = temp[j];
				}
				(*env)->ReleaseStringUTFChars (env, jstr, temp);
			}
		}
		else {
			clabelSize = ncurve * 100;
			clabelLen = (size_t)100;
			clabel = (char *)malloc(clabelSize);
			stringFill (clabel, ' ', (size_t) clabelSize);
			for  (i=0; i<ncurve; i++) {	
				jstr = (*env)->GetObjectArrayElement(env, j_clabel, i);
				if (!jstr) continue;
				temp = (char *)(*env)->GetStringUTFChars (env, jstr, 0);		
				len = (int)strlen(temp);				
				for (j=0; j<len; j++) {
					ipos = (i*100) +j;
					clabel[(i*100) +j] = temp[j];			            
				}
				(*env)->ReleaseStringUTFChars (env, jstr, temp);
			}
		}
	}
	else {
		clabel = (char *)malloc(ncurve);
		clabelLen = (size_t)0;
		clabel[0] = '\0';
    }

	//  FIX ME 
	//  THIS TAKES CARE OF WHAT APPEARS TO BE A MEMORY ISSUE!!!
    for  (i=0; i<1; i++) {
        for (j=0; j<100; j++) clabela[i][j] = ' ';
    }
   
	
	zspd_ ((long long*)ifltab, pathname, &nord, &ncurve,
        &ihorz, c1unit, c1type, c2unit,
        c2type, values, (const char *)clabel,
        &label, headu, &nheadu, &iplan,  istat,
        strlen(pathname), strlen(c1unit), strlen(c1type),
        strlen(c2unit), strlen(c2type), clabelLen);


	if (clabel) free(clabel);

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseStringUTFChars (env, j_c1unit, c1unit);
    (*env)->ReleaseStringUTFChars (env, j_c1type, c1type);
    (*env)->ReleaseStringUTFChars (env, j_c2unit, c2unit);
    (*env)->ReleaseStringUTFChars (env, j_c2type, c2type);
    (*env)->ReleaseFloatArrayElements (env, j_values, values, 0);
    if(nheadu) (*env)->ReleaseIntArrayElements (env, j_headu, headu, 0);
    (*env)->ReleaseIntArrayElements (env, j_istat, istat, 0);

}
