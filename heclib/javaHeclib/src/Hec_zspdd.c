#include <jni.h>
#include <stdio.h>
#include <string.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zspdd
    (JNIEnv *env, jobject obj, jintArray j_ifltab,
     jstring j_pathname, jint j_nord, jint j_ncurve,
     jint j_ihorz, jobject j_c1unit,
     jobject j_c1type, jobject j_c2unit, jobject j_c2type,
     jdoubleArray j_values,
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
	char ctemp[24];
    double *values;
    char *clabel;
    char *temp;
	int itemp;
    int label = 0;     /* boolean */
    int *headu=0;
    int nheadu;
    int iplan;
    int *istat;
	int iprec;
	size_t clabelSize;
	int labelLength;
	zStructPairedData *pds;

    int i;
	int len;
    jstring jstr;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

    ifltab   = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    pathname = (*env)->GetStringUTFChars (env, j_pathname, 0);
    nord     = (int) j_nord;
    ncurve   = (int) j_ncurve;
    ihorz    = (int) j_ihorz;
		istat = (*env)->GetIntArrayElements(env, j_istat, 0);

		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zspdd, pathname: ", pathname);
		}

		//  Initial error checking....
		if ((nord <= 0) || (ncurve <= 0)
			|| j_c1unit == 0
			|| j_c1type == 0
			|| j_c2unit == 0
			|| j_c2type == 0

			) {
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_TERSE)) {
				zmessage2((long long*)ifltab, "Heclib_Hec_zspdd: No data; or null units or type. Can't store empty record.  Pathname: ", pathname);
			}
			istat[0] = -1;
			(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
			(*env)->ReleaseStringUTFChars(env, j_pathname, pathname);
			(*env)->ReleaseIntArrayElements(env, j_istat, istat, 0);
			return;
		}


    c1unit   = (*env)->GetStringUTFChars (env, j_c1unit, 0);
    c1type   = (*env)->GetStringUTFChars (env, j_c1type, 0);
    c2unit   = (*env)->GetStringUTFChars (env, j_c2unit, 0);
    c2type   = (*env)->GetStringUTFChars (env, j_c2type, 0);
    values   = (*env)->GetDoubleArrayElements (env, j_values, 0);
    label    = (int) j_label;
    nheadu   = (int) j_nheadu;
    if(nheadu) headu = (*env)->GetIntArrayElements (env, j_headu, 0);
    iplan    = (int) j_iplan;
    


	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Heclib_Hec_zspdd, pathname: ", pathname);
	}

    if (label) {
		if (iplan == 11) {
			clabelSize = 100;
			clabel = (char *)malloc(clabelSize);
			jstr = (*env)->GetObjectArrayElement(env, j_clabel, 0);
			temp = (char *)(*env)->GetStringUTFChars (env, jstr, 0);
			len = (int)strlen(temp);	
			stringFill (clabel, ' ', (size_t) clabelSize);
			//for (j=0; j<len; j++) clabel[j] = temp[j];
			(*env)->ReleaseStringUTFChars (env, jstr, temp);
		}
		else {
			clabelSize = ncurve * 100;
			clabel = (char *)calloc(clabelSize, 1);
			labelLength = 0;
			for  (i=0; i<ncurve; i++) {				
				jstr = (*env)->GetObjectArrayElement(env, j_clabel, i);
				temp = (char *)(*env)->GetStringUTFChars (env, jstr, 0);		
				len = (int)strlen(temp);				
				stringCopy(&clabel[labelLength], clabelSize-labelLength, temp, len);
				labelLength += len + 1;	            
				(*env)->ReleaseStringUTFChars (env, jstr, temp);
			}
		}
	}
	else {
		clabel = (char *)malloc(2);
		clabel[0] = ' ';
		clabel[1] = '\0';
		clabelSize = 1;
    }

	pds = zstructPdNewDoubles(pathname, values, &values[nord], 
							 nord, ncurve, 
							 c1unit, c1type, 
							 c2unit, c2type);
	if (nheadu) {
		pds->userHeader = headu;
		pds->userHeaderNumber = nheadu;
	}

	if (label) {
		pds->labels = clabel;
		pds->labelsLength = labelLength;
	}
	else {
	}

	//  Set precision.  First, get it from the common block
	zgettimezone6_(&itemp, ctemp, &iprec, sizeof(ctemp));
	//iprec =-1;
	if (iprec < 0) {
		pds->xprecision = -1;
		pds->yprecision = -1;
	}
	else {
		pds->xprecision = iprec/10;
		pds->yprecision = iprec - (pds->xprecision * 10);
	}

	*istat = zpdStore((long long*)ifltab, pds, 0);

	if (clabel) free(clabel);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Heclib_Hec_zspdd, pathname: ", pathname);
	}

    /* Release */
	zstructFree(pds);
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_pathname, pathname);
    (*env)->ReleaseStringUTFChars (env, j_c1unit, c1unit);
    (*env)->ReleaseStringUTFChars (env, j_c1type, c1type);
    (*env)->ReleaseStringUTFChars (env, j_c2unit, c2unit);
    (*env)->ReleaseStringUTFChars (env, j_c2type, c2type);
    (*env)->ReleaseDoubleArrayElements (env, j_values, values, 0);
    if(nheadu) (*env)->ReleaseIntArrayElements (env, j_headu, headu, 0);
    (*env)->ReleaseIntArrayElements (env, j_istat, istat, 0);

}
