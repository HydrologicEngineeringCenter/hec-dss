#ifdef _MSC_VER

#include <string.h>
#include <jni.h>
#include <stdio.h>

void toshef_ (int *iunitShef, int *iunitLog, const char *cpath,  
		int *intl, float *data, int *itime, int *ndata,
		const char *cunits, 
		size_t lencpath, size_t lencunits);


JNIEXPORT void JNICALL Java_hec_heclib_dss_Sheflib_Hec_1ToShef
    (JNIEnv *env, jobject obj, jint j_iunitShef, jint j_iunitLog, 
	jstring j_cpath, jint j_intl, jfloatArray j_data, jintArray j_itime,
	jint j_ndata, jstring j_cunits)
{

    int iunitShef;
	int iunitLog;
	const char *cpath;
    int intl;
	float *data;
	int *itime;
	int ndata;
    const char *cunits;

	iunitShef = (int) j_iunitShef;
	iunitLog = (int) j_iunitLog;
    cpath = (const char *) (*env)->GetStringUTFChars (env, j_cpath, 0);
	intl = (int) j_intl;
	data = (*env)->GetFloatArrayElements (env, j_data, 0);
	itime = (*env)->GetIntArrayElements (env, j_itime, 0);
	ndata = (int) j_ndata;
	cunits = (const char *) (*env)->GetStringUTFChars (env, j_cunits, 0);

    toshef_ (&iunitShef, &iunitLog, cpath, &intl, data, itime, 
		&ndata, cunits,		
		strlen (cpath), strlen(cunits));

    /* Release the file name */
    (*env)->ReleaseStringUTFChars (env, j_cpath, cpath);
	(*env)->ReleaseStringUTFChars (env, j_cunits, cunits);	
    (*env)->ReleaseFloatArrayElements (env, j_data, data, 0);
    (*env)->ReleaseIntArrayElements (env, j_itime, itime, 0);

}

#endif

