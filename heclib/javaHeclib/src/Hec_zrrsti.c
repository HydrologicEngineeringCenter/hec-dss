#include <jni.h>
#include <string.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zrrsti(
	JNIEnv       *env, 
	jobject       obj, 
	jintArray     j_ifltab, // file table                                                
	jbyteArray    j_cpath,  // pathname                                                  
	jint          j_ifcat,  // force a new catalog (0 = no, 1 = if needed, >1 = yes)
	jintArray     j_iintrp, // interpolation behavior
	jintArray     j_iuflow, // underflow behavior
	jintArray     j_ioflow, // overflow behavior
	jbooleanArray j_lfound, // series info record found?
	jint          j_ktimes, // dimension of itimes
	jintArray     j_ntimes, // number if itimes set
	jintArray     j_itimes, // array of effective times
	jintArray     j_istat)  // success/failure status                                    
{

	const char *cpath;

	unsigned char *lfound;

	int *ifltab;
	int ifcat, *iintrp, *iuflow, *ioflow, _lfound = 0, ktimes, *ntimes, *itimes, 
		*istat;
	int len;

	//------------------//
	// attach variables //
	//------------------//
	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);
	cpath  = (*env)->GetStringUTFChars(env, j_cpath,  0);
	ifcat  = (int)j_ifcat;
	iintrp = (*env)->GetIntArrayElements(env, j_iintrp, 0);
	iuflow = (*env)->GetIntArrayElements(env, j_iuflow, 0);
	ioflow = (*env)->GetIntArrayElements(env, j_ioflow, 0);
	lfound = (*env)->GetBooleanArrayElements(env, j_lfound, 0);
	ktimes = (int)j_ktimes;
	ntimes = (*env)->GetIntArrayElements(env, j_ntimes, 0);
	itimes = (*env)->GetIntArrayElements(env, j_itimes, 0);
	istat  = (*env)->GetIntArrayElements(env, j_istat, 0);

	//---------------//
	// make the call //
	//---------------//
	len = (int)ifcat;
	zrrsti_((long long*)ifltab, cpath, &len, iintrp, iuflow, ioflow, &_lfound, &ktimes, ntimes, 
			itimes, istat, strlen(cpath));
  
	lfound[0] = _lfound ? JNI_TRUE : JNI_FALSE;
	
	//-------------------//
	// release variables //
	//-------------------//
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);
	(*env)->ReleaseIntArrayElements(env, j_iintrp, iintrp, 0);
	(*env)->ReleaseIntArrayElements(env, j_iuflow, iuflow, 0);
	(*env)->ReleaseIntArrayElements(env, j_ioflow, ioflow, 0);
	(*env)->ReleaseBooleanArrayElements(env, j_lfound, lfound, 0);
	(*env)->ReleaseIntArrayElements(env, j_ntimes, ntimes, 0);
	(*env)->ReleaseIntArrayElements(env, j_itimes, itimes, 0);
	(*env)->ReleaseIntArrayElements(env, j_istat, istat, 0);
}
