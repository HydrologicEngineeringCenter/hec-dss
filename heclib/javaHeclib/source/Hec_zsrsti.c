#include <jni.h>
#include <string.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsrsti(
	JNIEnv       *env, 
	jobject       obj, 
	jintArray    j_ifltab, // file table                                                
	jbyteArray    j_cpath,  // pathname                                                  
	jint          j_iintrp, // interpolation behavior
	jint          j_iuflow, // underflow behavior
	jint          j_ioflow, // overflow behavior
	jint          j_iplan,  // overwrite plan
	jintArray     j_istat)  // success/failure status                                    
{

	const char *cpath;

	int *ifltab;
	int iintrp, iuflow, ioflow, iplan, *istat;

	//------------------//
	// attach variables //
	//------------------//
	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);
	cpath  = (*env)->GetStringUTFChars(env, j_cpath,  0);
	iintrp = (int)j_iintrp;
	iuflow = (int)j_iuflow;
	ioflow = (int)j_ioflow;
	iplan  = (int)j_iplan;
	istat  = (*env)->GetIntArrayElements(env, j_istat, 0);

	//---------------//
	// make the call //
	//---------------//
	zsrsti_((long long*)ifltab, cpath, &iintrp, &iuflow, &ioflow, &iplan, istat, strlen(cpath));
  
	//-------------------//
	// release variables //
	//-------------------//
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);
	(*env)->ReleaseIntArrayElements(env, j_istat, istat, 0);
}
