#ifdef _MSC_VER

#include <string.h>
#include <jni.h>
#include <stdio.h>


void runshefdss_ (long long *ifltab, int *unitLog, const char *inputFile,  const char *shefParamFile, 
		const char *sensorFile, const char *paramFile, int *lstoreAll,
		int *ntotal, int *nsets, int *status, 
		size_t leninputFile, size_t lenshefParamFile,
		size_t lensensorFile, size_t lenparamFile);

//hec.heclib.dss.Sheflib.Hec_RunShefDss
// hec.importExport.shef.Sheflib.Hec_RunShefDss 
JNIEXPORT void JNICALL Java_hec_heclib_dss_Sheflib_Hec_1RunShefDss
//JNIEXPORT void JNICALL Java_hec_importExport_shef_Sheflib_Hec_1RunShefDss
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jint j_unitLog, jstring j_inputFile,
	jstring j_shefParamFile, jstring j_sensorFile, jstring j_paramFile,
	jint j_lstoreAll, jintArray j_ntotal, jintArray j_nsets, jintArray j_status)
{

	/*SUBROUTINE RUNSHEFDSS((long long*)ifltab, CINPUT, CSHEFPARM, 
     * CSENSOR, CPARAM, LSTOREALL, ISTATUS)
	 */
	int  unitLog;
    int *ifltab;
    int *ntotal;
	int *nsets;
	int *status;
    const char *inputFile;
    const char *shefParamFile;
	const char *sensorFile;
	const char *paramFile;
	int lstoreAll;


    inputFile = (const char *) (*env)->GetStringUTFChars (env, j_inputFile, 0);
	unitLog = (int) j_unitLog;
	shefParamFile = (const char *) (*env)->GetStringUTFChars (env, j_shefParamFile, 0);
	sensorFile = (const char *) (*env)->GetStringUTFChars (env, j_sensorFile, 0);
	paramFile = (const char *) (*env)->GetStringUTFChars (env, j_paramFile, 0);
	lstoreAll = (int) j_lstoreAll;
    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    ntotal = (*env)->GetIntArrayElements (env, j_ntotal, 0);
	nsets = (*env)->GetIntArrayElements (env, j_nsets, 0);
	status = (*env)->GetIntArrayElements (env, j_status, 0);

    runshefdss_ ((long long*)ifltab, &unitLog, inputFile,  shefParamFile, 
		sensorFile, paramFile,
		&lstoreAll, ntotal, nsets, status, 
		strlen (inputFile), strlen(shefParamFile),
		strlen (sensorFile), strlen(paramFile));

    /* Release the file name */
    (*env)->ReleaseStringUTFChars (env, j_inputFile, inputFile);
	(*env)->ReleaseStringUTFChars (env, j_shefParamFile, shefParamFile);
	(*env)->ReleaseStringUTFChars (env, j_sensorFile, sensorFile);
	(*env)->ReleaseStringUTFChars (env, j_paramFile, paramFile);
    /* Make sure the file table and status are returned! */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseIntArrayElements (env, j_ntotal, ntotal, 0);
	(*env)->ReleaseIntArrayElements (env, j_nsets, nsets, 0);
	(*env)->ReleaseIntArrayElements (env, j_status, status, 0);

}

#endif
