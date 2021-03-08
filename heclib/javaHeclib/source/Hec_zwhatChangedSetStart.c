#include <jni.h>
#include <string.h>
#include <stdio.h>

#include "javaHeclib.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "zdssMessages.h"
#include "heclib.h"



JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zwhatChangedSetStart(
    JNIEnv		*env,
	jobject		obj, 
	jintArray	j_ifltab,		 // file table (Access in java) 
	jobject		j_dssCatalog,
	jstring		j_pathnameWithWild,
	jboolean	j_boolUseCRC
	)
{
	int status;
	int boolUseCRC;
	int *ifltab;
	char *pathnameWithWild;
	zStructCatalog *catStruct;


	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);
	boolUseCRC = (int)j_boolUseCRC;
	if (j_pathnameWithWild) {
		pathnameWithWild = (char *)(*env)->GetStringUTFChars (env, j_pathnameWithWild, 0);
	}
	else {
		pathnameWithWild = (char *)calloc(1, 1);
	}
	
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zwhatChangedSetStart ", "");
	}

	if (j_dssCatalog) {
		catStruct = zstructCatalogNew();
		JavaCatalogToC(env, obj, (long long*)ifltab, j_dssCatalog, catStruct, 1);
	}
	else {
		catStruct = (zStructCatalog *)0;
	}

	status = zwhatChangedSetStart((long long *)ifltab, catStruct, (const char *)pathnameWithWild, boolUseCRC);

	if (j_dssCatalog) {
		C_CatalogToJava(env, obj, j_dssCatalog , catStruct);
		zstructFree(catStruct);
	}

	if (j_pathnameWithWild) {
		(*env)->ReleaseStringUTFChars (env, j_pathnameWithWild, pathnameWithWild);
	}
	else {
		free(pathnameWithWild);
	}	
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	
	return (jint)status;
}
