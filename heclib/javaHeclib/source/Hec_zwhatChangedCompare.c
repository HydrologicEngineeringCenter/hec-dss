#include <jni.h>
#include <string.h>
#include <stdio.h>

#include "javaHeclib.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "zdssMessages.h"
#include "heclib.h"
#include "zerrorCodes.h"

//  Main function to retrieve the catalog from a DSS file
//  The catalog is returned in the Java object dssCatalog
//  Note - sorting is done in Java, not in the C code

//  int zwhatChangedCompare(long long *ifltab, zStructCatalog *catStructBefore, zStructCatalog *catStructChanged, const char *pathWithWild, int boolUseCRC) 


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zwhatChangedCompare(
    JNIEnv		*env,
	jobject		obj, 
	jintArray	j_ifltab,		 // file table (Access in java)                                               
	jobject		j_dssCatalogBefore,
	jobject		j_dssCatalogChanged,
	jstring		j_pathWithWild,
	jboolean	j_boolUseCRC
	)
{
	zStructCatalog *catStructBefore;
	zStructCatalog *catStructChanged;
	const char *pathWithWild;
	int boolUseCRC;
	int status;
	int *ifltab;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);


	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);
	if (j_pathWithWild) {
		pathWithWild = (char *) (*env)->GetStringUTFChars (env, j_pathWithWild, 0);
	}
	else {
		pathWithWild = (char *)0;
	}
	boolUseCRC = (int)j_boolUseCRC;

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zwhatChangedCompare ", "");
	}

	catStructBefore = zstructCatalogNew();
	JavaCatalogToC(env, obj, (long long*)ifltab, j_dssCatalogBefore, catStructBefore, 0);
	catStructChanged = zstructCatalogNew();
	JavaCatalogToC(env, obj, (long long*)ifltab, j_dssCatalogChanged, catStructChanged, 1);

	status = zwhatChangedCompare((long long*)ifltab, catStructBefore, catStructChanged, pathWithWild, boolUseCRC);
	
	if (zisError(status)) {
		free(catStructBefore);
		free(catStructChanged);
		return status;
	}

	C_CatalogToJava(env, obj, j_dssCatalogChanged , catStructChanged);	

	if (j_pathWithWild) {
		(*env)->ReleaseStringUTFChars (env, j_pathWithWild, pathWithWild);
	}
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	zstructFree(catStructBefore);
	zstructFree(catStructChanged);

	return (jint)status;
}
