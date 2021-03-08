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


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zwhatChanged(
    JNIEnv       *env,
	jobject       obj, 
	jintArray     j_ifltab,		 // file table (Access in java)                                               
	jobject		  j_dssCatalog
	)
{
	zStructCatalog *catStruct;
	int status;
	int *ifltab;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);


	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zwhatChanged ", "");
	}

	catStruct = zstructCatalogNew();
	status = zwhatChanged((long long*)ifltab, catStruct);
	
	if (zisError(status)) {
		free(catStruct);
		return status;
	}

	C_CatalogToJava(env, obj, j_dssCatalog , catStruct);	
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	status = catStruct->numberPathnames;
	zstructFree(catStruct);

	return (jint)status;
}
