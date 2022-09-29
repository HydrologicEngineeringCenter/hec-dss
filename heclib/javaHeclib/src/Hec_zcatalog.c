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


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zcatalog(
    JNIEnv       *env,
	jobject       obj, 
	jintArray     j_ifltab,		 // file table (Access in java)                                               
	jobject		  j_dssCatalog
	)
{
	zStructCatalog *catStruct;
	int status;
	int getCollectionFlag=0;
	int *ifltab;
	int number;
	int maxNumber;
	int i;
	int len;
	int jpos;
	char *pos;
	char *path;

	jclass cls;
	jfieldID fid;
	jboolean jbool;
	jobjectArray stringArray;
	jclass stringClass;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	
	
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zcatalog ", "");
	}
	

	number = (int)zinquire((long long*)ifltab, "nrec");
	if (number == 0) {
		cls = (*env)->GetObjectClass(env, j_dssCatalog);
		stringClass = (*env)->FindClass(env, "Ljava/lang/String;");
		stringArray = (*env)->NewObjectArray(env, 0, stringClass, 0);		
		fid = (*env)->GetFieldID(env, cls, "pathnameList", "[Ljava/lang/String;");
		if (fid) {
			(*env)->SetObjectField(env, j_dssCatalog, fid, stringArray);
		}
		(*env)->DeleteLocalRef(env, stringArray);
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return (jint)0;
	}

	catStruct = zstructCatalogNew();
	JavaCatalogToC(env, obj, (long long*)ifltab, j_dssCatalog, catStruct, 1);
	
	catStruct->boolIncludeDates = 1;

	cls = (*env)->GetObjectClass (env, j_dssCatalog);
	fid = (*env)->GetFieldID (env, cls, "listIsCollection", "Z");
	if (fid) {
		jbool = (*env)->GetBooleanField(env, j_dssCatalog, fid);
		if (jbool) {
			getCollectionFlag = 2;
		}
		else {
			getCollectionFlag = 0;
		}
	}

	maxNumber = 0;
	fid = (*env)->GetFieldID(env, cls, "maxNumberToRetrieve", "I");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
	}
	else if (fid) {
		number = (int)(*env)->GetIntField(env, j_dssCatalog, fid);
		if (number > 0) {
			maxNumber = number;
		}
	}

	//status = zcatalog((long long*)ifltab, catStruct->pathWithWildChars, catStruct, getCollectionFlag);
	
	if (getCollectionFlag) {
		status = zcatInternalSort ((long long*)ifltab, catStruct->pathWithWildChars, catStruct, 0, 0, 1);
	}
	else {
		status = zcatalogInternal ((long long*)ifltab, catStruct->pathWithWildChars, catStruct, 0, 0, maxNumber, 0, 0);
	}
	
	if (zisError(status)) {
		return status;
	}
	
	C_CatalogToJava(env, obj, j_dssCatalog , catStruct);		

	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	status = catStruct->numberPathnames;
	zstructFree(catStruct); 
	return (jint)status;
}
