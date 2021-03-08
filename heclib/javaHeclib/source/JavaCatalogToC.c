#include <string.h>
#include <stdlib.h>
#include <jni.h>

#include "hecdssInternal.h"
#include "javaHeclib.h"
#include "zdssMessages.h"

/**

	Takes parts of a Java Catalog class and transfers information into a "C" Cat Struct.
	Separated from main catalog function because it is called by several functions.

	Bill Charley,   HEC  2016

**/


void JavaCatalogToC(					
					JNIEnv       *env,
					jobject       obj, 	  
					long long	*ifltab,
					jobject		  j_dssCatalog,
					zStructCatalog *catStruct,
					int			boolPrepToCatalog
	)
{
	int i;
	int len;
	const char *pathname;

	jintArray crcValues;
	jlongArray lastWriteTimeRecord;
	jlongArray pathnameHash;
	jlong *longArray;
	jint *intArray;
	jstring jpathname;
	jstring jstr;
	jobjectArray pathnameList;

	jclass cls;
    jfieldID fid;	

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);


	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, 0, "Enter JavaCatalogToC  boolPrepToCatalog ", boolPrepToCatalog);
	}

	cls = (*env)->GetObjectClass (env, j_dssCatalog);

	fid = (*env)->GetFieldID (env, cls, "statusWanted", "I");
	if (fid) {
		catStruct->statusWanted = (int)(*env)->GetIntField(env, j_dssCatalog, fid);;
	}

	fid = (*env)->GetFieldID (env, cls, "dataTypeStart", "I");
	if (fid) {
		catStruct->typeWantedStart = (int)(*env)->GetIntField(env, j_dssCatalog, fid);
	}

	fid = (*env)->GetFieldID (env, cls, "dataTypeEnd", "I");
	if (fid) {
		catStruct->typeWantedEnd = (int)(*env)->GetIntField(env, j_dssCatalog, fid);
	}
	
	//  Get the pathname with wild characters
    fid = (*env)->GetFieldID (env, cls, "pathnameWithWild", "Ljava/lang/String;");
    if (fid != 0) {
		jpathname = (*env)->GetObjectField(env, j_dssCatalog, fid); 
		if (jpathname) {
			catStruct->pathWithWildChars = mallocAndCopy((char *)(*env)->GetStringUTFChars(env, jpathname,  0));
			(*env)->DeleteLocalRef(env, jpathname);
		}
    }
	
	//  If we are going to call the catalog function, the remainder
	//  of this code is not needed (it's only for comparing cat structs in C)
	if (boolPrepToCatalog) {
		
		return;
	}
	

	//  The following is for sending a catalog struct back for comparison
	//  Not normally used (i.e., arrays should be null)

	fid = (*env)->GetFieldID (env, cls, "pathnameList", "[Ljava/lang/String;");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		fid = 0;
	}
	if (fid) {
		pathnameList = (*env)->GetObjectField(env, j_dssCatalog, fid);
		if (pathnameList) {
			len = (int)(*env)->GetArrayLength(env, pathnameList);
			if (len > 0) {
				catStruct->numberPathnames = len;
				catStruct->listSize = len;
				catStruct->pathnameList = (char **)calloc((size_t)catStruct->listSize, WORD_SIZE);
				catStruct->allocated[zSTRUCT_pathname] = 1;
				for (i = 0; i < catStruct->listSize; i++) {
					jstr = (jstring)(*env)->GetObjectArrayElement(env, pathnameList, i);
					if (jstr) {
						pathname = (*env)->GetStringUTFChars(env, jstr, 0);
						catStruct->pathnameList[i] = mallocAndCopy(pathname);
						(*env)->ReleaseStringUTFChars(env, jstr, pathname);
					}
				}
				(*env)->DeleteLocalRef(env, pathnameList);
			}
		}
	}

	
	fid = (*env)->GetFieldID (env, cls, "lastWriteTimeFile", "J");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		fid = 0;
	}
	if (fid) {
		catStruct->lastWriteTimeFile = (long long)(*env)->GetLongField(env, j_dssCatalog, fid);
	}

	fid = (*env)->GetFieldID (env, cls, "lastWriteTimeRecord", "[J");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		fid = 0;
	}
	if (fid) {
		lastWriteTimeRecord = (*env)->GetObjectField (env, j_dssCatalog, fid);
		if (lastWriteTimeRecord) {
			len = (int)(*env)->GetArrayLength(env, lastWriteTimeRecord);
			if (len == catStruct->listSize) {
				catStruct->lastWriteTimeRecord = (long long*)calloc((size_t)catStruct->listSize, LONG_SIZE);
				longArray = (*env)->GetLongArrayElements(env, lastWriteTimeRecord, 0);
				for (i=0; i<catStruct->listSize; i++) {
					catStruct->lastWriteTimeRecord[i] = (long long)longArray[i];
				}
				(*env)->ReleaseLongArrayElements(env, lastWriteTimeRecord, longArray, 0);
			}
			(*env)->DeleteLocalRef(env, lastWriteTimeRecord);
		}
	}

	fid = (*env)->GetFieldID (env, cls, "pathnameHash", "[J");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		fid = 0;
	}
	if (fid) {
		pathnameHash = (*env)->GetObjectField (env, j_dssCatalog, fid);
		if (pathnameHash) {
			len = (int)(*env)->GetArrayLength(env, pathnameHash);
			if (len == catStruct->listSize) {
				catStruct->pathnameHash = (long long*)calloc((size_t)catStruct->listSize, LONG_SIZE);
				longArray = (*env)->GetLongArrayElements(env, pathnameHash, 0);
				for (i=0; i<catStruct->listSize; i++) {
					catStruct->pathnameHash[i] = (long long)longArray[i];
				}
				(*env)->ReleaseLongArrayElements(env, pathnameHash, longArray, 0);
			}
			(*env)->DeleteLocalRef(env, pathnameHash);
		}
	}

	fid = (*env)->GetFieldID (env, cls, "crcValues", "[I");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		fid = 0;
	}
	if (fid) {
		crcValues = (*env)->GetObjectField (env, j_dssCatalog, fid);
		if (crcValues) {
			len = (int)(*env)->GetArrayLength(env, crcValues);
			if (len == catStruct->listSize) {
				catStruct->crcValues = (unsigned int*)calloc((size_t)catStruct->listSize, INT_SIZE);
				intArray = (*env)->GetIntArrayElements(env, crcValues, 0);
				for (i=0; i<catStruct->listSize; i++) {
					catStruct->crcValues[i] = (int)intArray[i];
				}
				(*env)->ReleaseIntArrayElements(env, crcValues, intArray, 0);
				catStruct->boolGetCRCvalues = 1;
			}
			(*env)->DeleteLocalRef(env, crcValues);
		}
	}

	return;
}
