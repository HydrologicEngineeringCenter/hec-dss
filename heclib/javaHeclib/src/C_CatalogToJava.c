#include <string.h>

#include "hecdss7.h"
#include "javaHeclib.h"

/**

	Takes a "C" Cat Struct and transfers information into a Java Catalog class
	Separated from main catalog function because it is called by several functions.

	Bill Charley,   HEC  2016

**/


void C_CatalogToJava(
					JNIEnv       *env,
					jobject       obj, 	                                          
					jobject		  j_dssCatalog,
					zStructCatalog *catStruct
	)
{
	int i;
	char *pathname;

	jintArray startDates;
	jintArray endDates;
	jintArray crcValues;
	jlongArray pathAddresses;
	jlongArray lastWriteTimeRecord;
	jlongArray pathnameHash;
	jstring jpathname;
	jstring jstr;
	jsize size;
	jobjectArray stringArray;
	jclass stringClass;

	jclass cls;
    jfieldID fid;	

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	
	cls = (*env)->GetObjectClass (env, j_dssCatalog);

	if (catStruct->pathnameList) {
		size = (jsize)catStruct->numberPathnames;
		stringClass = (*env)->FindClass(env, "Ljava/lang/String;");		
		stringArray = (*env)->NewObjectArray(env, size, stringClass, 0);
		for (i=0; i<catStruct->numberPathnames; i++) {
			pathname = catStruct->pathnameList[i];
			jpathname = (*env)->NewStringUTF(env, pathname);
			(*env)->SetObjectArrayElement(env, stringArray, i, jpathname);
			(*env)->DeleteLocalRef(env, jpathname);
		}
		fid = (*env)->GetFieldID (env, cls, "pathnameList", "[Ljava/lang/String;");
		if (fid) {
			(*env)->SetObjectField (env, j_dssCatalog, fid, stringArray);
		}
		(*env)->DeleteLocalRef(env, stringArray);
	}

	//  Get the pathname with wild characters
    fid = (*env)->GetFieldID (env, cls, "pathnameWithWild", "Ljava/lang/String;");
    if (fid != 0) {
		if (catStruct->pathWithWildChars) {
			jstr = (*env)->NewStringUTF(env, catStruct->pathWithWildChars);
		}
		else {
			jstr = (*env)->NewStringUTF(env, "");
		}
        (*env)->SetObjectField (env, cls, fid, jstr);
		(*env)->DeleteLocalRef(env, jstr);
	}

	if (catStruct->startDates) {
		size = (jsize)catStruct->numberPathnames;
		startDates = (*env)->NewIntArray(env, size);
		(*env)->SetIntArrayRegion(env, startDates, 0, size, catStruct->startDates); 
		fid = (*env)->GetFieldID (env, cls, "startDates", "[I");
		if (fid) {
			(*env)->SetObjectField (env, j_dssCatalog, fid, startDates);
		}
		(*env)->DeleteLocalRef(env, startDates);
	}

	if (catStruct->endDates) {
		size = (jsize)catStruct->numberPathnames;
		endDates = (*env)->NewIntArray(env, size);
		(*env)->SetIntArrayRegion(env, endDates, 0, size, catStruct->endDates); 
		fid = (*env)->GetFieldID (env, cls, "endDates", "[I");
		if (fid) {
			(*env)->SetObjectField (env, j_dssCatalog, fid, endDates);
		}
		(*env)->DeleteLocalRef(env, endDates);
	}

	if (catStruct->sortAddresses) {
		size = (jsize)catStruct->numberPathnames;
		pathAddresses = (*env)->NewLongArray(env, size);
		(*env)->SetLongArrayRegion(env, pathAddresses, 0, size, (const jlong *)catStruct->sortAddresses); 
		fid = (*env)->GetFieldID (env, cls, "pathnameAddresses", "[J");
		if (fid) {
			(*env)->SetObjectField (env, j_dssCatalog, fid, pathAddresses);
		}
		(*env)->DeleteLocalRef(env, pathAddresses);
	}

	fid = (*env)->GetFieldID (env, cls, "statusWanted", "I");
	if (fid) {
		(*env)->SetIntField(env, j_dssCatalog, fid, (jint)catStruct->statusWanted);		
	}
	/*
	fid = (*env)->GetFieldID (env, cls, "statusRead", "I");
	if (fid) {
		(*env)->SetIntField(env, j_dssCatalog, fid, (jint)catStruct->statusWanted);		
	}
	*/
	
	fid = (*env)->GetFieldID (env, cls, "listIsCollection", "Z");
	if (fid) {
		(*env)->SetBooleanField(env, j_dssCatalog, fid, (jboolean)catStruct->boolIsCollection);
	}

	fid = (*env)->GetFieldID (env, cls, "listIsSorted", "Z");
	if (fid) {
		(*env)->SetBooleanField(env, j_dssCatalog, fid, (jboolean)catStruct->boolSorted);
	}

	//  if (version == 6) return;

	fid = (*env)->GetFieldID (env, cls, "dataTypeStart", "I");
	if (fid) {
		(*env)->SetIntField(env, j_dssCatalog, fid, (jint)catStruct->typeWantedStart);
	}

	fid = (*env)->GetFieldID (env, cls, "dataTypeEnd", "I");
	if (fid) {
		(*env)->SetIntField(env, j_dssCatalog, fid, (jint)catStruct->typeWantedEnd);
	}
	
	//  Last write time
	fid = (*env)->GetFieldID (env, cls, "lastWriteTimeFile", "J");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		(*env)->SetLongField(env, j_dssCatalog, fid, catStruct->lastWriteTimeFile);
	}

	if (catStruct->lastWriteTimeRecord) {
		fid = (*env)->GetFieldID (env, cls, "lastWriteTimeRecord", "[J");
		if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
		}
		else if (fid) {
			size = (jsize)catStruct->numberPathnames;
			lastWriteTimeRecord = (*env)->NewLongArray(env, size);
			(*env)->SetLongArrayRegion(env, lastWriteTimeRecord, 0, size, (const jlong *)catStruct->lastWriteTimeRecord); 
			(*env)->SetObjectField (env, j_dssCatalog, fid, lastWriteTimeRecord);
			(*env)->DeleteLocalRef(env, lastWriteTimeRecord);
		}
	}

	if (catStruct->pathnameHash) {
		fid = (*env)->GetFieldID (env, cls, "pathnameHash", "[J");
		if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
		}
		else if (fid) {
			size = (jsize)catStruct->numberPathnames;
			pathnameHash = (*env)->NewLongArray(env, size);
			(*env)->SetLongArrayRegion(env, pathnameHash, 0, size, (const jlong *)catStruct->pathnameHash); 
			(*env)->SetObjectField (env, j_dssCatalog, fid, pathnameHash);
			(*env)->DeleteLocalRef(env, pathnameHash);
		}
	}

	if (catStruct->crcValues) {
		if (catStruct->crcValues && catStruct->boolGetCRCvalues) {
			fid = (*env)->GetFieldID (env, cls, "crcValues", "[I");
			if ((*env)->ExceptionOccurred(env)) {
				(*env)->ExceptionClear(env);			
			}
			else if (fid) {
				size = (jsize)catStruct->numberPathnames;
				crcValues = (*env)->NewIntArray(env, size);
				(*env)->SetIntArrayRegion(env, crcValues, 0, size, catStruct->crcValues); 			
				(*env)->SetObjectField (env, j_dssCatalog, fid, crcValues);	
				(*env)->DeleteLocalRef(env, crcValues);
			}
		}
	}
 
	return;
}
