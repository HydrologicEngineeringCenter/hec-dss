#include <jni.h>
#include <string.h>
#include "heclib.h"


/*
	This function is meant to be complete, not efficient
	It is not expected to be called often

	See zgetRecordBasics for a simpler more efficient call
*/

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zgetRecordBasics(
	JNIEnv      *env, 
	jobject     obj, 
	jintArray   j_ifltab,		                                               
	jobject		j_recordBasics)
{

	const char *cpath;
	zStructRecordBasics *recordBasics;

	jclass cls;
    jfieldID fid;

    jstring j_cpath;
	jint jnumber;
	jlong jlongNumber;
	int status;
	int *ifltab;


	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);	

	ifltab		= (*env)->GetIntArrayElements(env, j_ifltab, 0);	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_recordBasics);
    fid = (*env)->GetFieldID (env, cls, "pathname", "Ljava/lang/String;");
    if (fid != 0) {
		j_cpath = (*env)->GetObjectField(env, j_recordBasics, fid); 
        cpath = (*env)->GetStringUTFChars(env, j_cpath,  0);
    }

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_1zgetRecordBasics, Enter.  Pathname: ", cpath);
	}

	recordBasics = zstructRecordBasicsNew(cpath);

	status = zgetRecordBasics((long long*)ifltab, recordBasics);

	//  If record is not found, recordBasics will have all values set to zero

	//  Data Type (regular, irregular, profile, float, double...)
	fid = (*env)->GetFieldID (env, cls, "recordType", "I");
	if (fid) {
		jnumber = (jint)recordBasics->recordType;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	//  version - how many times written to
	fid = (*env)->GetFieldID(env, cls, "version", "I");
	if (fid) {
		jnumber = (jint)recordBasics->version;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	// The actual number stored (excluding missing) (Daily = 1 to 365)  
	fid = (*env)->GetFieldID (env, cls, "numberValues", "I");
	if (fid) {
		jnumber = (jint)recordBasics->numberValues;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	//  What the user expects including missing (Daily = 365) 
	fid = (*env)->GetFieldID(env, cls, "logicalNumberValues", "I");
	if (fid) {
		jnumber = (jint)recordBasics->logicalNumberValues;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	//  Internal sizes of data arrays - all 4 bytes
	//  values1Number  (For TS, this is data values) 
	fid = (*env)->GetFieldID(env, cls, "values1Number", "I");
	if (fid) {
		jnumber = (jint)recordBasics->values1Number;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	//  values2Number  (For TS, this is quality array)
	fid = (*env)->GetFieldID(env, cls, "values2Number", "I");
	if (fid) {
		jnumber = (jint)recordBasics->values2Number;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}
	
	//  values3Number  (For TS, this is notes array)
	fid = (*env)->GetFieldID(env, cls, "values3Number", "I");
	if (fid) {
		jnumber = (jint)recordBasics->values3Number;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	//  Record header array lengths (4 byte words)

	fid = (*env)->GetFieldID(env, cls, "internalHeaderNumber", "I");
	if (fid) {
		jnumber = (jint)recordBasics->internalHeaderNumber;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	fid = (*env)->GetFieldID(env, cls, "header2Number", "I");
	if (fid) {
		jnumber = (jint)recordBasics->header2Number;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	fid = (*env)->GetFieldID(env, cls, "userHeaderNumber", "I");
	if (fid) {
		jnumber = (jint)recordBasics->userHeaderNumber;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	//  The total allocated space for this record
	fid = (*env)->GetFieldID(env, cls, "allocatedSize", "I");
	if (fid) {
		jnumber = (jint)recordBasics->allocatedSize;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	//  Last write time
	fid = (*env)->GetFieldID (env, cls, "recLastWriteTimeMillis", "J");
	if (fid) {
		jlongNumber = (jlong)recordBasics->recLastWriteTimeMillis;
		(*env)->SetLongField(env, j_recordBasics, fid, jlongNumber);
	}	

	//  Creation time
	fid = (*env)->GetFieldID(env, cls, "recCreationTimeMillis", "J");
	if (fid) {
		jlongNumber = (jlong)recordBasics->recCreationTimeMillis;
		(*env)->SetLongField(env, j_recordBasics, fid, jlongNumber);
	}

	//  File Last write time
	fid = (*env)->GetFieldID(env, cls, "fileLastWriteTimeMillis", "J");
	if (fid) {
		jlongNumber = (jlong)recordBasics->fileLastWriteTimeMillis;
		(*env)->SetLongField(env, j_recordBasics, fid, jlongNumber);
	}

	//  File Creation time
	fid = (*env)->GetFieldID(env, cls, "fileCreationTimeMillis", "J");
	if (fid) {
		jlongNumber = (jlong)recordBasics->fileCreationTimeMillis;
		(*env)->SetLongField(env, j_recordBasics, fid, jlongNumber);
	}

	//  Pathname hash (a almost unique hash number)
	fid = (*env)->GetFieldID(env, cls, "pathnameHash", "J");
	if (fid) {
		jlongNumber = (jlong)recordBasics->pathnameHash;
		(*env)->SetLongField(env, j_recordBasics, fid, jlongNumber);
	}

	//  tableHash - points to the address for list of pathnames with same hash
	//   (typically tableHash is 1-10,000)
	fid = (*env)->GetFieldID(env, cls, "tableHash", "I");
	if (fid) {
		jnumber = (jint)recordBasics->tableHash;
		(*env)->SetIntField(env, j_recordBasics, fid, jnumber);
	}

	if (recordBasics) {
		zstructFree(recordBasics);
		recordBasics = 0;
	}
 	
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);

	return (jint)status;
}
