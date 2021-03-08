#include <jni.h>
#include <string.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1ztextRetrieve(
	JNIEnv       *env,
	jobject       obj, 
	jintArray    j_ifltab,		                                                
	jobject		  j_textContainer)  
{

	const char *cpath;
	zStructText *textStruct;
	int i, j;
	int count;
	int len;
	int status;		
	int *ifltab;

	jclass cls;
    jfieldID fid;
    jstring j_cpath;
	jstring jstr;
	jint jnumber;
	jint numberRows, numberCols;
	jobjectArray stringArray;
	jobjectArray textTable;
	jclass stringClass;
	jlong jlongNumber;

	
//
	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	

	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_textContainer);
    fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_cpath = (*env)->GetObjectField(env, j_textContainer, fid); 
        cpath = (*env)->GetStringUTFChars(env, j_cpath,  0);
    }
	

	textStruct = zstructTextNew(cpath); 
	if (!textStruct) {
		//  Error out!
		return -1;
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter ztextRetrieve; Pathname: ", textStruct->pathname);
	}

	status = ztextRetrieve((long long*)ifltab, textStruct);
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After text retrieve.  Status: ", status);
	}
	
	//  Fill in the text container with data from the struct
	if (!status) {
		
		fid = (*env)->GetFieldID (env, cls, "dataType", "I");
		if (fid) {
			(*env)->SetIntField(env, j_textContainer, fid, (jint)textStruct->dataType);
		}

		//  Text string?
		if (textStruct->textString) {
			cls = (*env)->GetObjectClass (env, j_textContainer);
			fid = (*env)->GetFieldID (env, cls, "text", "Ljava/lang/String;");
			if (fid) {
				jstr = (*env)->NewStringUTF(env, textStruct->textString);
				(*env)->SetObjectField (env, j_textContainer, fid, jstr);
				if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Text string read.  Number characters: ", textStruct->numberTextChars);
				}
			}
		}

		//  Text table?
		if (textStruct->textTable) {
			numberRows = (jint)textStruct->numberRows;
			numberCols = (jint)textStruct->numberColumns;
			stringClass = (*env)->FindClass(env, "Ljava/lang/String;");		
			stringArray = (*env)->NewObjectArray(env, numberRows, stringClass, 0);
			textTable = (*env)->NewObjectArray(env, numberRows, (*env)->GetObjectClass(env, stringArray), 0);
			(*env)->DeleteLocalRef(env, stringArray);
			count = 0;							
			for (j=0; j<numberRows; j++) {
				stringArray = (*env)->NewObjectArray(env, numberCols, stringClass, 0);
				for (i=0; i<numberCols; i++) {
					len = (int)strnlen_hec(&textStruct->textTable[count], textStruct->numberTableChars-count);
					jstr = (*env)->NewStringUTF(env, &textStruct->textTable[count]);
					(*env)->SetObjectArrayElement(env, stringArray, i, jstr);
					(*env)->DeleteLocalRef(env, jstr);				
					count += len + 1;
				}
				(*env)->SetObjectArrayElement(env, textTable, j, stringArray);
				(*env)->DeleteLocalRef(env, stringArray);	
			}
			cls = (*env)->GetObjectClass (env, j_textContainer);
			fid = (*env)->GetFieldID (env, cls, "textTable", "[[Ljava/lang/String;");
			if (fid) {
				(*env)->SetObjectField (env, j_textContainer, fid, textTable);
			}
			(*env)->DeleteLocalRef(env, textTable);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
				zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Text table read.  Number characters: ", textStruct->numberTableChars);
				zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Number Rows:    ", textStruct->numberRows);
				zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Number Columns: ", textStruct->numberColumns);
			}
		}

		//  Labels?
		if (textStruct->labels) {
			jnumber = (jint)textStruct->numberColumns;
			stringClass = (*env)->FindClass(env, "Ljava/lang/String;");
			stringArray = (*env)->NewObjectArray(env, jnumber, stringClass, 0);	
			count = 0;				
			for (i=0; i<textStruct->numberColumns; i++) {
				len = (int)strnlen_hec(&textStruct->labels[count], textStruct->numberLabelChars-count);
				jstr = (*env)->NewStringUTF(env, &textStruct->labels[count]);
				(*env)->SetObjectArrayElement(env, stringArray, i, jstr);
				(*env)->DeleteLocalRef(env, jstr);				
				count += len + 1;
				if (count >= textStruct->numberLabelChars) break;
			}
			cls = (*env)->GetObjectClass (env, j_textContainer);
			fid = (*env)->GetFieldID (env, cls, "labels", "[Ljava/lang/String;");
			if (fid) {
				(*env)->SetObjectField (env, j_textContainer, fid, stringArray);
			}
			(*env)->DeleteLocalRef(env, stringArray);
		}		

		//  User header (supplemental)
		if (textStruct->userHeaderNumber > 0) {
			fid = (*env)->GetFieldID (env, cls, "supplementalInfo", "Ljava/lang/String;");
			if (fid) {
				jstr = (*env)->NewStringUTF(env, (const char *)textStruct->userHeader);
				(*env)->SetObjectField (env, j_textContainer, fid, jstr);
			}
		}

		//  Last write time
		fid = (*env)->GetFieldID (env, cls, "lastWriteTimeMillis", "J");
		if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);
			zmessage((long long *)ifltab, "Exception in get fieldID lastWriteTimeMillis");
		}
		else if (fid) {
			jlongNumber = (jlong)textStruct->lastWrittenTime;
			(*env)->SetLongField(env, j_textContainer, fid, jlongNumber);
		}

		//  File last write time
		fid = (*env)->GetFieldID (env, cls, "fileLastWriteTimeMillis", "J");
		if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
		}
		else if (fid) {
			jlongNumber = (jlong)textStruct->fileLastWrittenTime;
			(*env)->SetLongField(env, j_textContainer, fid, jlongNumber);
		}

	}

	zstructFree(textStruct);

	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);


	return (jint)status;
}
