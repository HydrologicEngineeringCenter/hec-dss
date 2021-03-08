#include <jni.h>
#include <string.h>
#include "zdssMessages.h"
#include "heclib.h"

//  Returns the list of group names whose message level can be set
//  Usually for diagnostics 

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zmessageGroups(
	JNIEnv       *env,
	jobject       obj, 		                                                
	jobject		 j_textContainer)  
{

	jclass cls;
    jfieldID fid;
	jstring jstr;
	jint numberRows, numberCols;
	jobjectArray stringArray;
	jobjectArray textTable;
	jclass stringClass;

	
	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);	

	numberRows = NUMBER_METHOD_NAMES;
	numberCols = 1;
	stringClass = (*env)->FindClass(env, "Ljava/lang/String;");		
	stringArray = (*env)->NewObjectArray(env, numberCols, stringClass, 0);
	textTable = (*env)->NewObjectArray(env, numberCols, (*env)->GetObjectClass(env, stringArray), 0);
	(*env)->DeleteLocalRef(env, stringArray);
	stringArray = (*env)->NewObjectArray(env, numberRows, stringClass, 0);

	jstr = (*env)->NewStringUTF(env, MESS_METHOD_GLOBAL);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_GLOBAL_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);

	jstr = (*env)->NewStringUTF(env, MESS_METHOD_GENERAL);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_GENERAL_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);	
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_GET);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_GET_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_PUT);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_PUT_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_READ);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_READ_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_WRITE);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_WRITE_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_PERM);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_PERM_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_OPEN);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_OPEN_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_CHECK);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_CHECK_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_LOCKING);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_LOCKING_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_TS_READ);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_TS_READ_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_TS_WRITE);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_TS_WRITE_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_ALIAS);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_ALIAS_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_COPY);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_COPY_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_UTILITY);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_UTILITY_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_CATALOG);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_CATALOG_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_FILE_CHECK);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_FILE_CHECK_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);
	
	jstr = (*env)->NewStringUTF(env, MESS_METHOD_JNI);
	(*env)->SetObjectArrayElement(env, stringArray, MESS_METHOD_JNI_ID, jstr);
	(*env)->DeleteLocalRef(env, jstr);

	/*for (i=0; i<numberRows; i++) {
		jstr = (*env)->NewStringUTF(env, zmessaging_general);
		(*env)->SetObjectArrayElement(env, stringArray, i, jstr);
		(*env)->DeleteLocalRef(env, jstr);				
	} */

	(*env)->SetObjectArrayElement(env, textTable, 0, stringArray);
	(*env)->DeleteLocalRef(env, stringArray);	
	cls = (*env)->GetObjectClass (env, j_textContainer);
	fid = (*env)->GetFieldID (env, cls, "textTable", "[[Ljava/lang/String;");
	if (fid) {
		(*env)->SetObjectField (env, j_textContainer, fid, textTable);
	}
	(*env)->DeleteLocalRef(env, textTable);

	return (jint)numberRows;
}
