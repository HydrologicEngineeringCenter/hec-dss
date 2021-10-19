#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1ztextStore(
	JNIEnv       *env,
	jobject       obj, 
	jintArray    j_ifltab,		                                                
	jobject		  j_textContainer)  
{

	const char *cpath;
	const char *cstr;
	zStructText *textStruct;
	int i, j, k;
	int count;
	int len;
	int lenc;
	int status;		
	int *ifltab;

	jclass cls;
    jfieldID fid;
    jstring j_cpath;
	jstring jstr;
	jobjectArray stringArray;
	jobjectArray textTable;

	
	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	
	status = 0;
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
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_ztextStore; Pathname: ", textStruct->pathname);
	}


	fid = (*env)->GetFieldID (env, cls, "text", "Ljava/lang/String;");
	if (fid) {
		jstr = (*env)->GetObjectField(env, j_textContainer, fid);
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				len = (int)strlen(cstr);
				if (len > 0) {
					textStruct->textString = mallocAndCopy(cstr);
					textStruct->allocated[zSTRUCT_TX_textString] = 1;
					textStruct->numberTextChars = len;
					if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
						zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Text string to store.  Number characters: ", textStruct->numberTextChars);
					}
				}
			}
		}
	}
	
	//  Text table?
	fid = (*env)->GetFieldID (env, cls, "textTable", "[[Ljava/lang/String;");
	if (fid) {
		textTable = (*env)->GetObjectField (env, j_textContainer, fid);
		if (textTable) {
			len = (int)(*env)->GetArrayLength(env, textTable);
			if (len > 0) {
				textStruct->numberRows = len;
				stringArray = (*env)->GetObjectArrayElement(env, textTable, 0);
				if (stringArray) {
					len = (int)(*env)->GetArrayLength(env, stringArray);
					textStruct->numberColumns = len;					
				}
				//  Count the number of characters first
				count = 0;
				for (i=0; i<textStruct->numberRows; i++) {
					stringArray = (*env)->GetObjectArrayElement(env, textTable, i);
					if (stringArray) {
						len = (int)(*env)->GetArrayLength(env, stringArray);
						if (len > 0) {
							for (j=0; j<len; j++) {
								jstr = (jstring) (*env)->GetObjectArrayElement(env, stringArray, j);
								if (jstr) {
									cstr = (*env)->GetStringUTFChars(env, jstr, 0);
									if (cstr) {
										count += (int)strlen(cstr);
										count++;  //  For "\0" end of each string
									}
									else {
										count++; 
									}
								}
								else {
									count++; 
								}
								(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
							}
						}
					}
				}
				if (count > 0) {
					textStruct->numberTableChars = count;								
					//  Now allocate the space and copy the ordinates
					textStruct->textTable = (char *)calloc(count, 1);
					textStruct->allocated[zSTRUCT_TX_textTable] = 1;
					count = 0;
					for (i=0; i<textStruct->numberRows; i++) {
						stringArray = (*env)->GetObjectArrayElement(env, textTable, i);
						if (stringArray) {
							len = (int)(*env)->GetArrayLength(env, stringArray);
							for (j=0; j<len; j++) {
								jstr = (jstring) (*env)->GetObjectArrayElement(env, stringArray, j);
								if (jstr) {
									cstr = (*env)->GetStringUTFChars(env, jstr, 0);
									if (cstr) {
										lenc = (int)strlen(cstr);
										for (k=0; k<lenc; k++) {
											textStruct->textTable[count++] = cstr[k];
										}			
										textStruct->textTable[count++] = '\0';
									}										
									else {
										count++; 
									}
									(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
								}
								else {
									count++; 
								}
							}
						}								
					}
				}
				if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Text table to store.  Number characters: ", textStruct->numberTableChars);					
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Number Rows:    ", textStruct->numberRows);
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Number Columns: ", textStruct->numberColumns);
				}	
			}
		}
	}

	//  Labels?  (column header)
	fid = (*env)->GetFieldID (env, cls, "labels", "[Ljava/lang/String;");
	if (fid) {
		stringArray = (*env)->GetObjectField (env, j_textContainer, fid);
		if (stringArray) {
			len = (int)(*env)->GetArrayLength(env, stringArray);
			if (len > 0) {
				//  Count the number of characters first
				count = 0;
				for (i=0; i<len; i++) {
					jstr = (jstring) (*env)->GetObjectArrayElement(env, stringArray, i);
					if (!jstr) break;
					cstr = (*env)->GetStringUTFChars(env, jstr, 0);
					if (cstr) {
						count += (int)strlen(cstr);
						count++;  //  For "\0" end of each string
					}
					else {
						break;
					}
					(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
				}
				if (count > 0) {
					textStruct->numberLabelChars = count;
					//  Now allocate the space and copy the ordinates
					textStruct->labels = (char *)calloc(count, 1);
					textStruct->allocated[zSTRUCT_TX_labels] = 1;
					count = 0;
					for (i=0; i<len; i++) {
						jstr = (jstring) (*env)->GetObjectArrayElement(env, stringArray, i);
						cstr = (*env)->GetStringUTFChars(env, jstr, 0);
						if (cstr) {
							len = (int)strlen(cstr);
							for (j=0; j<len; j++) {
								textStruct->labels[count++] = cstr[j];
							}
							textStruct->labels[count++] = '\0';
						}
						(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
					}
					if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
						zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Text labels to store.  Number characters: ", textStruct->numberLabelChars);
						zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Number rows: ", len);
					}					
				}
			}
		}
	}

	//  User header
	fid = (*env)->GetFieldID (env, cls, "supplementalInfo", "Ljava/lang/String;");
	if (fid) {
		jstr = (*env)->GetObjectField(env, j_textContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				len = (int)strlen(cstr);
				if (len > 0) {
					textStruct->userHeaderNumber = numberIntsInBytes(len);					
					textStruct->userHeader = (int *)calloc(textStruct->userHeaderNumber, 4);
					charInt ((void *)cstr, textStruct->userHeader, len, (textStruct->userHeaderNumber * 4), 1, 1, 0);
					textStruct->allocated[zSTRUCT_userHeader] = 1;
				}
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	status = ztextStore((long long*)ifltab, textStruct);
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After text store.  Status: ", status);
	}
	

	zstructFree(textStruct);
  
	
	//-------------------//
	// release variables //
	//-------------------//
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);


	return (jint)status;
}
