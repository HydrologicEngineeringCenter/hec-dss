#include <jni.h>
#include "heclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zconvertVersion(
    JNIEnv       *env,
	jobject       obj, 
	jstring		  j_fileNameFrom,		                                             
	jstring		  j_fileNameTo
	)
{
    int status;
    const char *fileNameFrom; 
	const char *fileNameTo; 

    fileNameFrom = (const char *) (*env)->GetStringUTFChars (env, j_fileNameFrom, 0);
	fileNameTo = (const char *) (*env)->GetStringUTFChars (env, j_fileNameTo, 0);
	
	status = zconvertVersion(fileNameFrom, fileNameTo);

    /* Release the file names */
    (*env)->ReleaseStringUTFChars (env, j_fileNameFrom, fileNameFrom);
	(*env)->ReleaseStringUTFChars (env, j_fileNameTo, fileNameTo);
	return (jint)status;
}
