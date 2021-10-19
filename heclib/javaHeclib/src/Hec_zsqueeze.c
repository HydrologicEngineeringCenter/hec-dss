#include <string.h>
#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zsqueeze
  (JNIEnv *env, jobject obj, jstring j_filename) {
  
    char *filename;
    int   status;


    filename = (char *) (*env)->GetStringUTFChars (env, j_filename, 0); 
	zsqueeze_ (filename, &status, strlen(filename));  
	(*env)->ReleaseStringUTFChars (env, j_filename, filename);
    return status;
}

//  For backward compatibility
JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1squeezeDSS
  (JNIEnv *env, jobject obj, jstring j_filename) {

    char *filename;
    int   status;

    filename = (char *) (*env)->GetStringUTFChars (env, j_filename, 0); 
	zsqueeze_ (filename, &status, strlen(filename));  
	(*env)->ReleaseStringUTFChars (env, j_filename, filename);
    return status;
}
