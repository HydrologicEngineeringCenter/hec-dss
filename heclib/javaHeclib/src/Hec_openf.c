#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1openf
  (JNIEnv *env, jobject obj, jstring j_filename) {

    int   handle;
    char *filename;    

    filename = (char *) (*env)->GetStringUTFChars (env, j_filename, 0);    
	handle = zopenFile(filename, O_RDWR);

    (*env)->ReleaseStringUTFChars (env, j_filename, filename);
    
    return handle;
}
