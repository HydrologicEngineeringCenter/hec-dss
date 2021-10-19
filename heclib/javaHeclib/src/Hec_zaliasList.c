#include <string.h>
#include <jni.h>
#include "heclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zaliasList(
	JNIEnv      *env, 
	jobject     obj, 
	jintArray   j_ifltab,
	jstring		j_aliasPath,
	jobject		j_pathList)

  {

    int number;
    int *ifltab;
	const char *aliasPath;
	char *pathList;
	int pathListLength;
	int count;
	int len;
	int i;

	jclass cls;
    jfieldID fid;
	jobjectArray stringArray;
	jclass stringClass;
    jstring jstr;


	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);	
	aliasPath = (char *) (*env)->GetStringUTFChars (env, j_aliasPath, 0);

	number = zaliasList((long long *)ifltab, aliasPath, &pathList, &pathListLength);

	if (number > 0) {
		stringClass = (*env)->FindClass(env, "Ljava/lang/String;");
		stringArray = (*env)->NewObjectArray(env, (jint)number, stringClass, 0);	
		count = 0;	
		i = 0;
		while (count < pathListLength) {
			len = (int)strnlen_hec(&pathList[count], pathListLength-count);
			jstr = (*env)->NewStringUTF(env, &pathList[count]);
			(*env)->SetObjectArrayElement(env, stringArray, i, jstr);
			(*env)->DeleteLocalRef(env, jstr);			
			count += len + 1;
			i++;
		}
		cls = (*env)->GetObjectClass (env, j_pathList);
		fid = (*env)->GetFieldID (env, cls, "stringArray", "[Ljava/lang/String;");
		if (fid) {
			(*env)->SetObjectField (env, j_pathList, fid, stringArray);
		}
		(*env)->DeleteLocalRef(env, stringArray);
	}

	(*env)->ReleaseStringUTFChars(env, j_aliasPath,  aliasPath);
    (*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)number;
}
