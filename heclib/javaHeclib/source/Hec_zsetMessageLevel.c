#include <jni.h>
#include "heclib.h"

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsetMessageLevel
  (JNIEnv *env, jobject obj, jint j_methodID, jint j_levelID)
{

	int methodID;
	int levelID;

	methodID = (int)j_methodID;
	levelID  = (int)j_levelID;

	zsetMessageLevel(methodID, levelID);
}
