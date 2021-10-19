#ifndef ZDSS_JNI_H
#define ZDSS_JNI_H


int Hec_zlocationToStruct(JNIEnv *env, jobject obj, jobject j_dataContainer, zStructLocation *locationStruct);
int Hec_zlocationFromStruct(JNIEnv *env, jobject obj, jobject j_dataContainer, zStructLocation *locationStruct);


#endif //  ZDSS_JNI_H