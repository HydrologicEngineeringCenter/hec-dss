#ifndef JAVA_HECLIB_H
#define JAVA_HECLIB_H

#include <jni.h>
#include "hecdssInternal.h"

void C_CatalogToJava(JNIEnv *env, jobject obj, jobject j_dssCatalog, zStructCatalog *catStruct);
void JavaCatalogToC(JNIEnv *env, jobject obj, long long	*ifltab, jobject j_dssCatalog, zStructCatalog *catStruct, int boolPrepToCatalog);
int Hec_zlocationToStruct(JNIEnv *env, jobject obj, jobject j_dataContainer, zStructLocation *locationStruct);
int Hec_zlocationFromStruct(JNIEnv *env, jobject obj, jobject j_dataContainer, zStructLocation *locationStruct);


#endif //  JAVA_HECLIB_H
