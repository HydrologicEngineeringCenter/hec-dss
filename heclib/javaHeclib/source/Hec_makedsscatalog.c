#include <string.h>
#include <jni.h>
#include "heclib.h"

//  DEPRECIATED
//	Use zcatalogToFile instead

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1makedsscatalog
    (JNIEnv *env, jobject obj, jstring j_dssFileName, jintArray j_ifltab,
     jstring j_catalogInstructions, jintArray j_numberFound,
     jintArray j_catalogUnit)	   
{

    const char *dssFileName;
    int *ifltab;
    const char *catalogInstructions;
    int *numberFound;
    int *catalogUnit;

	//printf("Enter Hec_makedsscatalog\n ");
    dssFileName  = (*env)->GetStringUTFChars (env, j_dssFileName, 0);
    ifltab       = (*env)->GetIntArrayElements (env, j_ifltab, 0);
    catalogInstructions = (*env)->GetStringUTFChars (env,
                           j_catalogInstructions, 0);
    numberFound  = (*env)->GetIntArrayElements (env, j_numberFound, 0);
    catalogUnit  = (*env)->GetIntArrayElements (env, j_catalogUnit, 0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_makedsscatalog ", dssFileName);
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "catalogInstructions ", catalogInstructions);
	}


    makedsscatalog_ (dssFileName, ifltab, catalogInstructions,
                     numberFound, catalogUnit,
                     strlen(dssFileName), strlen(catalogInstructions));

    (*env)->ReleaseStringUTFChars (env, j_dssFileName, dssFileName);
    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseStringUTFChars (env, j_catalogInstructions,
                                   catalogInstructions);
    (*env)->ReleaseIntArrayElements (env, j_numberFound, numberFound, 0);
    (*env)->ReleaseIntArrayElements (env, j_catalogUnit, catalogUnit, 0);

}
