#include <jni.h>
#include "heclib.h"



JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zsetCatalogSortAddresses
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jlongArray j_sortAddresses, jint j_sortAddressesLen)
{
    int *ifltab;
    long long *sortAddresses;
	int sortAddressesLen;
	int status;


	ifltab	= (*env)->GetIntArrayElements(env, j_ifltab, 0);
    sortAddresses = (*env)->GetLongArrayElements (env, j_sortAddresses, 0);
	sortAddressesLen = (int)j_sortAddressesLen;
	
	status = zsetCatalogSortAddresses((long long*)ifltab, sortAddresses, sortAddressesLen);

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
    (*env)->ReleaseLongArrayElements (env, j_sortAddresses, sortAddresses, 0);

	return (jint)status;
}
