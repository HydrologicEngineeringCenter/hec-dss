#include <string.h>
#include <jni.h>
#include <stdio.h>

#include "heclib.h"
#include "zdssLocking.h"
#include "zdssKeys.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zdbmod
    (JNIEnv *env, jobject obj, jintArray j_ifltab, jlong j_address,
	 jlong j_value, jstring j_characterValue, jint j_useCharacterValue)
{
    long long *ifltab;
	long long *fileHeader;
	long long laddress;
	long long lvalue;
	int i;
    int address;
	int value;
	int nlongwords;
	int status;
	long long charArray[10];
    const char *characterValue;    
	int useCharacterValue;
    
    ifltab            = (long long*)(*env)->GetIntArrayElements (env, j_ifltab, 0);
    laddress          = (long long) j_address;
	lvalue            = (long long) j_value;
	characterValue    = (const char *)(*env)->GetStringUTFChars (env, j_characterValue, 0);
	useCharacterValue = (int) j_useCharacterValue;
	fileHeader		  = (long long *)ifltab[zdssKeys.kfileHeader];

	if (zgetVersion(ifltab) == 6) {
		address = (int)laddress;
		value = (int)lvalue;
		zdbmod6_ (ifltab, &address, &value, characterValue, &useCharacterValue, strlen(characterValue));
		return (jint)0;
	}
	else {
		status =  zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
		if (zisError(status)) {
			status = zerrorUpdate(ifltab, status, DSS_FUNCTION_javaNativeInterface_ID);
		}
		if (useCharacterValue) {
			nlongwords = (((int)strlen(characterValue) -1) / 8) + 1;
			if (nlongwords < 10) {
				charInt((void *)characterValue, (void *)charArray, (int)strlen(characterValue), (int)strlen(characterValue), 0, 1, 0);
				status = zput(ifltab, laddress, charArray, nlongwords, 2);
				if (laddress < (int)ifltab[zdssKeys.kfiHeadSize]) {
					for (i=0; i<nlongwords; i++) {
						fileHeader[(int)laddress+i] = charArray[i];
					}
				}
			}
		}
		else {
			status = zput(ifltab, laddress, &lvalue, 1, 2);		
			if (laddress < (int)ifltab[zdssKeys.kfiHeadSize]) {
				fileHeader[(int)laddress] = lvalue;
			}
		}
		status = zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		if (zisError(status)) {
			status = zerrorUpdate(ifltab, status, DSS_FUNCTION_javaNativeInterface_ID);
		}
	}

    /* Release */
    (*env)->ReleaseIntArrayElements (env, j_ifltab, (int *)ifltab, 0);   
    (*env)->ReleaseStringUTFChars (env, j_characterValue, characterValue); 
	return (jint)status;
}



