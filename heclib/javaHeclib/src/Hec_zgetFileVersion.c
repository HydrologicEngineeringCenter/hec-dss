#include <jni.h>
#include "heclib.h"

/**
*  Function:	zgetFileVersion
*
*  Use:			Public   
*
*  Description:	Returns the version number of a (unopened) DSS file
* 
*  Declaration: int zgetFileVersion(const char *dssFilename)
*
*  Parameters:	
*				char *dssFilename:  The DSS file name to check the version of.
*				The file does not have to exist (in which case zero is returned)
*
*	Returns:	7:  A DSS version 7 file
*				6:  A DSS version 6 file
*				0:  File does not exist
*			   -1:  Not a DSS file (but file exists)
*              -2:  Invalid file name
*			   -3:  Open error (undefined)
*			  <-3:  abs(error) is system open or read error
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

JNIEXPORT int JNICALL Java_hec_heclib_util_Heclib_Hec_1zgetFileVersion
    (JNIEnv *env, jobject obj, jstring j_dssFileName)
{
    const char *dssFileName;
	int iver;

    dssFileName = (const char *) (*env)->GetStringUTFChars (env, j_dssFileName, 0);

	iver = zgetFileVersion(dssFileName);
	
    (*env)->ReleaseStringUTFChars (env, j_dssFileName, dssFileName);

	return iver;
}

