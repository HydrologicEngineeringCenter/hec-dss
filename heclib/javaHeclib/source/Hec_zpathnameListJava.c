#include <jni.h>
#include <string.h>
#include <stdio.h>

#include "hecdss7.h"
#include "zprogress.h"
#include "zdssMessages.h"
#include "heclib.h"
	/**
	 * DEPRECIATED, use HecDssCatalog class.
	 * public String[] getCatalog(boolean sorted, String pathWithWildChars)
	 *
	 * @return  a sorted list of pathnames.  Null or 0 if
	 * this function could not get a list
	 * 
	 * This is here for compatibility only!
	 */

//_int64* avoidCast(int *in);

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zpathnameListJava
    (JNIEnv *env, jobject obj, jintArray j_ifltab,  jobjectArray j_pathnames,
	jint j_statusWanted, jint j_typeWantedStart, jint j_typeWantedEnd,
	jintArray j_startDates, jintArray j_endDates, jint j_arrayLength)
{


    int *ifltab;
	int *startDates;
	int *endDates;
	int arrayLength;
	jstring jpathname;
	int len;
	int jpos;
	char *pos;
	char *path;
	char ctemp[13];

	int count;
	int i;
	int statusWanted;
	int typeWantedStart;
	int typeWantedEnd;
	int boolHMS_fix;

	zStructCatalog *catStruct;


    ifltab = (*env)->GetIntArrayElements (env, j_ifltab, 0);

	startDates = (*env)->GetIntArrayElements (env, j_startDates, 0);
	endDates   = (*env)->GetIntArrayElements (env, j_endDates, 0);	
	statusWanted = (int)j_statusWanted;
	typeWantedStart = (int)j_typeWantedStart;
	typeWantedEnd  = (int)j_typeWantedEnd;
	arrayLength = (int)j_arrayLength;

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_zpathnameListJava ", "");
	}

	catStruct = zstructCatalogNew();
	catStruct->boolIncludeDates = 1;
	zcatalog((long long*)ifltab, (const char*)0, catStruct, 1);
	
	boolHMS_fix = 0;
#ifdef _MSC_VER
	//  Only for 32
	if (sizeof(intptr_t) == 4) {
		zinquireChar((long long*)ifltab, "prog", ctemp, sizeof(ctemp), &i);
		if (!strncmp(ctemp, "HECHMS", 6)) boolHMS_fix = 1;
	}
#endif
	count = 0;
	if (catStruct->pathnameList) {		
		for (i=0; i<catStruct->numberPathnames; i++) {
			if (i > arrayLength) break;
			/////////////////////////////////////////////////
			/////////////////////////////////////////////////
			//  For compatibility purposes only...
			//  Make pathname uppercase and change "Minute" to "MIN"
			if (boolHMS_fix) {
				upperCase(catStruct->pathnameList[i]);
				pos = strstr(catStruct->pathnameList[i], "MINUTE/");
				if (pos) {
					path = catStruct->pathnameList[i];
					len = (int)strlen(path);
					pos += 3;
					jpos = pos - path;
					path[jpos] = '\0';
					pos += 3;
					//strcat_s(path, len, pos);
					stringCat(path, len, pos, _TRUNCATE);
				}
			}
			///////////////////////////////////////////////////
			jpathname = (*env)->NewStringUTF(env, catStruct->pathnameList[i]);
			(*env)->SetObjectArrayElement(env, j_pathnames, i, jpathname);
			(*env)->DeleteLocalRef(env, jpathname);
			if (catStruct->startDates && catStruct->endDates) {
				startDates[i] = catStruct->startDates[i];
				endDates[i] = catStruct->endDates[i];
			}
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_zpathnameListJava, pathname ", catStruct->pathnameList[i]);
			}
			count++;
		}
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_zpathnameListJava, number pathnames ", count);
	}

	zstructFree(catStruct);

    (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0);
	(*env)->ReleaseIntArrayElements (env, j_startDates, startDates, 0);
	(*env)->ReleaseIntArrayElements (env, j_endDates, endDates, 0);

    return (jint)count;
}
