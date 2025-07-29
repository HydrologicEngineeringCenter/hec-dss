#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "javaHeclib.h"
#include "verticalDatum.h"
#include "jni_utility.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1ztsRetrieve(
	JNIEnv       *env,
	jobject       obj, 
	jintArray    j_ifltab,		 // file table                                                
	jobject		  j_timeSeriesContainer,		 // pathname 
	jbyteArray    j_startDate,   //
	jbyteArray    j_startTime,   //
	jbyteArray    j_endDate,   //
	jbyteArray    j_endTime,
	jint		  j_retrieveFlag)   //  0 = normal, -1 = trim for regular
									//  1 = retrieve one before, 2 = one after, 
									//  3 = one before and one after (Irregular data only)
	                               
{

	const char *startDate;
	const char *startTime;
	const char *endDate;
	const char *endTime;

	jclass cls;
  jfieldID fid;

	jstring jstr;
	jintArray times;
	jintArray quality;
	jintArray inotes;
	jdoubleArray values;
	jdoubleArray profileDepths;
	jsize size;
	jclass doubleArrayClass;
	jclass intArrayClass;
	jclass stringClass;
	jobjectArray doubleDim;	
	jobjectArray doubleIntDim;
	jobjectArray stringArray;
	jint jnumber;
	jlong jlongNumber;

	int i, j;
	int status;
	int itime;
	int idate;
	int ipos;
	int lencnotesRead;
	int retrieveFlag;
	int len;
	int boolMalloced;
	int number;
	char cpart[MAX_PART_SIZE];
	char cpath[MAX_PATHNAME_SIZE] = "";
	zStructTimeSeries *tss;
	
	int *ifltab;

	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);


	
	ifltab		= (*env)->GetIntArrayElements(env, j_ifltab, 0);	
	startDate	= (*env)->GetStringUTFChars(env, j_startDate,  0);
	startTime	= (*env)->GetStringUTFChars(env, j_startTime,  0);
	endDate		= (*env)->GetStringUTFChars(env, j_endDate,  0);
	endTime		= (*env)->GetStringUTFChars(env, j_endTime,  0);
	retrieveFlag = (int) j_retrieveFlag;
	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_timeSeriesContainer);

	hec_dss_jni_getStringField(env, cls, j_timeSeriesContainer, "fullName", cpath,(size_t)MAX_PATHNAME_SIZE);
    
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_ztsRetrieve; Pathname: ", cpath);
	}
	tss = zstructTsNewTimes(cpath, startDate, startTime, endDate, endTime); 
	(*env)->ReleaseStringUTFChars(env, j_startDate, startDate);
	(*env)->ReleaseStringUTFChars(env, j_startTime, startTime);
	(*env)->ReleaseStringUTFChars(env, j_endDate, endDate);
	(*env)->ReleaseStringUTFChars(env, j_endTime, endTime);
	if (!tss) {
		//  Error out!
		return -1;
	}
	
	tss->boolRetrieveAllTimes = hec_dss_jni_getBooleanField(env,cls,obj,"retrieveAllTimes",0);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsRetrieve; Retrieve all times set to: ", tss->boolRetrieveAllTimes);
	}

	//  0 is retrieve as stored; 1 is force floats, 2 force doubles
	int retrieveDoublesFlag = 0;

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		ztsMessTimeWindow((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, tss);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsRetrieve.  retrieve flag: ", retrieveFlag);
	}

	status = ztsRetrieve((long long*)ifltab, tss, retrieveFlag, retrieveDoublesFlag, 1);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After ztsRetrieve.  Status: ", status);
		if (status >= 0) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsRetrieve.  dataType: ", tss->dataType);
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsRetrieve; numberValues: ", tss->numberValues);
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsRetrieve; julianBaseDate: ", tss->julianBaseDate);
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsRetrieve; Retrieve all times set to: ", tss->boolRetrieveAllTimes);
			ztsMessTimeWindow((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, tss);
			if (status == STATUS_OKAY) {
				if (tss->units) {
					zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Units: ", tss->units);
				}
				if (tss->type) {
					zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Type: ", tss->type);
				}
			}
		}
	}

	//  Fill in the time series container with data from the time series struct
	if (status == 0) {
		//  Data Type (regular, irregular, profile, float, double...)
		hec_dss_jni_setIntField(env, cls, j_timeSeriesContainer, "dataType", tss->dataType);
		
		//  Number of values (both times, data, quality, notes, etc...)  
		hec_dss_jni_setIntField(env, cls, j_timeSeriesContainer, "numberValues", tss->numberValues);
		
		//  Time related varibles
		//  time interval  

			jnumber = (jint)tss->timeIntervalSeconds;
			if (jnumber >= SECS_IN_1_MINUTE) {
				jnumber /= SECS_IN_1_MINUTE;
				if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "ztsRetrieve.  interval in minutes: ", (int)jnumber);
				}
			}
			else {
				//  Time interval is less than one minute
				if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "ztsRetrieve.  interval in seconds: ", (int)jnumber);
				}
			}
		hec_dss_jni_setIntField(env, cls, j_timeSeriesContainer, "interval", jnumber);
		
		//  timeGranularitySeconds
		//  Number of seconds each unit in times array has, normally 60 (for one minute)
		jnumber = (jint)tss->timeGranularitySeconds;
		if (jnumber == 0) jnumber = SECS_IN_1_MINUTE;
		hec_dss_jni_setIntField(env, cls, j_timeSeriesContainer, "timeGranularitySeconds", jnumber);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "ztsRetrieve.  timeGranularitySeconds: ", (int)jnumber);
		}

		//  Set Time Array
		if ((tss->times) && (tss->numberValues > 0)) {
			size = (jsize)tss->numberValues;
			times = (*env)->NewIntArray(env, size);
			(*env)->SetIntArrayRegion(env, times, 0, size, tss->times); 
			fid = (*env)->GetFieldID (env, cls, "times", "[I");
			if (fid) {
				(*env)->SetObjectField (env, j_timeSeriesContainer, fid, times);
			}
			(*env)->DeleteLocalRef(env, times);
		}

		//  Julian Base Date (days since 01Jan1900).  Allows seconds to work and
		hec_dss_jni_setIntField(env, cls, j_timeSeriesContainer, "julianBaseDate", tss->julianBaseDate);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "ztsRetrieve.  julianBaseDate: ", tss->julianBaseDate);
		}

		hec_dss_jni_updateHecTime(env, cls, j_timeSeriesContainer, "startHecTime", tss->startJulianDate,tss->startTimeSeconds);

		idate = tss->startJulianDate - tss->julianBaseDate;
		itime = tss->startTimeSeconds;
		hec_dss_jni_setIntTimeField(env,cls,j_timeSeriesContainer,"startTime",idate, itime, tss->timeGranularitySeconds);

		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "ztsRetrieve.  startTime: ", (int)jnumber);
		}

		hec_dss_jni_updateHecTime(env, cls, j_timeSeriesContainer, "endHecTime", tss->endJulianDate,tss->endTimeSeconds);

		idate = tss->endJulianDate - tss->julianBaseDate;
		itime = tss->endTimeSeconds;
		hec_dss_jni_setIntTimeField(env, cls, j_timeSeriesContainer, "endTime", idate, itime, tss->timeGranularitySeconds);

		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "ztsRetrieve.  endTime: ", (int)jnumber);
		}

		// Set the units and type 
		hec_dss_jni_setStringField(env,cls, j_timeSeriesContainer, "units", tss->units);
		hec_dss_jni_setStringField(env, cls, j_timeSeriesContainer, "type", tss->type);

		//  Data related

		if (tss->numberValues > 0) {
			//  Quality
			if (tss->quality) {
				jnumber = (jint)tss->numberValues;
				size = (jint)tss->qualityElementSize;
				if (size <= 1) {
					tss->qualityElementSize = 1;
					fid = (*env)->GetFieldID (env, cls, "quality", "[I");
					if (fid) {
						quality = (*env)->NewIntArray(env, jnumber);
						(*env)->SetIntArrayRegion(env, quality, 0, jnumber, tss->quality); 
						(*env)->SetObjectField (env, j_timeSeriesContainer, fid, quality);	
						(*env)->DeleteLocalRef(env, quality);
					}
				}
				else if (size > 1) {
					hec_dss_jni_setIntField(env, cls, j_timeSeriesContainer, "sizeEachQuality7", size);

					intArrayClass = (*env)->FindClass(env, "[I");
					doubleIntDim = (*env)->NewObjectArray(env, jnumber, intArrayClass, 0);				
					for (i=0; i<tss->numberValues; i++) {
						j = i * tss->qualityElementSize;
						quality = (*env)->NewIntArray(env, size);
						(*env)->SetIntArrayRegion(env, quality, 0, size, &tss->quality[j]);
						(*env)->SetObjectArrayElement(env, doubleIntDim, i, quality);
						(*env)->DeleteLocalRef(env, quality);
					}
					fid = (*env)->GetFieldID (env, cls, "quality7", "[[I");
						if ((*env)->ExceptionOccurred(env)) {
							(*env)->ExceptionClear(env);			
					}
					else if (fid) {
						(*env)->SetObjectField (env, j_timeSeriesContainer, fid, doubleIntDim);
					}
					(*env)->DeleteLocalRef(env, doubleIntDim);
				}
			}

			//  Integer notes
			if (tss->inotes) {
				jnumber = (jint)tss->numberValues;
				size = (jint)tss->inoteElementSize;
				if (size == 1) {
					fid = (*env)->GetFieldID (env, cls, "inotes", "[I");
					if ((*env)->ExceptionOccurred(env)) {
							(*env)->ExceptionClear(env);			
					}
					else if (fid) {
						inotes = (*env)->NewIntArray(env, jnumber);
						(*env)->SetIntArrayRegion(env, inotes, 0, jnumber, tss->inotes); 
						(*env)->SetObjectField (env, j_timeSeriesContainer, fid, inotes);	
						(*env)->DeleteLocalRef(env, inotes);
					}
				}
				else if (size > 1) {
					hec_dss_jni_setIntField(env, cls, j_timeSeriesContainer, "sizeEachNote", size);
					
					intArrayClass = (*env)->FindClass(env, "[I");
					doubleIntDim = (*env)->NewObjectArray(env, jnumber, intArrayClass, 0);				
					for (i=0; i<tss->numberValues; i++) {
						j = i * tss->inoteElementSize;
						inotes = (*env)->NewIntArray(env, size);
						(*env)->SetIntArrayRegion(env, inotes, 0, size, &tss->inotes[j]);
						(*env)->SetObjectArrayElement(env, doubleIntDim, i, inotes);
						(*env)->DeleteLocalRef(env, inotes);
					}
					fid = (*env)->GetFieldID (env, cls, "inotes", "[[I");
					if ((*env)->ExceptionOccurred(env)) {
							(*env)->ExceptionClear(env);			
					}
					else if (fid) {
						(*env)->SetObjectField (env, j_timeSeriesContainer, fid, doubleIntDim);
					}
					(*env)->DeleteLocalRef(env, doubleIntDim);
				}
			}
		
			//  Character notes
			else if (tss->cnotes) {
				jnumber = (jint)tss->numberValues;				
				stringClass = (*env)->FindClass(env, "Ljava/lang/String;");
				stringArray = (*env)->NewObjectArray(env, jnumber, stringClass, 0);	
				ipos = 0;
				lencnotesRead = tss->cnotesLengthTotal;
				for (i=0; i<tss->numberValues; i++) {
					len = (int)strnlen_hec(&tss->cnotes[ipos], lencnotesRead);
					jstr = (*env)->NewStringUTF(env, &tss->cnotes[ipos]);
					(*env)->SetObjectArrayElement(env, stringArray, i, jstr);
					(*env)->DeleteLocalRef(env, jstr);				
					ipos += len + 1;
					lencnotesRead -= len + 1;
					if (lencnotesRead <= 0) {
						break;
					}
				}
				fid = (*env)->GetFieldID (env, cls, "cnotes", "[Ljava/lang/String;");
				if ((*env)->ExceptionOccurred(env)) {
						(*env)->ExceptionClear(env);			
				}
				else if (fid) {
					(*env)->SetObjectField (env, j_timeSeriesContainer, fid, stringArray);
				}	
				(*env)->DeleteLocalRef(env, stringArray);
			}

			//  Data is requested as doubles, regardless of how it was stored (hopefully floats!)
			if ((tss->floatProfileDepths == 0) && (tss->doubleProfileDepths == 0)) {
				//  Non-profile (normal) data
				int storedAsdoubles = tss->doubleValues ? 1 : 0;
				hec_dss_jni_setBooleanField(env, cls,j_timeSeriesContainer, "storedAsdoubles",storedAsdoubles );
				if (tss->floatValues) {
					//  Convert float to doubles
					if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
						zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "ztsRetrieve.  Data stored as floats", "");
					}
					tss->doubleValues = (double *)malloc(tss->numberValues * 8);
					convertDataArray((void *)tss->floatValues, (void *)tss->doubleValues, tss->numberValues, 1, 2);
					boolMalloced = 1;
				}
				else {
					boolMalloced = 0;
					if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
						zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "ztsRetrieve.  Data stored as doubles", "");
					}
				}
				if (tss->doubleValues) {				
					size = (jsize)tss->numberValues;
					values = (*env)->NewDoubleArray(env, size);
					(*env)->SetDoubleArrayRegion(env, values, 0, size, tss->doubleValues); 
					fid = (*env)->GetFieldID (env, cls, "values", "[D");
					if (fid) {
						(*env)->SetObjectField (env, j_timeSeriesContainer, fid, values);
					}
					(*env)->DeleteLocalRef(env, values);
				}
				if (boolMalloced) {
					free(tss->doubleValues);
					tss->doubleValues = 0;
				}
			}
			else {
				//  Profile Data
				//  Number of Depths (length of doubleDepths array)  
				if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "ztsRetrieve.  Data is profile data, number depths: ", tss->profileDepthsNumber);
				}
				hec_dss_jni_setIntField(env, cls, j_timeSeriesContainer, "numberDepths", tss->profileDepthsNumber);
				
				if (!tss->doubleProfileDepths && !tss->floatProfileDepths) {
					//   No profile data?
					return -1;
				}
				if (tss->floatProfileDepths) {
					//  Convert floats to doubles
					tss->doubleProfileDepths = (double *)malloc(tss->profileDepthsNumber * 8);
					convertDataArray((void *)tss->floatProfileDepths, (void *)tss->doubleProfileDepths, tss->profileDepthsNumber, 1, 2);				
					boolMalloced = 1;
				}
				else {
					boolMalloced = 0;
				}
				if (tss->doubleProfileDepths) {
					size = (jsize)tss->profileDepthsNumber;
					profileDepths = (*env)->NewDoubleArray(env, size);
					(*env)->SetDoubleArrayRegion(env, profileDepths, 0, size, tss->doubleProfileDepths); 
					fid = (*env)->GetFieldID (env, cls, "profileDepths", "[D");
					if ((*env)->ExceptionOccurred(env)) {
							(*env)->ExceptionClear(env);			
					}
					else if (fid) {
						(*env)->SetObjectField (env, j_timeSeriesContainer, fid, profileDepths);
					}
					(*env)->DeleteLocalRef(env, profileDepths);
				}
				if (boolMalloced) {
					free(tss->doubleProfileDepths);
					tss->doubleProfileDepths = 0;
				}
				if (!tss->doubleProfileValues && !tss->floatProfileValues) {
					//   No profile data?
					return -1;
				}
				number = tss->numberValues * tss->profileDepthsNumber;
				if (tss->floatProfileValues) {
					//  Convert floats to doubles
					tss->doubleProfileValues = (double *)malloc(number * 8);
					convertDataArray((void *)tss->floatProfileValues, (void *)tss->doubleProfileValues, number, 1, 2);				
					boolMalloced = 1;
				}
				else {
					boolMalloced = 0;
				}
				if (tss->doubleProfileValues) {	
					//  Create and copy the double dimensioned array
					jnumber = (jint)tss->numberValues;
					doubleArrayClass = (*env)->FindClass(env, "[D");
					doubleDim = (*env)->NewObjectArray(env, jnumber, doubleArrayClass, 0);
					size = (jsize)tss->profileDepthsNumber;
					for (i=0; i<tss->numberValues; i++) {
						j = i * tss->profileDepthsNumber;
						values = (*env)->NewDoubleArray(env, size);
						(*env)->SetDoubleArrayRegion(env, values, 0, size, &tss->doubleProfileValues[j]);
						(*env)->SetObjectArrayElement(env, doubleDim, i, values);
						(*env)->DeleteLocalRef(env, values);
					}
					fid = (*env)->GetFieldID (env, cls, "profileValues", "[[D");
					if ((*env)->ExceptionOccurred(env)) {
						(*env)->ExceptionClear(env);			
					}
					else if (fid) {
						(*env)->SetObjectField (env, j_timeSeriesContainer, fid, doubleDim);
					}
					(*env)->DeleteLocalRef(env, doubleDim);
				}
				if (boolMalloced) {
					free(tss->doubleProfileValues);
					tss->doubleProfileValues = 0;
				}
				hec_dss_jni_setStringField(env, cls, j_timeSeriesContainer, "unitsProfileValues", tss->unitsProfileValues);
				hec_dss_jni_setStringField(env, cls, j_timeSeriesContainer, "unitsProfileDepths", tss->unitsProfileDepths);
			} // end profile
		}

		//  Precision
		hec_dss_jni_setIntField(env, cls, j_timeSeriesContainer, "precision", tss->precision);

		//  Time zone
		hec_dss_jni_setStringField(env, cls, j_timeSeriesContainer, "timeZoneID", tss->timeZoneName);

		//  User header (supplemental)
		if (tss->userHeaderNumber > 0) {
			char* headerString = NULL;
			if (zgetVersion((long long*)ifltab) == 7) {
				headerString = userHeaderToString(tss->userHeader, tss->userHeaderNumber);
			}
			else {
				headerString = (char*)tss->userHeader;
			}
			hec_dss_jni_setStringField(env, cls, j_timeSeriesContainer, "supplementalInfo", headerString);
			if (zgetVersion((long long*)ifltab) == 7) {
				free(headerString);
			}
		}
		//  Now fill in extra container values
		//  watershed
		len = zpathnameGetPart (tss->pathname, 1, cpart, sizeof(cpart));
		if (len > 0) {
			hec_dss_jni_setStringField(env, cls, j_timeSeriesContainer, "watershed", cpart);
		}

		//  location
		len = zpathnameGetPart (tss->pathname, 2, cpart, sizeof(cpart));
		if (len > 0) {
			hec_dss_jni_setStringField(env, cls, j_timeSeriesContainer, "location", cpart);
		}

		//  parameter
		len = zpathnameGetPart (tss->pathname, 3, cpart, sizeof(cpart));
		if (len > 0) {
			hec_dss_jni_setStringField(env, cls, j_timeSeriesContainer, "parameter", cpart);
		}

		//  version
		len = zpathnameGetPart (tss->pathname, 6, cpart, sizeof(cpart));
		if (len > 0) {
			hec_dss_jni_setStringField(env, cls, j_timeSeriesContainer, "version", cpart);
		}

		//  Last write time
		hec_dss_jni_setLongField(env, cls, j_timeSeriesContainer, "lastWriteTimeMillis", tss->lastWrittenTime);
		
		//  File last write time
		hec_dss_jni_setLongField(env, cls, j_timeSeriesContainer, "fileLastWriteTimeMillis", tss->fileLastWrittenTime);
		

		if (tss->locationStruct) {						
			Hec_zlocationFromStruct(env, obj, j_timeSeriesContainer, tss->locationStruct);
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
				zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsRetrieve; Location infomation loaded; coord system: ", tss->locationStruct->coordinateSystem);
			}
		}
		else {
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
				zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsRetrieve; No location infomation found", "");
			}
		}
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_ztsRetrieve, status: ", status);
	}

	zstructFree(tss);
  
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);	
	

	///*************   FIX ME - Release time series container ???

	

	return (jint)status;
}

