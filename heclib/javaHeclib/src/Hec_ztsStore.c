#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"
#include "javaHeclib.h"
#include "verticalDatum.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1ztsStore(
	JNIEnv       *env, 
	jobject       obj, 
	jintArray    j_ifltab,		                                               
	jobject		  j_timeSeriesContainer,		 
	jint		  j_storageFlag)   
	                               
{

	const char *cpath;

	jclass cls;
    jfieldID fid;
	jclass cls2;
	jfieldID fid2;

    jstring j_cpath;
	jboolean jbool;
	jintArray times;
	jint *itimes;
	jintArray quality;
	jint *iquality;
	jintArray notes;
	jint *inote;
	jdoubleArray values;
	jdouble *vals;
	jdoubleArray profileDepths;
	jdouble *depths;
	jstring jstr;	
	jobject hecTime;
	jobjectArray doubleDim;	
	jobjectArray objectArray;	


	int i, j, k;
	int storageFlag;
	int storeDoubles;
	int number;
	int count;
	int len;
	int status;
	int startTime;
	int endTime;
	int julian;
	int seconds;
	int timeMultiplier;
	const char *cstr;

	zStructTimeSeries *tss;

	
	int *ifltab;


	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);

	ifltab		= (*env)->GetIntArrayElements(env, j_ifltab, 0);	
	storageFlag = (int)j_storageFlag;

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_timeSeriesContainer);
    fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_cpath = (*env)->GetObjectField(env, j_timeSeriesContainer, fid); 
        cpath = (*env)->GetStringUTFChars(env, j_cpath,  0);
		
    }
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PATHNAME,
			0, 0, zdssErrorSeverity.WARNING, "", "");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_ztsStore; Pathname: ", cpath);
	}
	tss = zstructTsNew(cpath); 

	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);

	fid = (*env)->GetFieldID (env, cls, "dataType", "I");
	if (fid) {		
		number = (int)(*env)->GetIntField(env, j_timeSeriesContainer, fid);
		tss->dataType = number;
	}

	//  Number of values (both times, data, quality, notes, etc...)  
	fid = (*env)->GetFieldID (env, cls, "numberValues", "I");
	if (fid) {
		number = (int)(*env)->GetIntField(env, j_timeSeriesContainer, fid);
		tss->numberValues = number;
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tss->pathname, "numberValues");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(tss);
		return status;
	}

	//  Time related varibles
	//  Julian base date (Usually 0)
	fid = (*env)->GetFieldID (env, cls, "julianBaseDate", "I");
	if (fid) {
		number = (int)(*env)->GetIntField(env, j_timeSeriesContainer, fid);
		tss->julianBaseDate = number;
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_DATE_TIME,
			0, 0, zdssErrorSeverity.WARNING, tss->pathname, "Julian base date");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(tss);
		return status;
	}	

	//  start time
	fid = (*env)->GetFieldID (env, cls, "startTime", "I");
	if (fid) {
		startTime = (int)(*env)->GetIntField(env, j_timeSeriesContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_DATE_TIME,
			0, 0, zdssErrorSeverity.WARNING, tss->pathname, "startTime");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(tss);
		return status;
	}

	//  See if hecStartTime is used and defined for the start (instead of start time in minutes)
	//  This is used in extended dates where the start time may overflow
	fid = (*env)->GetFieldID(env, cls, "startHecTime", "Lhec/heclib/util/HecTime;");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		fid = 0;
	}
	if (fid) {
		hecTime = (*env)->GetObjectField(env, j_timeSeriesContainer, fid);
		if (hecTime) {
			cls2 = (*env)->GetObjectClass(env, hecTime);
			if (cls2) {
				julian = 0;
				seconds = 0;
				fid2 = (*env)->GetFieldID(env, cls2, "_julian", "I");
				if (fid2) {
					julian = (int)(*env)->GetIntField(env, hecTime, fid2);
				}
				fid2 = (*env)->GetFieldID(env, cls2, "_secondsSinceMidnight", "I");
				if (fid2) {
					seconds = (int)(*env)->GetIntField(env, hecTime, fid2);
				}
				//  0, 0 is an illegal date/time and is often used for unset
				if ((julian != 0) && (seconds > 0)) {
					if (julian != UNDEFINED_TIME) {
						tss->startJulianDate = julian;
						tss->startTimeSeconds = seconds;
						startTime = 0;
						if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
							zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; HecTime startTime specified, startJulianDate: ", julian);
							zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; HecTime  startTimeSeconds: ", seconds);
						}
					}
				}
			}
		}
	}
	else {
		//  This is okay; for older Java code without extended dates
	}


	//  end time
	fid = (*env)->GetFieldID (env, cls, "endTime", "I");
	if (fid) {
		endTime = (int)(*env)->GetIntField(env, j_timeSeriesContainer, fid);
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_DATE_TIME,
			0, 0, zdssErrorSeverity.WARNING, tss->pathname, "endTime");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		zstructFree(tss);
		return status;
	}

	fid = (*env)->GetFieldID(env, cls, "endHecTime", "Lhec/heclib/util/HecTime;");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		fid = 0;
	}
	if (fid) {
		hecTime = (*env)->GetObjectField(env, j_timeSeriesContainer, fid);
		if (hecTime) {
			cls2 = (*env)->GetObjectClass(env, hecTime);
			if (cls2) {
				julian = 0;
				seconds = 0;
				fid2 = (*env)->GetFieldID(env, cls2, "_julian", "I");
				if (fid2) {
					julian = (int)(*env)->GetIntField(env, hecTime, fid2);
				}
				fid2 = (*env)->GetFieldID(env, cls2, "_secondsSinceMidnight", "I");
				if (fid2) {
					seconds = (int)(*env)->GetIntField(env, hecTime, fid2);
				}
				//  0, 0 is an illegal date/time and is often used for unset
				if ((julian != 0) && (seconds > 0)) {
					if (julian != UNDEFINED_TIME) {
						tss->endJulianDate = julian;
						tss->endTimeSeconds = seconds;
						endTime = 0;
						if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
							zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; HecTime endTime specified, endJulianDate: ", julian);
							zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; HecTime  endTimeSeconds: ", seconds);
						}
					}
				}
			}
		}
	}
	else {
		//  This is okay; for older Java code without extended dates
	}


	//  timeGranularitySeconds
	//  Number of seconds each unit in times array has, normally SECS_IN_1_MINUTE (for one minute)
	//  Can be 1, SECS_IN_1_MINUTE, SECS_IN_1_HOUR, SECS_IN_1_DAY
	fid = (*env)->GetFieldID (env, cls, "timeGranularitySeconds", "I");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		number = (int)(*env)->GetIntField(env, j_timeSeriesContainer, fid);
		tss->timeGranularitySeconds = number;
	}
	if (tss->timeGranularitySeconds == 0) tss->timeGranularitySeconds = SECS_IN_1_MINUTE;
	
	if (startTime != 0) {
		if (tss->timeGranularitySeconds == SECS_IN_1_MINUTE) {
			//  Times in minutes (most common)
			tss->startJulianDate = startTime / MINS_IN_1_DAY;
			tss->startTimeSeconds = startTime - (tss->startJulianDate * MINS_IN_1_DAY);
			tss->startTimeSeconds *= SECS_IN_1_MINUTE;
			tss->endJulianDate = endTime / MINS_IN_1_DAY;
			tss->endTimeSeconds = endTime - (tss->endJulianDate * MINS_IN_1_DAY);
			tss->endTimeSeconds *= SECS_IN_1_MINUTE;
		}
		else {
			timeMultiplier = SECS_IN_1_DAY / tss->timeGranularitySeconds;
			tss->startJulianDate = startTime / timeMultiplier;
			tss->startTimeSeconds = startTime - (tss->startJulianDate * timeMultiplier);
			tss->endJulianDate = endTime / timeMultiplier;
			tss->endTimeSeconds = endTime - (tss->endJulianDate * timeMultiplier);
		}
	}

	//  In Java TimeSeriesContainer, start and end time are from julian base date,
	//  In timeSeriesStructs, startJulian is independent of base date. (it is days from 01Jan1900, not base date)
	//  This means that the base date has to be added when going from TimeSeriesContainer to timeSeriesStructs
	//  (Usually base date is zero)
	//tss->startJulianDate += tss->julianBaseDate;
	//tss->endJulianDate += tss->julianBaseDate;





	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; numberValues: ", tss->numberValues);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; startTime: ", startTime);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; endTime: ", endTime);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; julianBaseDate: ", tss->julianBaseDate);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; timeGranularitySeconds: ", tss->timeGranularitySeconds);
		ztsMessTimeWindow((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, tss);
	}

	//  Set Time Array
	//tss->times = new int[tss->numberValues];	
	fid = (*env)->GetFieldID (env, cls, "times", "[I");
	if (fid) {
		times = (*env)->GetObjectField (env, j_timeSeriesContainer, fid);
		if (times) {
			itimes = (*env)->GetIntArrayElements(env, times, 0);
			if (itimes) {
				tss->times = (int *)calloc(tss->numberValues, 4);
				tss->allocated[zSTRUCT_TS_times] = 1;
				for (i=0; i<tss->numberValues; i++) {
					tss->times[i] = (int)itimes[i];
				}
			}
			if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; first time (raw): ", tss->times[0]);
			}
			(*env)->ReleaseIntArrayElements(env, times, itimes, 0);
		}
	}
	
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {
		if (tss->times) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; first time (raw): ", tss->times[0]);
		}
	}

	//  Units
	fid = (*env)->GetFieldID (env, cls, "units", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_timeSeriesContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tss->units = mallocAndCopy(cstr);
				tss->allocated[zSTRUCT_TS_units] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	//  Type
	fid = (*env)->GetFieldID (env, cls, "type", "Ljava/lang/String;");
	if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_timeSeriesContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tss->type = mallocAndCopy(cstr);
				tss->allocated[zSTRUCT_TS_type] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	//  Precision
	fid = (*env)->GetFieldID (env, cls, "precision", "I");
	if (fid) {		
		number = (int)(*env)->GetIntField(env, j_timeSeriesContainer, fid);
		tss->precision = number;
	}

	//  Store data as doubles? (not recommended, unless required)
	storeDoubles = 0;
	fid = (*env)->GetFieldID (env, cls, "storedAsdoubles", "Z");
	if (fid) {
		jbool = (*env)->GetBooleanField(env, j_timeSeriesContainer, fid);
		if (jbool) {
			storeDoubles = 1;
		}
	}

	fid = (*env)->GetFieldID (env, cls, "values", "[D");
	if (fid) {
		values = (*env)->GetObjectField (env, j_timeSeriesContainer, fid);
		if (values) {
			vals = (*env)->GetDoubleArrayElements(env, values, 0);
			if (storeDoubles) {
				tss->doubleValues = (double *)calloc(tss->numberValues, 8);
				tss->allocated[zSTRUCT_TS_doubleValues] = 1;
				for (i=0; i<tss->numberValues; i++) {
					tss->doubleValues[i] = vals[i];
				}				
			}
			else {
				tss->floatValues = (float *)calloc(tss->numberValues, 4);
				tss->allocated[zSTRUCT_TS_floatValues] = 1;
				for (i=0; i<tss->numberValues; i++) {
					tss->floatValues[i] = (float)vals[i];
				}
			}
			(*env)->ReleaseDoubleArrayElements(env, values, vals, 0);
		}
	}

	//  Profile Data
	//  Number of Depths (length of doubleDepths array)  
	fid = (*env)->GetFieldID (env, cls, "numberDepths", "I");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		number = (int)(*env)->GetIntField(env, j_timeSeriesContainer, fid);
		if (number > 0) {
			tss->profileDepthsNumber = number;	
			//tss->doubleProfileDepths = new double[tss->profileDepthsNumber];
			tss->doubleProfileDepths = (double *)calloc(tss->profileDepthsNumber, 8);
			tss->allocated[zSTRUCT_TS_profileDoubleDepths] = 1;
			fid = (*env)->GetFieldID (env, cls, "profileDepths", "[D");
			if (fid) {
				profileDepths = (*env)->GetObjectField (env, j_timeSeriesContainer, fid);
				depths = (*env)->GetDoubleArrayElements(env, profileDepths, 0);
				for (i=0; i<tss->profileDepthsNumber; i++) {
					tss->doubleProfileDepths[i] = (double)depths[i];
				}
				(*env)->ReleaseDoubleArrayElements(env, profileDepths, depths, 0);
			}

			//tss->doubleProfileValues = new double[(tss->profileDepthsNumber * tss->numberValues)];
			tss->doubleProfileValues = (double *)calloc((tss->profileDepthsNumber * tss->numberValues), 8);
			tss->allocated[zSTRUCT_TS_profileDoubleValues] = 1;
			fid = (*env)->GetFieldID (env, cls, "profileValues", "[[D");
			if (fid) {
				doubleDim = (*env)->GetObjectField (env, j_timeSeriesContainer, fid);		
				for (i=0; i<tss->numberValues; i++) {
					values = (*env)->GetObjectArrayElement(env, doubleDim, i);
					vals = (*env)->GetDoubleArrayElements(env, values, 0);
					for (j=0; j<tss->profileDepthsNumber; j++) {
						k = (i * tss->profileDepthsNumber) + j;
						tss->doubleProfileValues[k] = (double)vals[j];
					}
					(*env)->ReleaseDoubleArrayElements(env, values, vals, 0);
					(*env)->DeleteLocalRef(env, values);
				}
			}
		}
	}

	//  unitsProfileDepths
	fid = (*env)->GetFieldID (env, cls, "unitsProfileDepths", "Ljava/lang/String;");
    if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		jstr = (*env)->GetObjectField(env, j_timeSeriesContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tss->unitsProfileDepths = mallocAndCopy(cstr);
				tss->allocated[zSTRUCT_TS_profileUnitsDepths] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	//  unitsProfileValues
	fid = (*env)->GetFieldID (env, cls, "unitsProfileValues", "Ljava/lang/String;");
    if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		jstr = (*env)->GetObjectField(env, j_timeSeriesContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tss->unitsProfileValues = mallocAndCopy(cstr);
				tss->allocated[zSTRUCT_TS_profileUnitsValues] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	//  profileLabel
/*	fid = (*env)->GetFieldID (env, cls, "profileLabel", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_timeSeriesContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tss->profileLabel = mallocAndCopy(cstr);
				tss->allocated[zSTRUCT_TS_profileUnitsValues] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }
*/
	
	//  Quality
	//tss->quality = new int[tss->numberValues];	
	fid = (*env)->GetFieldID (env, cls, "quality", "[I");
	if (fid) {
		quality = (*env)->GetObjectField (env, j_timeSeriesContainer, fid);
		if (quality) {
			iquality = (*env)->GetIntArrayElements(env, quality, 0);
			if (iquality) {
				tss->quality = (int *)calloc(tss->numberValues, 4);
				tss->qualityElementSize = 1;
				tss->allocated[zSTRUCT_TS_quality] = 1;
				for (i=0; i<tss->numberValues; i++) {
					tss->quality[i] = (int)iquality[i];
				}
			}
			(*env)->ReleaseIntArrayElements(env, quality, iquality, 0);
			(*env)->DeleteLocalRef(env, quality);
		}
	}

	//  quality7
	//  size each quality (in ints) (length of quality7 array)  
	fid = (*env)->GetFieldID (env, cls, "sizeEachQuality7", "I");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		number = (int)(*env)->GetIntField(env, j_timeSeriesContainer, fid);
		if (number > 0) {
			tss->qualityElementSize = number;			
			tss->quality = (int *)calloc(tss->numberValues, (tss->qualityElementSize * 4));
			tss->allocated[zSTRUCT_TS_quality] = 1;
			fid = (*env)->GetFieldID (env, cls, "quality7", "[[I");
			if (fid) {
				doubleDim = (*env)->GetObjectField (env, j_timeSeriesContainer, fid);		
				for (i=0; i<tss->numberValues; i++) {
					quality = (*env)->GetObjectArrayElement(env, doubleDim, i);
					iquality = (*env)->GetIntArrayElements(env, quality, 0);
					for (j=0; j<tss->qualityElementSize; j++) {
						k = (i * tss->qualityElementSize) + j;
						tss->quality[k] = iquality[j];
					}
					(*env)->ReleaseIntArrayElements(env, quality, iquality, 0);
					(*env)->DeleteLocalRef(env, quality);
				}
				(*env)->DeleteLocalRef(env, doubleDim);
			}
		}
	}

	//  inotes
	//  size each note (in ints) 
	fid = (*env)->GetFieldID (env, cls, "sizeEachNote", "I");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		number = (int)(*env)->GetIntField(env, j_timeSeriesContainer, fid);
		if (number > 0) {
			tss->inoteElementSize = number;			
			tss->inotes = (int *)calloc(tss->numberValues, (tss->inoteElementSize * 4));
			tss->allocated[zSTRUCT_TS_inotes] = 1;
			fid = (*env)->GetFieldID (env, cls, "inotes", "[[I");
			if (fid) {
				doubleDim = (*env)->GetObjectField (env, j_timeSeriesContainer, fid);		
				for (i=0; i<tss->numberValues; i++) {
					notes = (*env)->GetObjectArrayElement(env, doubleDim, i);
					inote = (*env)->GetIntArrayElements(env, notes, 0);
					for (j=0; j<tss->inoteElementSize; j++) {
						k = (i * tss->inoteElementSize) + j;
						tss->inotes[k] = inote[j];
					}
					(*env)->ReleaseIntArrayElements(env, notes, inote, 0);
					(*env)->DeleteLocalRef(env, notes);
				}
				(*env)->DeleteLocalRef(env, doubleDim);
			}
		}
	}

	//  cnotes
	fid = (*env)->GetFieldID (env, cls, "cnotes", "[Ljava/lang/String;");
	if ((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);			
	}
	else if (fid) {
		objectArray = (*env)->GetObjectField (env, j_timeSeriesContainer, fid);
		if (objectArray) {
			count = 0;
			for (i=0; i<tss->numberValues; i++) {
				jstr = (jstring) (*env)->GetObjectArrayElement(env, objectArray, i);
				if (!jstr) break;
				cstr = (*env)->GetStringUTFChars(env, jstr, 0);
				if (cstr) {
					count += (int)strlen(cstr);
					count++;  //  For "\0" end of each string
				}
				else {
					break;
				}
				(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
			}
			if (count > 0) {
				tss->cnotesLengthTotal = count;
				tss->cnotes = (char *)calloc(count, 1);
				tss->allocated[zSTRUCT_TS_cnotes] = 1;
				count = 0;
				for (i=0; i<tss->numberValues; i++) {
					jstr = (jstring) (*env)->GetObjectArrayElement(env, objectArray, i);
					cstr = (*env)->GetStringUTFChars(env, jstr, 0);
					if (cstr) {
						len = (int)strlen(cstr);
						for (j=0; j<len; j++) {
							tss->cnotes[count++] = cstr[j];
						}
						tss->cnotes[count++] = '\0';
					}
					(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
				}
			}
		}
	}

	//  User header
	fid = (*env)->GetFieldID (env, cls, "supplementalInfo", "Ljava/lang/String;");
	if (fid) {
		jstr = (*env)->GetObjectField(env, j_timeSeriesContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			tss->userHeader = stringToUserHeader(cstr, &tss->userHeaderNumber);
			tss->userHeaderSize = tss->userHeaderNumber;
			tss->allocated[zSTRUCT_userHeader] = 1;
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	//  Time zone
	fid = (*env)->GetFieldID (env, cls, "timeZoneID", "Ljava/lang/String;");
	if (fid) {
		jstr = (*env)->GetObjectField(env, j_timeSeriesContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tss->timeZoneName = mallocAndCopy(cstr);
				tss->allocated[zSTRUCT_timeZoneName] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; storing location info", "");
	}

	Hec_zlocationToStruct(env, obj, j_timeSeriesContainer, tss->locationStruct);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Hec_ztsStore; ready to store, storageFlag: ", storageFlag);
	}
	status = ztsStore((long long*)ifltab, tss, storageFlag);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Exit Hec_ztsStore.  Status: ", status);
	}

	if (tss->times) {
		free(tss->times);
		tss->times = 0;
	}
	if (tss->doubleValues) {
		free(tss->doubleValues);
		tss->doubleValues = 0;
	}
	if (tss->floatValues) {
		free(tss->floatValues);
		tss->floatValues = 0;
	}

	zstructFree(tss);

	//-------------------//
	// release variables //
	//-------------------//
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}
