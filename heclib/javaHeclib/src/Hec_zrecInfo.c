#include <jni.h>
#include <string.h>
#include "heclib.h"
#include "jni_utility.h"

/*
	This function is meant to be complete, not efficient
	It is not expected to be called often

	See zgetRecordBasics for a simpler more efficient call
*/

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zrecInfo(
	JNIEnv      *env, 
	jobject     obj, 
	jintArray   j_ifltab,		                                               
	jobject		j_recordInfo,		
	jstring		j_startDate,   
	jstring		j_startTime,   
	jstring		j_endDate,   
	jstring		j_endTime)	                               
{

	const char *cpath;
	const char *startDate;
	const char *startTime;
	const char *endDate;
	const char *endTime;
	int len;

	const char *recordTypeDescription;

	jclass cls;
    jfieldID fid;

    jstring j_cpath;
	jstring jstr;
	jint jnumber;
	jlong jlongNumber;

	int dataType;
	int status;
	zStructTimeSeries *tss;

	int *ifltab;
	long long addresses[15];


	jint capacity=40;
	(*env)->EnsureLocalCapacity(env, capacity);
	
	tss = 0;

	ifltab		= (*env)->GetIntArrayElements(env, j_ifltab, 0);	
	startDate	= (*env)->GetStringUTFChars(env, j_startDate,  0);
	startTime	= (*env)->GetStringUTFChars(env, j_startTime,  0);
	endDate		= (*env)->GetStringUTFChars(env, j_endDate,  0);
	endTime		= (*env)->GetStringUTFChars(env, j_endTime,  0);
	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_recordInfo);
    fid = (*env)->GetFieldID (env, cls, "pathname", "Ljava/lang/String;");
    if (fid != 0) {
		j_cpath = (*env)->GetObjectField(env, j_recordInfo, fid); 
        cpath = (*env)->GetStringUTFChars(env, j_cpath,  0);
    }


	//  Get the data type
	dataType = zdataType ((long long*)ifltab, cpath);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_1zrecInfo, Enter.  Pathname: ", cpath);
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Heclib_Hec_1zrecInfo, data type: ", dataType);
	}

	//  Data Type (regular, irregular, profile, float, double...)
	fid = (*env)->GetFieldID (env, cls, "recordType", "I");
	if (fid) {
		jnumber = (jint)dataType;
		(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
	}
	if (dataType > 0) {
		recordTypeDescription = ztypeName(dataType, 1);
		fid = (*env)->GetFieldID (env, cls, "recordTypeDescription", "Ljava/lang/String;");
		if (fid) {
			jstr = (*env)->NewStringUTF(env, recordTypeDescription);
			(*env)->SetObjectField (env, j_recordInfo, fid, jstr);
		}
	}

	
		
	status = zrecordAddresses((long long*)ifltab, cpath, addresses);

	//  Address type info
	fid = (*env)->GetFieldID (env, cls, "tableHash", "J");
	if (fid) {
		jlongNumber = (jlong)addresses[0];
		(*env)->SetLongField(env, j_recordInfo, fid, jlongNumber);
	}
	fid = (*env)->GetFieldID (env, cls, "hashCode", "J");
	if (fid) {
		jlongNumber = (jlong)addresses[1];
		(*env)->SetLongField(env, j_recordInfo, fid, jlongNumber);
	}
	fid = (*env)->GetFieldID (env, cls, "hashAddress", "J");
	if (fid) {
		jlongNumber = (jlong)addresses[2];
		(*env)->SetLongField(env, j_recordInfo, fid, jlongNumber);
	}
	fid = (*env)->GetFieldID (env, cls, "binLength", "J");
	if (fid) {
		jlongNumber = (jlong)addresses[3];
		(*env)->SetLongField(env, j_recordInfo, fid, jlongNumber);
	}

	if (status == STATUS_RECORD_NOT_FOUND) {
		//  Record does not exist
		fid = (*env)->GetFieldID (env, cls, "exists", "Z");
		if (fid) {
			(*env)->SetBooleanField(env, j_recordInfo, fid, (jboolean)0);
		}
	}
	else {
		fid = (*env)->GetFieldID (env, cls, "exists", "Z");
		if (fid) {
			(*env)->SetBooleanField(env, j_recordInfo, fid, (jboolean)1);
		}
		fid = (*env)->GetFieldID (env, cls, "binAddress", "J");
		if (fid) {
			jlongNumber = (jlong)addresses[4];
			(*env)->SetLongField(env, j_recordInfo, fid, jlongNumber);
		}
		fid = (*env)->GetFieldID (env, cls, "infoLength", "J");
		if (fid) {
			jlongNumber = (jlong)addresses[5];
			(*env)->SetLongField(env, j_recordInfo, fid, jlongNumber);
		}
		fid = (*env)->GetFieldID (env, cls, "infoAddress", "J");
		if (fid) {
			jlongNumber = (jlong)addresses[6];
			(*env)->SetLongField(env, j_recordInfo, fid, jlongNumber);
		}
		fid = (*env)->GetFieldID (env, cls, "dataLength", "J");
		if (fid) {
			jlongNumber = (jlong)addresses[7];
			(*env)->SetLongField(env, j_recordInfo, fid, jlongNumber);
		}
		fid = (*env)->GetFieldID (env, cls, "dataAddress", "J");
		if (fid) {
			jlongNumber = (jlong)addresses[8];
			(*env)->SetLongField(env, j_recordInfo, fid, jlongNumber);
		} 
		fid = (*env)->GetFieldID (env, cls, "spaceAllocated", "I");
		if (fid) {
			jnumber = (jint)addresses[9];
			(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
		}

		//  Other
		fid = (*env)->GetFieldID (env, cls, "recordTag", "Ljava/lang/String;");
		if (fid) {
			jstr = (*env)->NewStringUTF(env, "");
			(*env)->SetObjectField (env, j_recordInfo, fid, jstr);
		}
		fid = (*env)->GetFieldID (env, cls, "timeSeriesCompression", "I");
		if (fid) {
			jnumber = (jint)0;
			(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
		}

		//  If this is time series, get additional time series data also.

		if ((dataType >= DATA_TYPE_RTS) && (dataType < DATA_TYPE_PD)) {

			tss = zstructTsNewTimes(cpath, startDate, startTime, endDate, endTime);
			if (!tss) {
				//  Error out!
				return -1;
			}
			status = ztsRetrieve((long long*)ifltab, tss, 0, 1, 1);

			//////////////////////////////////////////////
			//////////////////////////////////////////////////
			//   FIX ME - HAVE ONE STRUCT THAT GETS ALL INFO 
			//   ABOUT A SINGLE RECORD (BOTH TS, PAIRED, ETC.)
			//   ANOTHER THAT COMBINES THEM FOR MULIPLE RECS
			//   COMBINE zrecordAddresses AND ztsGetSizesInternal
			///////////////////////////////////////////////////


			if (tss->dataType > 0) {

				//  Data Type (regular, irregular, profile, float, double...)
				fid = (*env)->GetFieldID(env, cls, "recordType", "I");
				if (fid) {
					jnumber = (jint)tss->dataType;
					(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
				}

				//  Number of values (both times, data, quality, notes, etc...)  
				fid = (*env)->GetFieldID(env, cls, "numberData", "I");
				if (fid) {
					jnumber = (jint)tss->numberValues;
					(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
				}

				//  Doubles
				if (tss->sizeEachValueRead == 2) {
					fid = (*env)->GetFieldID(env, cls, "doubles", "Z");
					if (fid) {
						(*env)->SetBooleanField(env, j_recordInfo, fid, (jboolean)1);
					}
				}
				else {
					fid = (*env)->GetFieldID(env, cls, "doubles", "Z");
					if (fid) {
						(*env)->SetBooleanField(env, j_recordInfo, fid, (jboolean)0);
					}
				}
				fid = (*env)->GetFieldID(env, cls, "sizeEachValue", "I");
				if (fid) {
					jnumber = (jint)tss->sizeEachValueRead;
					(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
				}

				//  Quality
				if (tss->qualityElementSize > 0) {
					fid = (*env)->GetFieldID(env, cls, "quality", "Z");
					if (fid) {
						(*env)->SetBooleanField(env, j_recordInfo, fid, (jboolean)1);
					}
				}
				else {
					fid = (*env)->GetFieldID(env, cls, "quality", "Z");
					if (fid) {
						(*env)->SetBooleanField(env, j_recordInfo, fid, (jboolean)0);
					}
				}
				fid = (*env)->GetFieldID(env, cls, "sizeEachQuality", "I");
				if (fid) {
					jnumber = (jint)tss->qualityElementSize;
					(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
				}

				//   Notes
				fid = (*env)->GetFieldID(env, cls, "sizeEachInote", "I");
				if (fid) {
					jnumber = (jint)tss->inoteElementSize;
					(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
				}
				fid = (*env)->GetFieldID(env, cls, "lengthCnotes", "I");
				if (fid) {
					jnumber = (jint)tss->cnotesLengthTotal;
					(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
				}

				//  Precision
				fid = (*env)->GetFieldID(env, cls, "precision", "I");
				if (fid) {
					jnumber = (jint)tss->precision;
					(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
				}
				//  Time zone
				fid = (*env)->GetFieldID(env, cls, "timeZoneID", "Ljava/lang/String;");
				if ((*env)->ExceptionOccurred(env)) {
					(*env)->ExceptionClear(env);
				}
				else {
					if (fid) {
						if (tss->timeZoneName) {
							len = (int)strlen(tss->timeZoneName);
							if (len > 2) {
								jstr = (*env)->NewStringUTF(env, tss->timeZoneName);
								(*env)->SetObjectField(env, j_recordInfo, fid, jstr);
								(*env)->DeleteLocalRef(env, jstr);
							}
						}
					}
				}
				//  Version  
				/*fid = (*env)->GetFieldID (env, cls, "version", "I");
				if (fid) {
				jnumber = (jint)timeSeriesRecordSizes->version;
				(*env)->SetIntField(env, j_recordInfo, fid, jnumber);
				}*/
				//  Last write time
				fid = (*env)->GetFieldID(env, cls, "lastWriteTimeValue", "J");
				if (fid) {
					//  Time is in mills (since 01Jan1970).  Make it seconds
					jlongNumber = (jlong)(tss->lastWrittenTime / 1000);
					(*env)->SetLongField(env, j_recordInfo, fid, jlongNumber);
				}
				hec_dss_jni_setStringField(env, cls, j_recordInfo, "programName", tss->programName);
				//  End of time series data
				zstructFree(tss);
				tss = 0;
			}
		}

		if (dataType >= DATA_TYPE_PD && dataType < DATA_TYPE_TEXT) {
			// get program name by reading the paired data.
			zStructPairedData* pd = zstructPdNew(cpath);
			if (!pd) {
				//  Error out!
				return -2;
			}
			status = zpdRetrieve((long long*)ifltab, pd, 0);
			if (status == 0) {
				hec_dss_jni_setStringField(env, cls, j_recordInfo, "programName", pd->programName);
			}
			zstructFree(pd);
			pd = 0;
		}

		

		/*
		tableHash = internals[0];
		hashCode = internals[1];
		hashAddress = internals[2];
		binLength = internals[3];
		binAddress = internals[4];
		infoLength = internals[5];
		infoAddress = internals[6];
		dataLength = internals[7];
		dataAddress = internals[8];
		allocatedSpace = internals[9];
		*/
	}

	if (tss) {
		zstructFree(tss);
		tss = 0;
	}
 
	
	//-------------------//
	// release variables //
	//-------------------//
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);
	(*env)->ReleaseStringUTFChars(env, j_startDate,  startDate);
	(*env)->ReleaseStringUTFChars(env, j_startTime,  startTime);
	(*env)->ReleaseStringUTFChars(env, j_endDate,  endDate);
	(*env)->ReleaseStringUTFChars(env, j_endTime,  endTime);

	

	return (jint)status;
}
