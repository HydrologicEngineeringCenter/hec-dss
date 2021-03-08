#include <jni.h>

#include "heclib.h"
#include "javaHeclib.h"


/*
*	Takes information from a Java DataContainer and puts into a location struct
*/

int Hec_zlocationToStruct(JNIEnv *env, jobject obj, jobject j_dataContainer, zStructLocation *locationStruct)
{
	jclass cls;
    jfieldID fid;

	jint j_int;
	double j_double;
	jstring		j_str;
	char*		c_str;


	cls = (*env)->GetObjectClass (env, j_dataContainer);

	 /* Longitude, Easting or decimal degrees (negative for Western Hemisphere) */ 
	fid = (*env)->GetFieldID (env, cls, "xOrdinate", "D");
	if (fid) {
		j_double = (double)(*env)->GetDoubleField(env, j_dataContainer, fid);
		locationStruct->xOrdinate = j_double;
	}

	/* Longitude, Easting or decimal degrees (negative for Western Hemisphere) */ 
	fid = (*env)->GetFieldID (env, cls, "yOrdinate", "D");
	if (fid) {
		j_double = (double)(*env)->GetDoubleField(env, j_dataContainer, fid);
		locationStruct->yOrdinate = j_double;
	}

	
	/* Elevation */
	fid = (*env)->GetFieldID (env, cls, "zOrdinate", "D");
	if (fid) {
		j_double = (double)(*env)->GetDoubleField(env, j_dataContainer, fid);
		locationStruct->zOrdinate = j_double;
	}
	

	/**
	 * coordinateSystem <p>
	 *  <li>0 - no coordinates set </li>
	 *  <li>1 - Lat/long</li>
	 *  <li>2 - State Plane, FIPS </li>
	 *  <li>3 - State Plane, ADS</li>
	 *  <li>4 - UTM</li>
	 *  <li>5 - Local (other)</li>
	 */
	fid = (*env)->GetFieldID (env, cls, "coordinateSystem", "I");
	if (fid) {
		j_int = (int)(*env)->GetIntField(env, j_dataContainer, fid);
		locationStruct->coordinateSystem = j_int;
	}

	/* coordinateID = UTM zone #, or FIPS SPCS # ADS SPCS #  */
	fid = (*env)->GetFieldID (env, cls, "coordinateID", "I");
	if (fid) {
		j_int = (int)(*env)->GetIntField(env, j_dataContainer, fid);
		locationStruct->coordinateID = j_int;
	}

	/**
	 * Horizontal Units can be one of the following:
	 * <li>0 - unspecified</li>
	 * <li>1 - feet</li>
	 * <li>2 - meeters</li>
	 * <li>3 - Decimal Degrees</li>
	 * <li>4 - Degrees Minutes Seconds </li>
	 */
	fid = (*env)->GetFieldID (env, cls, "horizontalUnits", "I");
	if (fid) {
		j_int = (int)(*env)->GetIntField(env, j_dataContainer, fid);
		locationStruct->horizontalUnits = j_int;
	}

	 /**
	 * horizontalDatum  can be one of the following:
	 * <li>0 - unset</li>
	 * <li>1 - NAD83</li>
	 * <li>2 - NAD27</li>
	 * <li>3 - WGS84</li>
	 * <li>4 - WGS72</li>
	 * <li>5 - Local (other)</li>
	 */
	fid = (*env)->GetFieldID (env, cls, "horizontalDatum", "I");
	if (fid) {
		j_int = (int)(*env)->GetIntField(env, j_dataContainer, fid);
		locationStruct->horizontalDatum = j_int;
	}

	 /**
	 * Vertical Units can be one of the following:
	 * <li>0 - unspecified</li>
	 * <li>1 - feet</li>
	 * <li>2 - meters</li>
	 */
	fid = (*env)->GetFieldID (env, cls, "verticalUnits", "I");
	if (fid) {
		j_int = (int)(*env)->GetIntField(env, j_dataContainer, fid);
		locationStruct->verticalUnits = j_int;
	}

	 /**
     * verticalDatum  can be one of the following:
     * <ul>
     * <li>0 - unset</li>
     * <li>1 - NAVD88</li>
     * <li>2 - NGVD29</li>
     * <li>3 - Local (other)</li>
     */
	fid = (*env)->GetFieldID (env, cls, "verticalDatum", "I");
	if (fid) {
		j_int = (int)(*env)->GetIntField(env, j_dataContainer, fid);
		locationStruct->verticalDatum = j_int;
	}

	/**
     * Time zone name at the location
     * NOT the time zone of the data
     * (data may GMT and location PST)
     */
	fid = (*env)->GetFieldID (env, cls, "locationTimezone", "Ljava/lang/String;");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);
	}
	else { 
		if (fid) {
			j_str = (*env)->GetObjectField(env, j_dataContainer, fid); 
			if (j_str) {
				c_str = (char *)(*env)->GetStringUTFChars(env, j_str, 0);
				if (c_str) {
					locationStruct->timeZoneName = mallocAndCopy(c_str);
					locationStruct->allocated[zSTRUCT_timeZoneName] = 1;
					(*env)->ReleaseStringUTFChars(env, j_str, c_str);
				}
			}
		}
    }

	//int *supplemental;   
	//int numberSupplemental; 
	fid = (*env)->GetFieldID (env, cls, "supplementalInfo", "Ljava/lang/String;");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);
	}
	else { 
		if (fid) {
			j_str = (*env)->GetObjectField(env, j_dataContainer, fid); 
			if (j_str) {
				c_str = (char *)(*env)->GetStringUTFChars(env, j_str,  0);
				if (c_str) {
					locationStruct->supplemental = (char *)mallocAndCopy(c_str);
					(*env)->ReleaseStringUTFChars (env, j_str, c_str);
				}
			}
		}
    }
 
	return STATUS_OKAY;
}