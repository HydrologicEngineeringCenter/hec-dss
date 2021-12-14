#include <jni.h>
#include <string.h>

#include "heclib.h"
#include "javaHeclib.h"


/*
*	Takes information from a location struct and puts in a Java DataContainer
*/

int Hec_zlocationFromStruct(JNIEnv *env, jobject obj, jobject j_dataContainer, zStructLocation *locationStruct)
{
	jclass cls;
    jfieldID fid;
	jstring jstr;
	jint j_int;
	double j_double;


	cls = (*env)->GetObjectClass (env, j_dataContainer);

	 /* Longitude, Easting or decimal degrees (negative for Western Hemisphere) */ 
	fid = (*env)->GetFieldID (env, cls, "xOrdinate", "D");
	if (fid) {
		j_double = (jdouble)locationStruct->xOrdinate;
		(*env)->SetDoubleField(env, j_dataContainer, fid, j_double);
	}
	
	 /* Longitude, Easting or decimal degrees (negative for Western Hemisphere) */ 
	fid = (*env)->GetFieldID (env, cls, "yOrdinate", "D");
	if (fid) {
		j_double = (jdouble)locationStruct->yOrdinate;
		(*env)->SetDoubleField(env, j_dataContainer, fid, j_double);
	}
	
	  /* Elevation */
	fid = (*env)->GetFieldID (env, cls, "zOrdinate", "D");
	if (fid) {
		j_double = (jdouble)locationStruct->zOrdinate;
		(*env)->SetDoubleField(env, j_dataContainer, fid, j_double);
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
		j_int = (jint)locationStruct->coordinateSystem;
		(*env)->SetIntField(env, j_dataContainer, fid, j_int);
	}

	/* coordinateID = UTM zone #, or FIPS SPCS # ADS SPCS #  */
	fid = (*env)->GetFieldID (env, cls, "coordinateID", "I");
	if (fid) {
		j_int = (jint)locationStruct->coordinateID;
		(*env)->SetIntField(env, j_dataContainer, fid, j_int);
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
		j_int = (jint)locationStruct->horizontalUnits;
		(*env)->SetIntField(env, j_dataContainer, fid, j_int);
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
		j_int = (jint)locationStruct->horizontalDatum;
		(*env)->SetIntField(env, j_dataContainer, fid, j_int);
	}

	 /**
	 * Vertical Units can be one of the following:
	 * <li>0 - unspecified</li>
	 * <li>1 - feet</li>
	 * <li>2 - meters</li>
	 */
	fid = (*env)->GetFieldID (env, cls, "verticalUnits", "I");
	if (fid) {
		j_int = (jint)locationStruct->verticalUnits;
		(*env)->SetIntField(env, j_dataContainer, fid, j_int);
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
		j_int = (jint)locationStruct->verticalDatum;
		(*env)->SetIntField(env, j_dataContainer, fid, j_int);
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
		j_int = (jint)locationStruct->verticalDatum;
		(*env)->SetIntField(env, j_dataContainer, fid, j_int);
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
			if (locationStruct->timeZoneName) {
				jstr = (*env)->NewStringUTF(env, (const char*)locationStruct->timeZoneName);			
				(*env)->SetObjectField (env, j_dataContainer, fid, jstr);
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
			if ((*env)->GetObjectField(env, j_dataContainer, fid) == NULL) {
				// don't overwrite existing supplemental info
				if (locationStruct->supplemental) {
					FILE *fp = fopen("test.log", "a");
					fprintf(fp, "\nIn Hec_zlocationFromStruct()\n");
					fprintf(fp, "\tlocationStruct->supplemental = >%s<\n", locationStruct->supplemental);
					fclose(fp);
					// convert locationStruct->supplemental delimiters ('\n') to user header delimiters (';')
					char *supplemental = (char *)malloc(strlen(locationStruct->supplemental)+1);
					strcpy(supplemental, locationStruct->supplemental);
					for (char *cp = supplemental; *cp; ++cp) {
						if (*cp == '\n') *cp = ';';
					}
					jstr = (*env)->NewStringUTF(env, (const char*)supplemental);			
					(*env)->SetObjectField (env, j_dataContainer, fid, jstr);
					free(supplemental);
				}
			}
		}
	}

	return 0;
}
