#include <jni.h>
#include <string.h>
#include <stdbool.h>

#include "heclib.h"
#include "javaHeclib.h"

#ifdef _MSC_VER 
    #define strtok_r strtok_s
#endif

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


	fid = (*env)->GetFieldID (env, cls, "supplementalInfo", "Ljava/lang/String;");
	if ((*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);
	}
	else { 
		if (fid) {
			//----------------------------//
			// set up the data structures //
			//----------------------------//
			typedef struct si_elem_s {char *key; char *value;} si_elem;
			si_elem *existing_si = NULL;
			si_elem *location_si = NULL;
			int existing_elem_count = 0;
			int location_elem_count = 0;
			char *saveptr;
			char *buf;
			//--------------------------------------//
			// parse the existing supplemental info //
			//--------------------------------------//
			jstr = (*env)->GetObjectField(env, j_dataContainer, fid);
			char *cp;
			if (jstr != NULL && (*env)->GetStringLength(env, jstr) > 0) {
				const char *existing = (*env)->GetStringUTFChars(env, jstr, 0);
				buf = strdup((char *)existing);
				cp = strtok_r((char *)buf, ";", &saveptr);
				while (cp) {
					int i = existing_elem_count++;
					existing_si = (si_elem*)realloc(existing_si, existing_elem_count * sizeof(si_elem));
					existing_si[i].key = strdup(cp);
					cp = strchr(existing_si[i].key, ':');
					if (cp) {
						*cp = '\0';
						existing_si[i].value = ++cp;
					}
					else {
						existing_si[i].value = NULL;
					}
					cp = strtok_r(NULL, ";", &saveptr);
				}
				free(buf);
				(*env)->ReleaseStringUTFChars(env, jstr, existing);
			}
			if (locationStruct->supplemental) {
				//--------------------------------------//
				// parse the location supplemental info //
				//--------------------------------------//
				buf = strdup(locationStruct->supplemental);
				cp = strtok_r((char *)buf, "\n", &saveptr);
				while (cp) {
					int i = location_elem_count++;
					location_si = (si_elem*)realloc(location_si, location_elem_count * sizeof(si_elem));
					location_si[i].key = strdup(cp);
					cp = strchr(location_si[i].key, ':');
					if (cp) {
						*cp = '\0';
						location_si[i].value = ++cp;
					}
					else {
						location_si[i].value = NULL;
					}
					cp = strtok_r(NULL, "\n", &saveptr);
				}
				free(buf);
				if (location_elem_count > 0) {
					//---------------------------------------------------------//
					// combine the two, giving preference to the existing info //
					//---------------------------------------------------------//
					char *combined = NULL;
					int oldLen = 0;
					int newLen;
					bool inExisting;
					for (int i = 0; i < existing_elem_count; ++i) {
						newLen = oldLen + strlen(existing_si[i].key) + 1;
						if (existing_si[i].value) {
							newLen += strlen(existing_si[i].value) + 1;
						}
						combined = (char*)realloc(combined, newLen);
						strcpy(combined+oldLen, existing_si[i].key);
						if (existing_si[i].value) {
							strcat(combined, ":");
							strcat(combined, existing_si[i].value);
					}
						strcat(combined, ";");
						oldLen = newLen;
					}
					for (int i = 0; i < location_elem_count; ++i) {
						inExisting = false;
						for (int j = 0; j < existing_elem_count; ++j) {
							if (!strcmp(location_si[i].key, existing_si[j].key)) {
								inExisting = true;
								break;
							}
						}
						if (!inExisting) {
							newLen = oldLen + strlen(location_si[i].key) + 1;
							if (location_si[i].value) {
								newLen += strlen(location_si[i].value) + 1;
							}
							combined = (char*)realloc(combined, newLen);
							strcpy(combined+oldLen, location_si[i].key);
							if (location_si[i].value) {
								strcat(combined, ":");
								strcat(combined, location_si[i].value);
							}
							strcat(combined, ";");
							oldLen = newLen;
						}
					}
					jstr = (*env)->NewStringUTF(env, (const char*)combined);			
					(*env)->SetObjectField (env, j_dataContainer, fid, jstr);
					//----------//
					// clean up //
					//----------//
					for (int i = 0; i < location_elem_count; ++i) {
						if (location_si[i].key) {
							free(location_si[i].key); // value is in memory malloc'd for key
						}
					}
					free(location_si);
					free(combined);
				}
			}
			for (int i = 0; i < existing_elem_count; ++i) {
				if (existing_si[i].key) {
					free(existing_si[i].key); // value is in memory malloc'd for key
				}
			}
			free(existing_si);
		}
	}

	return 0;
}
