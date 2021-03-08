#include <jni.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"
#include "javaHeclib.h"

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1ztinStore(
	JNIEnv       *env, 
	jobject       obj, 
	jintArray    j_ifltab,		                                              
	jobject		 j_tinContainer)
	                               
{
	jclass cls;
    jfieldID fid;
    jstring j_cpath;	
	jstring jstr;	
	jdoubleArray	jdoubArray;
	double			*doubArray;
	jintArray	jintAry;
	int			*intAry;
	jobjectArray stringArray;
	jdoubleArray connectionColumns;
	int *connectionRow;
	int arraySize;

	const char *cpath;
	int i;
	int j;
	int number;
	int status;
	int count;
	int len;
	int numbercoordinates;
	int numberPoints;	
	double doubleNumber;
	int *ifltab;
	const char *cstr;

	zStructSpatialTin *tinStruct;

	
	jint capacity=47;
	(*env)->EnsureLocalCapacity(env, capacity);
	status = 0;
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);	

	//  Get the pathname
	cls = (*env)->GetObjectClass (env, j_tinContainer);
    fid = (*env)->GetFieldID (env, cls, "fullName", "Ljava/lang/String;");
    if (fid != 0) {
		j_cpath = (*env)->GetObjectField(env, j_tinContainer, fid); 
        cpath = (*env)->GetStringUTFChars(env, j_cpath,  0);
		if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
			zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Enter Hec_ztinStore; Pathname: ", cpath);
		}
    }
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_PATHNAME,
			0, 0, zdssErrorSeverity.WARNING, "", "ztinStore");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	tinStruct = zstructSpatialTinNew(cpath); 
	(*env)->ReleaseStringUTFChars(env, j_cpath,  cpath);
	(*env)->DeleteLocalRef(env, j_cpath);

	if (!tinStruct) {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
			0, 0, zdssErrorSeverity.MEMORY_ERROR, tinStruct->pathname, "ztinStore, tinStruct");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}


	// Geospatial metadata
	fid = (*env)->GetFieldID (env, cls, "SpatialReferenceSystem", "Ljava/lang/String;");
    if (fid) {
		jstr = (*env)->GetObjectField(env, j_tinContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tinStruct->SpatialReferenceSystem = mallocAndCopy(cstr);
				tinStruct->allocated[zSTRUCT_TIN_SpatialReferenceSystem] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, SpatialReferenceSystem");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "SRSType", "I");
	if (fid) {
		number = (int)(*env)->GetIntField(env, j_tinContainer, fid);
		tinStruct->SRSType = number;
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, SRSType");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "SRSName", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_tinContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tinStruct->SRSName = mallocAndCopy(cstr);
				tinStruct->allocated[zSTRUCT_TIN_SRSName] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }

	fid = (*env)->GetFieldID (env, cls, "SRSUnits", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_tinContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tinStruct->SRSUnits = mallocAndCopy(cstr);
				tinStruct->allocated[zSTRUCT_TIN_SRSUnits] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }


	//  metadata for data values
	fid = (*env)->GetFieldID (env, cls, "units", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_tinContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tinStruct->units = mallocAndCopy(cstr);
				tinStruct->allocated[zSTRUCT_TIN_units] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }
	fid = (*env)->GetFieldID (env, cls, "type", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_tinContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tinStruct->type = mallocAndCopy(cstr);
				tinStruct->allocated[zSTRUCT_TIN_type] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }
	fid = (*env)->GetFieldID (env, cls, "timeZoneName", "Ljava/lang/String;");
    if (fid != 0) {
		jstr = (*env)->GetObjectField(env, j_tinContainer, fid); 
		if (jstr) {
			cstr = (*env)->GetStringUTFChars(env, jstr,  0);
			if (cstr) {
				tinStruct->timeZoneName = mallocAndCopy(cstr);
				tinStruct->allocated[zSTRUCT_TIN_timeZoneName] = 1;
			}
			(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
		}
    }
	fid = (*env)->GetFieldID(env, cls, "minXCoordinate", "D");
	if (fid) {
		doubleNumber = (double)(*env)->GetDoubleField(env, j_tinContainer, fid);
		tinStruct->minXCoordinate = (float)doubleNumber;
	}
	fid = (*env)->GetFieldID(env, cls, "minYCoordinate", "D");
	if (fid) {
		doubleNumber = (double)(*env)->GetDoubleField(env, j_tinContainer, fid);
		tinStruct->minYCoordinate = (float)doubleNumber;
	}
	fid = (*env)->GetFieldID(env, cls, "maxXCoordinate", "D");
	if (fid) {
		doubleNumber = (double)(*env)->GetDoubleField(env, j_tinContainer, fid);
		tinStruct->maxXCoordinate = (float)doubleNumber;
	}
	fid = (*env)->GetFieldID(env, cls, "maxYCoordinate", "D");
	if (fid) {
		doubleNumber = (double)(*env)->GetDoubleField(env, j_tinContainer, fid);
		tinStruct->maxYCoordinate = (float)doubleNumber;
	}
	fid = (*env)->GetFieldID(env, cls, "minValue", "D");
	if (fid) {
		doubleNumber = (double)(*env)->GetDoubleField(env, j_tinContainer, fid);
		tinStruct->minValue = (float)doubleNumber;
	}
	fid = (*env)->GetFieldID(env, cls, "meanValue", "D");
	if (fid) {
		doubleNumber = (double)(*env)->GetDoubleField(env, j_tinContainer, fid);
		tinStruct->meanValue = (float)doubleNumber;
	}
	fid = (*env)->GetFieldID(env, cls, "maxValue", "D");
	if (fid) {
		doubleNumber = (double)(*env)->GetDoubleField(env, j_tinContainer, fid);
		tinStruct->maxValue = (float)doubleNumber;
	}


	// TIN is made up of points and connections
	fid = (*env)->GetFieldID (env, cls, "numberPoints", "I");
	if (fid) {
		number = (int)(*env)->GetIntField(env, j_tinContainer, fid);
		tinStruct->numberPoints = number;
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, numberPoints");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}
	fid = (*env)->GetFieldID (env, cls, "slendernessRatio", "D");
	if (fid) {
		doubleNumber = (int)(*env)->GetDoubleField(env, j_tinContainer, fid);
		tinStruct->slendernessRatio = (float)doubleNumber;
	}


	// These arrays have one value per point
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "numberPoints: ", tinStruct->numberPoints);
	}

	fid = (*env)->GetFieldID (env, cls, "xCoordinate", "[D");
	if (fid) {
		jdoubArray = (*env)->GetObjectField (env, j_tinContainer, fid);
		if (jdoubArray) {
			doubArray = (*env)->GetDoubleArrayElements(env, jdoubArray, 0);
			if (doubArray) {
				numbercoordinates = (int)(*env)->GetArrayLength(env, jdoubArray);
				if (numbercoordinates < 1) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
						numbercoordinates, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, no X doubArray");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					(*env)->ReleaseDoubleArrayElements (env, jdoubArray, doubArray, 0);
					return status;
				}
				tinStruct->xCoordinate = (float *)calloc(numbercoordinates, 4);
				if (!tinStruct->xCoordinate) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
						numbercoordinates, 0, zdssErrorSeverity.MEMORY_ERROR, tinStruct->pathname, "ztinStore, xCoordinate");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					return status;
				}
				tinStruct->allocated[zSTRUCT_TIN_xCoordinate] = 1;	
				convertDataArray((void *)doubArray, (void *)tinStruct->xCoordinate, numbercoordinates, 2, 1);
				(*env)->ReleaseDoubleArrayElements(env, jdoubArray, doubArray, 0);
				(*env)->DeleteLocalRef(env, jdoubArray);				
			}
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, no xCoordinates");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "yCoordinate", "[D");
	if (fid) {
		jdoubArray = (*env)->GetObjectField (env, j_tinContainer, fid);
		if (jdoubArray) {
			doubArray = (*env)->GetDoubleArrayElements(env, jdoubArray, 0);
			if (doubArray) {
				numbercoordinates = (int)(*env)->GetArrayLength(env, jdoubArray);
				if (numbercoordinates < 1) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
						numbercoordinates, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, no Y doubArray");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					(*env)->ReleaseDoubleArrayElements (env, jdoubArray, doubArray, 0);
					return status;
				}
				tinStruct->yCoordinate = (float *)calloc(numbercoordinates, 4);
				if (!tinStruct->yCoordinate) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
						numbercoordinates, 0, zdssErrorSeverity.MEMORY_ERROR, tinStruct->pathname, "ztinStore, yCoordinate");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					return status;
				}
				tinStruct->allocated[zSTRUCT_TIN_yCoordinate] = 1;	
				convertDataArray((void *)doubArray, (void *)tinStruct->yCoordinate, numbercoordinates, 2, 1);
				(*env)->ReleaseDoubleArrayElements(env, jdoubArray, doubArray, 0);
				(*env)->DeleteLocalRef(env, jdoubArray);				
			}
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, no yCoordinates");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "value", "[D");
	if (fid) {
		jdoubArray = (*env)->GetObjectField (env, j_tinContainer, fid);
		if (jdoubArray) {
			doubArray = (*env)->GetDoubleArrayElements(env, jdoubArray, 0);
			if (doubArray) {
				numbercoordinates = (int)(*env)->GetArrayLength(env, jdoubArray);
				if (numbercoordinates < 1) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
						numbercoordinates, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, no value array");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					(*env)->ReleaseDoubleArrayElements (env, jdoubArray, doubArray, 0);
					return status;
				}
				tinStruct->value = (float *)calloc(numbercoordinates, 4);
				if (!tinStruct->value) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
						numbercoordinates, 0, zdssErrorSeverity.MEMORY_ERROR, tinStruct->pathname, "ztinStore,  value");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					return status;
				}
				tinStruct->allocated[zSTRUCT_TIN_value] = 1;	
				convertDataArray((void *)doubArray, (void *)tinStruct->value, numbercoordinates, 2, 1);
				(*env)->ReleaseDoubleArrayElements(env, jdoubArray, doubArray, 0);
				(*env)->DeleteLocalRef(env, jdoubArray);				
			}
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, no yCoordinates");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	fid = (*env)->GetFieldID (env, cls, "pointType", "[I");
	if (fid) {
		jintAry = (*env)->GetObjectField (env, j_tinContainer, fid);
		if (jintAry) {
			intAry = (*env)->GetIntArrayElements(env, jintAry, 0);
			if (intAry) {
				numbercoordinates = (int)(*env)->GetArrayLength(env, jintAry);
				if (numbercoordinates < 1) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
						numbercoordinates, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, no pointType array");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					(*env)->ReleaseIntArrayElements (env, jintAry, intAry, 0);
					return status;
				}
				tinStruct->pointType = (int *)calloc(numbercoordinates, 4);
				if (!tinStruct->pointType) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
						numbercoordinates, 0, zdssErrorSeverity.MEMORY_ERROR, tinStruct->pathname, "ztinStore, pointType");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					return status;
				}
				tinStruct->allocated[zSTRUCT_TIN_pointType] = 1;	
				convertDataArray(intAry, tinStruct->pointType, numbercoordinates, 1, 1);
				(*env)->ReleaseIntArrayElements(env, jintAry, intAry, 0);
				(*env)->DeleteLocalRef(env, jintAry);				
			}
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, no pointType");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	/*
	fid = (*env)->GetFieldID (env, cls, "numberConnections", "[I");
	if (fid) {
		jintAry = (*env)->GetObjectField (env, j_tinContainer, fid);
		if (jintAry) {
			intAry = (*env)->GetIntArrayElements(env, jintAry, 0);
			if (intAry) {
				numbercoordinates = (int)(*env)->GetArrayLength(env, jintAry);
				if (numbercoordinates < 1) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
						numbercoordinates, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, no numberConnections array");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					(*env)->ReleaseIntArrayElements (env, jintAry, intAry, 0);
					return status;
				}
				tinStruct->numberConnections = (int *)calloc(numbercoordinates, 4);
				if (!tinStruct->numberConnections) {
					status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
						numbercoordinates, 0, zdssErrorSeverity.MEMORY_ERROR, tinStruct->pathname, "ztinStore, numberConnections");
					(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
					return status;
				}
				tinStruct->allocated[zSTRUCT_TIN_numberConnections] = 1;	
				convertDataArray(intAry, tinStruct->numberConnections, numbercoordinates, 1, 1);
				(*env)->ReleaseIntArrayElements(env, jintAry, intAry, 0);
				(*env)->DeleteLocalRef(env, jintAry);				
			}
		}
	}
	else {
		status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.INVALID_NUMBER,
			0, 0, zdssErrorSeverity.WARNING, tinStruct->pathname, "ztinStore, no numberConnections");
		(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
		return status;
	}

	*/

	//  connection
	//  connection is an array of arrays.  It is not really doubley dimensioned,
	//  as each sub array will be a different length
	fid = (*env)->GetFieldID (env, cls, "connection", "[[I");
	if (fid) {
		jdoubArray = (*env)->GetObjectField (env, j_tinContainer, fid);
		if (jdoubArray) {
			//  Allocate space for the number Connections array.  
			//  This contains the length of each points connections array
			tinStruct->numberConnections = (int *)calloc(tinStruct->numberPoints, 4);
			if (!tinStruct->numberConnections) {
				status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
					numbercoordinates, 0, zdssErrorSeverity.MEMORY_ERROR, tinStruct->pathname, "ztinStore, numberConnections");
				(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
				return status;
			}
			tinStruct->allocated[zSTRUCT_TIN_numberConnections] = 1;	
			
			numberPoints = (int)(*env)->GetArrayLength(env, jdoubArray);  //  Should be the same as in the struct
			/*
			if (numberPoints != tinStruct->numberPoints) {
				error out
			}
			*/
			// Count the total number of cells.
			count = 0;
			for (i=0; i<numberPoints; i++) {
				connectionColumns = (*env)->GetObjectArrayElement(env, jdoubArray, i);
				number = (int)(*env)->GetArrayLength(env, connectionColumns);
				tinStruct->numberConnections[i] = number;
				count += number;
			}
			tinStruct->connectTableLen = count;
			tinStruct->connectTo = (int *)calloc(count, 4);
			if (!tinStruct->connectTo) {
				status = zerrorProcessing((long long*)ifltab, MESS_METHOD_JNI_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
						numbercoordinates, 0, zdssErrorSeverity.MEMORY_ERROR, tinStruct->pathname, "ztinStore, connection array");
				(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
				return status;
			}
			tinStruct->allocated[zSTRUCT_TIN_connection] = 1;

			count = 0;
			for (i=0; i<numberPoints; i++) {
				connectionColumns = (*env)->GetObjectArrayElement(env, jdoubArray, i);
				number = (int)(*env)->GetArrayLength(env, connectionColumns);
				connectionRow = (int *)(*env)->GetIntArrayElements(env, connectionColumns, 0);
				for (j=0; j<number; j++) {
					tinStruct->connectTo[count++] = connectionRow[j];
				}
				(*env)->ReleaseIntArrayElements(env, connectionColumns, connectionRow, 0);
				(*env)->DeleteLocalRef(env, connectionColumns);
			}
			(*env)->DeleteLocalRef(env, jdoubArray);
		}
	}


	//  labels
	fid = (*env)->GetFieldID (env, cls, "label", "[Ljava/lang/String;");
	if (fid) {
		stringArray = (*env)->GetObjectField (env, j_tinContainer, fid);
		if (stringArray) {
			arraySize = (int)(*env)->GetArrayLength(env, stringArray);
			if (arraySize > 0) {
				//  Count the number of characters first
				count = 0;
				for (i=0; i<arraySize; i++) {
					jstr = (jstring) (*env)->GetObjectArrayElement(env, stringArray, i);
					if (!jstr) continue;
					cstr = (*env)->GetStringUTFChars(env, jstr, 0);
					if (cstr) {
						count += (int)strlen(cstr);
						count++;  //  For "\0" end of each string
					}
					else {
						count++; 
						continue;
					}
					(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
				}
				if (count > 0) {
					tinStruct->pointLabelLen = count;
					//  Now allocate the space and copy the ordinates
					tinStruct->pointLabel = (char *)calloc(count, 1);
					tinStruct->allocated[zSTRUCT_TIN_label] = 1;
					count = 0;
					for (i=0; i<arraySize; i++) {
						jstr = (jstring) (*env)->GetObjectArrayElement(env, stringArray, i);
						cstr = (*env)->GetStringUTFChars(env, jstr, 0);
						if (cstr) {
							len = (int)strlen(cstr);
							for (j=0; j<len; j++) {
								tinStruct->pointLabel[count++] = cstr[j];
							}
							tinStruct->pointLabel[count++] = '\0';
						}
						(*env)->ReleaseStringUTFChars(env, jstr,  cstr);
						(*env)->DeleteLocalRef(env, jstr);
					}
					if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
						zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Labels to store.  Number characters: ", tinStruct->pointLabelLen);
					}					
				}
			}
		}
	}

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebug((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "Before tin data store ", "");
	}

	status = 0;
	status = zspatialTinStore((long long *)ifltab, tinStruct);

	if (zmessageLevel((long long*)ifltab, MESS_METHOD_JNI_ID, MESS_LEVEL_USER_DIAG)) {		
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, "After tin data store.  Status: ", status);
	}

	zstructFree(tinStruct);

	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);

	return (jint)status;
}
