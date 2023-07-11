#include <jni.h>
#include <stdio.h>
#include <time.h>
#ifdef _MSC_VER
#  define tzset _tzset
#  define timezone _timezone
#endif
#include "heclib.h"

#define RELEASE() {                                                                     \
	if (ifltab != NULL) (*env)->ReleaseIntArrayElements (env, j_ifltab, ifltab, 0); \
}

#define THROW_NEW(classname, msg) {                                   \
	RELEASE();                                                    \
	(*env)->ThrowNew(env,(*env)->FindClass(env, classname), msg); \
	return -1;                                                    \
}

#define CHECK_EXCEPTION() {                                      \
	if(exc = (*env)->ExceptionOccurred(env)) {               \
		handle_exception(env);                           \
		THROW_NEW("java/lang/Exception", "JNI Error");   \
	}                                                        \
}

#define WHERE_AM_I() {                                          \
	printf("At line %d in file %s.\n", __LINE__, __FILE__); \
	fflush(stdout);                                         \
}

void handle_exception(JNIEnv *env) {

	(*env)->ExceptionDescribe(env);
	(*env)->ExceptionClear(env);
}

JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zndata(
	JNIEnv    *env,
	jclass     obj,
	jintArray  j_ifltab,          // DSS file table
	jlong      startTime,         // start time in millis
	jobject    pathnamesVec,      // pathnames updated since start time
	jobject    updateTimesVec,    // update times in millis
	jobject    recordTypesVec) {  // record types

	int *ifltab = NULL;
	int juls, isecs;
	int nullCount = 0;
	int ifpos[] = { 0 }, npath[] = { 0 }, juld[] = { 0 }, isecd[] = { 0 }, idtype[] = { 0 }, istat[] = { 0 };
	char cpath[392];
	time_t seconds;
	struct tm *ptm;
	jstring pathname;
	jlong updateTime;
	jint  recordType;

	jthrowable exc;
	jclass vectorClass;
	jmethodID classEquals;
	jmethodID clearVector;
	jmethodID appendVector;
	jclass longClass;
	jmethodID longConstructor;
	jclass intClass;
	jmethodID intConstructor;
	jclass constClass;
	jlong constUndefinedTime;

	zStructCatalog *catStruct;
	zStructCatalog *catStructDiff;
	int i;
	int number;
	int status;

	/*------------------------------------------*/
	/* get the classes and methods we will need */
	/*------------------------------------------*/
	vectorClass = (*env)->FindClass(env, "java/util/Vector");
	CHECK_EXCEPTION();
	classEquals = (*env)->GetMethodID(env, vectorClass, "equals", "(Ljava/lang/Object;)Z");
	CHECK_EXCEPTION();
	clearVector = (*env)->GetMethodID(env, vectorClass, "clear", "()V");
	CHECK_EXCEPTION();
	appendVector = (*env)->GetMethodID(env, vectorClass, "add", "(Ljava/lang/Object;)Z");
	CHECK_EXCEPTION();

	intClass = (*env)->FindClass(env, "java/lang/Integer");
	CHECK_EXCEPTION();
	intConstructor = (*env)->GetMethodID(env, intClass, "<init>", "(I)V");
	CHECK_EXCEPTION();

	longClass = (*env)->FindClass(env, "java/lang/Long");
	CHECK_EXCEPTION();
	longConstructor = (*env)->GetMethodID(env, longClass, "<init>", "(J)V");
	CHECK_EXCEPTION();

	constClass = (*env)->FindClass(env, "hec/lang/Const");
	CHECK_EXCEPTION();
	constUndefinedTime = (*env)->GetStaticLongField(env, constClass, (*env)->GetStaticFieldID(env, constClass, "UNDEFINED_TIME", "J"));
	CHECK_EXCEPTION();

	/*-----------------------*/
	/* verify the parameters */
	/*-----------------------*/
	ifltab = (int*)(*env)->GetIntArrayElements(env, j_ifltab, 0);
	CHECK_EXCEPTION();
	if (ifltab == NULL) {
		THROW_NEW("java/lang/NullPointerException", "DSS file table cannot be null.");
	}
	if (pathnamesVec != NULL) {
		if (!((*env)->CallBooleanMethod(env, vectorClass, classEquals, (*env)->GetObjectClass(env, pathnamesVec)))) {
			THROW_NEW("java/lang/IllegalArgumentException", "Pathnames vector is wrong type.");
		}
		(*env)->CallBooleanMethod(env, pathnamesVec, clearVector);
		CHECK_EXCEPTION();
	}
	else ++nullCount;
	if (updateTimesVec != NULL) {
		if (!((*env)->CallBooleanMethod(env, vectorClass, classEquals, (*env)->GetObjectClass(env, updateTimesVec)))) {
			THROW_NEW("java/lang/IllegalArgumentException", "Update times vector is wrong type.");
		}
		(*env)->CallBooleanMethod(env, updateTimesVec, clearVector);
		CHECK_EXCEPTION();
	}
	else ++nullCount;
	if (recordTypesVec != NULL) {
		if (!((*env)->CallBooleanMethod(env, vectorClass, classEquals, (*env)->GetObjectClass(env, recordTypesVec)))) {
			THROW_NEW("java/lang/IllegalArgumentException", "Record types vector is wrong type.");
		}
		(*env)->CallBooleanMethod(env, recordTypesVec, clearVector);
		CHECK_EXCEPTION();
	}
	else ++nullCount;

	/*--------------------------------------------*/
	/* short circuit if no results were requested */
	/*--------------------------------------------*/
	if (nullCount == 3) {
		CHECK_EXCEPTION();
		RELEASE();
		return (jint)0;
	}

	/*----------------------------------------------------------------------*/
	/* parse the startTime for the library call, changing from UTC to local */
	/*----------------------------------------------------------------------*/
	tzset();
	if (startTime == constUndefinedTime) {
		juls = isecs = 1;
	}
	else {
		seconds = (time_t)(startTime / 1000L);
		ptm = localtime(&seconds);
		seconds -= (ptm->tm_isdst ? timezone - SECS_IN_1_HOUR : timezone);
		juls = (int)(seconds / SECS_IN_1_DAY) + JUL_01JAN1970;
		isecs = seconds % SECS_IN_1_DAY;
		if (isecs == 0) {
			--juls;
			isecs = SECS_IN_1_DAY;
		}
	}

	if (zgetVersion((long long *)ifltab) == 6) {
		/*--------------------------------------------------*/
		/* call the library routine and collect the results */
		/*--------------------------------------------------*/
		CHECK_EXCEPTION();
		zndata6_((long long *)ifltab, ifpos, &juls, &isecs, cpath, npath, juld, isecd, idtype, istat, sizeof(cpath));
		while (istat[0] == 0) {
			CHECK_EXCEPTION();

			if (pathnamesVec != NULL) {
				cpath[npath[0]] = '\0';
				pathname = (*env)->NewStringUTF(env, cpath);
				CHECK_EXCEPTION();
				(*env)->CallBooleanMethod(env, pathnamesVec, appendVector, pathname);
				CHECK_EXCEPTION();
			}
			if (updateTimesVec != NULL) {
				seconds = (time_t)((jlong)(juld[0] - JUL_01JAN1970) * SECS_IN_1_DAY + isecd[0]);
				ptm = localtime(&seconds);
				seconds += (ptm->tm_isdst ? timezone - SECS_IN_1_HOUR : timezone);
				updateTime = (jlong)seconds * (jlong)1000;
				(*env)->CallBooleanMethod(env, updateTimesVec, appendVector, (*env)->NewObject(env, longClass, longConstructor, updateTime));
				CHECK_EXCEPTION();
			}
			if (recordTypesVec != NULL) {
				recordType = (jint)idtype[0];
				(*env)->CallBooleanMethod(env, recordTypesVec, appendVector, (*env)->NewObject(env, intClass, intConstructor, recordType));
				CHECK_EXCEPTION();
			}

			zndata6_((long long *)ifltab, ifpos, &juls, &isecs, cpath, npath, juld, isecd, idtype, istat, sizeof(cpath));
		}
	}
	else {
		catStruct = zstructCatalogNew();
		status = zwhatChangedSetStart((long long *)ifltab, catStruct, (const char *)0, 0);
		catStruct->lastWriteTimeFile = startTime;
		catStructDiff = zstructCatalogNew();
		number = zwhatChangedCompare((long long *)ifltab, catStruct, catStructDiff, (const char *)0, 0);

		for (i = 0; i < number; i++) {
			CHECK_EXCEPTION();

			if (pathnamesVec != NULL) {
				pathname = (*env)->NewStringUTF(env, catStructDiff->pathnameList[i]);
				CHECK_EXCEPTION();
				(*env)->CallBooleanMethod(env, pathnamesVec, appendVector, pathname);
				CHECK_EXCEPTION();
			}
			if (updateTimesVec != NULL) {
				updateTime = catStructDiff->lastWriteTimeRecord[i];
				(*env)->CallBooleanMethod(env, updateTimesVec, appendVector, (*env)->NewObject(env, longClass, longConstructor, updateTime));
				CHECK_EXCEPTION();
			}
			if (recordTypesVec != NULL) {
				recordType = (jint)catStructDiff->recordType[i];
				(*env)->CallBooleanMethod(env, recordTypesVec, appendVector, (*env)->NewObject(env, intClass, intConstructor, recordType));
				CHECK_EXCEPTION();
			}
		}
	}

	/*----------------------------*/
	/* return zero or status code */
	/*----------------------------*/
	CHECK_EXCEPTION();
	RELEASE();
	return (jint)(istat[0] == 1 ? 0 : istat[0]);

}

