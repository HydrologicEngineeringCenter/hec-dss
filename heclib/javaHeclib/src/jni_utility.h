#pragma once
#include <jni.h>

/*

 *  Java Type       JNI Type         Signature     C Type (under the hood)
 *------------------------------------------------------------------------
 *  boolean         jboolean         Z             unsigned char
 *  byte            jbyte            B             signed char
 *  char            jchar            C             unsigned short
 *  short           jshort           S             signed short
 *  int             jint             I             signed int
 *  long            jlong            J             long long
 *  float           jfloat           F             float
 *  double          jdouble          D             double
 *  void            void             V             void
 *========================================================================
 *                          OBJECT & ARRAY TYPES
 *========================================================================
 *  Java Class                    JNI Ref Type        Signature
 *------------------------------------------------------------------------
 *  java.lang.Object              jobject             Ljava/lang/Object;
 *  java.lang.String              jstring             Ljava/lang/String;
 *  java.lang.Class               jclass              Ljava/lang/Class;
 *  java.lang.Throwable           jthrowable          Ljava/lang/Throwable;
 *
 *  boolean[]                     jbooleanArray       [Z
 *  byte[]                        jbyteArray          [B
 *  char[]                        jcharArray          [C
 *  short[]                       jshortArray         [S
 *  int[]                         jintArray           [I
 *  long[]                        jlongArray          [J
 *  float[]                       jfloatArray         [F
 *  double[]                      jdoubleArray        [D
 *  object[]                      jobjectArray        [Lhec/heclib/Type;
 *
 *
 *========================================================================
 *                        COMMON JNI HANDLE / ID TYPES
 *========================================================================
 *  jfieldID      // handle to a field (static or instance)
 *  jmethodID     // handle to a method (static or instance)
 *  jobject       // generic reference to any Java object
 *  jthrowable    // reference to Throwable or subclass
 *  jarray        // generic reference to any Java array
 */

 /*
  * ========================================================================
  *
  *
  *   find a class
  *    jclass cls = (*env)->FindClass(env, "hec/heclib/util/HecTime");
  *   get a field ID
  * 	jfieldID fid = (*env)->GetFieldID (env, cls, "internalHeader", "[I");
  *  jmethodID mid = (*env)->GetStaticMethodID(env, cls,
  *                            "methodName", "(Ljava/lang/String;)V");
  *
  *   // call an instance method
  *  (*env)->CallVoidMethod(env, obj, mid, someString);
  * ========================================================================
  */


/// <summary>
/// sets the value of a java string field
/// </summary>
/// <param name="env">JNI environment</param>
/// <param name="cls">Java class</param>
/// <param name="obj">Instance of Java Class</param>
/// <param name="name">fieldName</param>
/// <param name="value">value to set</param>
void hec_dss_jni_setStringField(JNIEnv* env, jclass cls, jobject obj, const char* name, const char* value);


/// <summary>
/// gets the value of a java boolean field
/// </summary>
/// <param name="env">JNI environment</param>
/// <param name="cls">Java class</param>
/// <param name="obj">Instance of Java Class</param>
/// <param name="name">fieldName</param>
/// <param name="default">default value if a error occures</param>
int hec_dss_jni_getBooleanFieldValue(JNIEnv* env, jclass cls, jobject obj, const char* name, int defaultValue);

////// hec_dss_jni_getBooleanFieldValue(env,cls,obj,"retrieveAllTimes",0);

void hec_dss_jni_setBooleanField(JNIEnv* env, jclass cls, jobject obj, const char* name, int value);