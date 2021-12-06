#include <string.h>
#include <jni.h>
#include "heclib.h"


JNIEXPORT jint JNICALL Java_hec_heclib_util_Heclib_Hec_1zgpnp
    (JNIEnv *env, jobject obj, jstring j_inputString, jobject j_aPart,
     jobject j_bPart, jobject j_cPart, jobject j_dPart, jobject j_ePart,
     jobject j_fPart)
{
    const char *inputString;

    char aPart[MAX_PART_SIZE];
    char bPart[MAX_PART_SIZE];
    char cPart[MAX_PART_SIZE];
    char dPart[MAX_PART_SIZE];
    char ePart[MAX_PART_SIZE];
    char fPart[MAX_F_PART_SIZE];
    int nstats[6];
    int i;

    jclass cls;
    jfieldID fid;
    jstring jstr;

    for (i=0; i<6; i++)
        nstats[i] = 0;

    inputString = (*env)->GetStringUTFChars (env, j_inputString, 0);

    zgpnp_ (inputString, aPart, bPart, cPart, dPart, ePart, fPart,
            nstats, strlen(inputString), sizeof(aPart)-1, sizeof(bPart)-1,
            sizeof(cPart)-1, sizeof(dPart)-1, sizeof(ePart)-1, sizeof(fPart));

    (*env)->ReleaseStringUTFChars (env, j_inputString, inputString);


    cls = (*env)->GetObjectClass (env, j_aPart);
    if (cls != 0) {
        fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
        if (fid != 0) {
            if (nstats[0] >= 0) {
                aPart[nstats[0]] = '\0';
                jstr = (*env)->NewStringUTF(env, aPart);
            }
            else {
                jstr = NULL;
            }
            (*env)->SetObjectField (env, j_aPart, fid, jstr);
        }
    }

    cls = (*env)->GetObjectClass (env, j_bPart);
    if (cls != 0) {
        fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
        if (fid != 0) {
            if (nstats[1] >= 0) {
                bPart[nstats[1]] = '\0';
                jstr = (*env)->NewStringUTF(env, bPart);
            }
            else {
                jstr = NULL;
            }
            (*env)->SetObjectField (env, j_bPart, fid, jstr);
        }
    }

    cls = (*env)->GetObjectClass (env, j_cPart);
    if (cls != 0) {
        fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
        if (fid != 0) {
            if (nstats[2] >= 0) {
                cPart[nstats[2]] = '\0';
                jstr = (*env)->NewStringUTF(env, cPart);
            }
            else {
                jstr = NULL;
            }
            (*env)->SetObjectField (env, j_cPart, fid, jstr);
        }
    }

    cls = (*env)->GetObjectClass (env, j_dPart);
    if (cls != 0) {
        fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
        if (fid != 0) {
            if (nstats[3] >= 0) {
                dPart[nstats[3]] = '\0';
                jstr = (*env)->NewStringUTF(env, dPart);
            }
            else {
                jstr = NULL;
            }
            (*env)->SetObjectField (env, j_dPart, fid, jstr);
        }
    }

    cls = (*env)->GetObjectClass (env, j_ePart);
    if (cls != 0) {
        fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
        if (fid != 0) {
            if (nstats[4] >= 0) {
                ePart[nstats[4]] = '\0';
                jstr = (*env)->NewStringUTF(env, ePart);
            }
            else {
                jstr = NULL;
            }
            (*env)->SetObjectField (env, j_ePart, fid, jstr);
        }
    }

    cls = (*env)->GetObjectClass (env, j_fPart);
    if (cls != 0) {
        fid = (*env)->GetFieldID (env, cls, "string", "Ljava/lang/String;");
        if (fid != 0) {
            if (nstats[5] >= 0) {
                fPart[nstats[5]] = '\0';
                jstr = (*env)->NewStringUTF(env, fPart);
            }
            else {
                jstr = NULL;
            }
            (*env)->SetObjectField (env, j_fPart, fid, jstr);
        }
    }

    if (nstats[0] == -10)
        return -1;

    return 0;
}
