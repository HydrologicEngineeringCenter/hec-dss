#ifdef _MSC_VER
#include <string.h>
#include <jni.h>
#include <stdio.h>
#include "fotran_string_len_size.h"

void loadparam_ (int *iunitOut, const char *cpfile,  
		const char *csfile, 
		slen_t lencpfile, slen_t lencsfile);


JNIEXPORT void JNICALL Java_hec_heclib_dss_Sheflib_Hec_1LoadParam
    (JNIEnv *env, jobject obj, jint j_iunitOut, jstring j_cpfile,
	 jstring j_csfile)
{

	/*SUBROUTINE LoadParam_ [STDCALL] (LFNOUT, CPFILE, CSFILE, 
     *   L_CPFILE, L_CSFILE)
	 */
    int iunitOut;
	const char *cpfile;
    const char *csfile;

	iunitOut = (int) j_iunitOut;
    cpfile = (const char *) (*env)->GetStringUTFChars (env, j_cpfile, 0);
	csfile = (const char *) (*env)->GetStringUTFChars (env, j_csfile, 0);


    loadparam_ (&iunitOut, cpfile, csfile,		
				strlen (cpfile), strlen(csfile));
	
    /* Release the file name */
    (*env)->ReleaseStringUTFChars (env, j_cpfile, cpfile);
	(*env)->ReleaseStringUTFChars (env, j_csfile, csfile);	
    

}

#endif
