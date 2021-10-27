#include <jni.h>
#if defined(_MSC_VER)
  #include <windows.h>
  #include <process.h>
  #include <tchar.h>
  #include <Psapi.h>
#endif
#include <string.h>
#include "heclib.h"

JNIEXPORT jstring JNICALL  Java_hec_heclib_util_Heclib_Hec_1getProcessName
  (JNIEnv *env, jobject obj, jint j_pid) 
{

	jstring j_name;

	int i;
        
#if defined(_MSC_VER)

	DWORD pid;
	TCHAR  buff[MAX_PATH];
	char cname[MAX_PATH];
	HANDLE hProcess;
	DWORD len = MAX_PATH;

	pid = (DWORD)j_pid;
	
	hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, pid);

	if (hProcess != NULL) {
		QueryFullProcessImageName( hProcess, 0, buff, &len);
		for (i=0; i<(int)len; i++) {
			cname[i] = (char)buff[i];
		}
		cname[len] = '\0';
	}
	else {
		stringCopy (cname, MAX_PATH, "No such process", _TRUNCATE);
	}

	j_name = (*env)->NewStringUTF (env, cname);
#else
      
       j_name = (*env)->NewStringUTF(env,""); 
#endif 
        
	return j_name;
}
