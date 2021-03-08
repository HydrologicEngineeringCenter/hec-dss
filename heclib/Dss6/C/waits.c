/*
 *    WAITS - Wait for some duration of seconds (EX: 2.502148 seconds)
 *        CALL WAITS ( SECS )
 *
 *    Must compile with -lrt flag on Solaris
 *
 */
 
#ifdef _MSC_VER

/*--[WINDOWS]---------------------------------------------------------*/

#include <windows.h>
void waits_(float *secs) {Sleep((DWORD)(*secs * 1000)); }

#else

/*--[UNIX]------------------------------------------------------------*/

#include <time.h>
#include <math.h>
#include <stdint.h>
void waits_(float *secs) {
	
	struct timespec ts;
	ts.tv_sec = (time_t)*secs;
	ts.tv_nsec = (int32_t)(1.0e9 * (*secs - floor(*secs)));
	nanosleep(&ts, NULL);
}

/*--------------------------------------------------------------------*/
#endif

 
 
