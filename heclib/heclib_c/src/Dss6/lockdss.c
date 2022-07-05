// lockdss is called by Fortran (DSS6)
#ifdef _MSC_VER

#include <stdio.h>
#include <sys/locking.h>
#include <errno.h>
#include <io.h>

void lockdss_(int *ihandle, int *mode, int *position, int *nbytes, int *istat)
{
	long lbytes;
	long lposition;
	long int newPosition;
	/*   extern char *sys_errlist[];
	extern int errno;  */

	lbytes = (long)*nbytes;
	lposition = (long)*position;

	newPosition = _lseek(*ihandle, lposition, SEEK_SET);
	if (newPosition < 0) {
		*istat = -1;
		return;
	}

	/*   Unlock the record */
	if (*mode == 0) {
		*istat = _locking(*ihandle, _LK_UNLCK, lbytes);
	}
	/*   Lock the record.  If already locked, wait until available */
	else if (*mode == 1) {
		*istat = _locking(*ihandle, _LK_LOCK, lbytes);
	}
	/*   Lock the record.  If already locked, return with istat != 0 */
	else if (*mode == 2) {
		*istat = _locking(*ihandle, _LK_NBLCK, lbytes);
	}
	/*   Test to see if the record is already locked */
	else if (*mode == 3) {
		*istat = _locking(*ihandle, _LK_NBLCK, lbytes);
		if (*istat == 0) {
			_locking(*ihandle, _LK_UNLCK, lbytes);
		}
	}

	/*if (*istat == -1) perror ("\nError: Lock Failed:  %s\n"); */

}

#else


#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdint.h>

#define UNLOCK 0
#define LOCK_WAIT 1
#define LOCK_NOWAIT 2
#define TEST_LOCK 3

void lockdss_(int *ihandle, int *mode, int *position, int *nbytes, int *istat)
{
	off_t lbytes;
	off_t lposition;
	long newPosition;
	
	lbytes = (off_t)*nbytes;
	lposition = (off_t)*position;

	newPosition = lseek(*ihandle, lposition, SEEK_SET);
	if (newPosition < 0) {
		*istat = -1;
		return;
	}

	switch (*mode) {

	case UNLOCK:
		*istat = lockf(*ihandle, F_ULOCK, lbytes);
		break;

	case LOCK_WAIT:
		*istat = lockf(*ihandle, F_LOCK, lbytes);
		if (*istat) printf("\nError: Lock Failed:\n");
		break;

	case LOCK_NOWAIT:
		*istat = lockf(*ihandle, F_TLOCK, lbytes);
		break;

	case TEST_LOCK:
		*istat = lockf(*ihandle, F_TEST, lbytes);
		break;

	default:
		printf("\nInvalid lock mode: %d\n", *mode);
		*istat = -1;
	}
}
#endif
