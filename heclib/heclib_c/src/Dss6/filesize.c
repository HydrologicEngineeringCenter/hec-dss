#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>

#ifdef _MSC_VER
#include <io.h>
#include <stdlib.h>
#define stat64 _stat64
#include <windows.h>
typedef __int32 int32_t;
typedef __int64 int64_t;
#else
#include <errno.h>
#include <stdint.h>
#endif

#include "heclibDate.h"

//
// Get the size in bytes of the specified file.
//
// Calling FILESIZE from Fortran with an INTEGER*4 variable for
// the nbytes parameter will return in one of 3 conditions:
//
//   1. status = 0, nbytes >= 0.  This indicates success, with the
//      file size return in nbytes.
//
//   2. status != 0.  Error condition, most often status = 2 which
//      indicates the specified file file could not be located.
//
//   3. status = 0, nbytes = -1.  This indicates the size of the
//      file was successfully determined, but does not fit in a 4-byte
//      integer (e.g. file size >= 2 GB).  Call FILESIZEN64 instead.
//
//
// FILESIZE64 must be called from Fortran with an INTEGER*8 variable
// for the nbytes parameter, and the return condition will be 1. or 2.
// from above.
//

#if (defined(__SUNPRO_CC) || defined(__linux__) || (defined __APPLE__))
//
// Sun Studio 12 C++ on 32-bit Linux doesn't support large file operations and
// Sun Studio 12 C++ on 64-bit Linux doesn't use special names for large file operatios
//
#define stat64 stat
#define fstat64 fstat
#endif

int stringCopy(char *destination, size_t sizeOfDestination, const char* source, size_t lenSource);

	//-------------------//
	// filename versions //
	//-------------------//

	void filesizen64_(char *fname, int64_t *nbytes, int32_t *status, int32_t filename_len) {

#ifdef _MSC_VER
#if _MSC_VER < 1400
		HANDLE h_file = CreateFile(
			LPCSTR(fname),
			GENERIC_READ,
			FILE_SHARE_WRITE,
			NULL,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL,
			NULL);
		if (h_file == INVALID_HANDLE_VALUE) {
			*status = -1;
		}
		else {
			*status = !GetFileSizeEx(h_file, reinterpret_cast<PLARGE_INTEGER>(nbytes)) == 0 ? -1 : 0;
			CloseHandle(h_file);
		}
		if (*status) *nbytes = -1;
#else
		struct __stat64 buf;
		*status = _stat64(fname, &buf) == 0 ? 0 : errno;
		*nbytes = *status == 0 ? buf.st_size : -1;
#endif
#else
		struct stat64 buf;
		*status = stat64(fname, &buf) == 0 ? 0 : errno;
		*nbytes = *status == 0 ? buf.st_size : -1;
#endif
	}

	void filesizen_(char *filename, int32_t *nbytes, int32_t *status, int32_t filename_len) {

		char *fname = NULL;
		int32_t i;

		fname = (char *)malloc(filename_len + 1);
		stringCopy(fname, (filename_len + 1), filename, filename_len);
		fname[filename_len] = '\0';
		for (i = filename_len - 1; i >= 0; --i) {
			if (fname[i] != ' ') break;
			fname[i] = '\0';
		}
		int64_t filesize;
		filesizen64_(fname, &filesize, status, filename_len);
		if (!*status) {
			*nbytes = (int)(filesize <= LONG_MAX ? filesize : -1);
		}
		free(fname);
	}

	//-----------------//
	// handle versions //
	//-----------------//
	void filesize64_(int32_t *handle, int64_t *nbytes) {

#ifdef _MSC_VER
		*nbytes = _filelengthi64(*handle);
#else
		struct stat64 buf;
		int32_t status = fstat64(*handle, &buf) == 0 ? 0 : errno;
		*nbytes = status == 0 ? buf.st_size : -1;
#endif
	}

	void filesize_(int32_t *handle, int32_t *nbytes) {

		int64_t filesize;
		filesize64_(handle, &filesize);
		*nbytes = (int)(filesize <= LONG_MAX ? filesize : -1);
	}

