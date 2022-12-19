#include <fortran_string_len_size.h>

#ifdef _MSC_VER

#include <windows.h>
#include <stdlib.h>

void isonlocaldrive_ (const char *pathName, int *isLocal, slen_t len_pathName)
{

	wchar_t absPath[_MAX_PATH];
	wchar_t wcstr[_MAX_PATH];
	TCHAR** lppPart={NULL};
	size_t returnLen;
	int drive;

	mbstowcs_s(&returnLen, wcstr, _MAX_PATH, pathName, strlen(pathName));

	GetFullPathName(wcstr, _MAX_PATH, absPath, lppPart);
	absPath[3] = '\0';
	drive = GetDriveType(absPath);
	switch (drive) {
		case DRIVE_FIXED     :
		case DRIVE_REMOVABLE :
		case DRIVE_CDROM     :
		case DRIVE_RAMDISK   :
		  /* return TRUE; */
			*isLocal = 1;
			return;
		default :
		  /* return FALSE; */
			*isLocal = 0;
			return;

	}
}
#else
void isonlocaldrive_(const char *pathName, int *isLocal, slen_t len_pathName)
{
	*isLocal = 1;
}
#endif

