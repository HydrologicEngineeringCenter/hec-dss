#include <heclib.h>
#include <stdlib.h>
#include <memory.h>

#ifdef _MSC_VER
	#include <io.h>
	#define F_OK 0
	#define access _access
	#define COPY_CMD "copy"
#else
	#include <unistd.h>
	#define COPY_CMD "cp"
#endif

int exists(const char* fname) {
	return access(fname, F_OK) == 0;
}

int testLargeCopy() {
	int status;
	long long ifltab[2][250];
	const char *src_filename6 = "large-record-src6.dss";
	const char *src_filename7 = "large-record-src7.dss";
	const char *dst_filename6 = "large-record-dst6.dss";
	const char *dst_filename7 = "large-record-dst7.dss";
	const char *src_filename = NULL;
	const char *dst_filename = NULL;
	
	src_filename = src_filename7;
	dst_filename = dst_filename7;
	if (!exists(src_filename)) {
		printf("No such file: %s\n", src_filename);
		return 1;
	}
	if (exists(dst_filename)) {
		remove(dst_filename);
	}
	memset(ifltab, 0, sizeof(ifltab));
	status = zopen7(ifltab[0], src_filename);
	
	assert(status == 0);			
	status = zopen7(ifltab[1], dst_filename);
	assert(status == 0);

	status = zcopyFile(ifltab[0], ifltab[1], REC_STATUS_VALID);
	zclose(ifltab[0]);
	zclose(ifltab[1]);
		
	return status;
}