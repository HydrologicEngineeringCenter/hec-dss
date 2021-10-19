/* minimal code example showing how to call the zfp (de)compressor */

//#define ZLIB_WINAPI   // actually actually needed (for linkage)
#ifdef _MSC_VER
#include "windows.h"  // get BYTE et al.
#endif
#include "zlib.h"
#include <stdlib.h>

int GetMaxCompressedLen(int size)
{
	int n16kBlocks = (size + 16383) / 16384; // round up any fraction of a block
	return (size + 6 + (n16kBlocks * 5));
}

int compress_zlib(void* array, int size, void **buffer)
{
	z_stream zInfo = { 0 };
	zInfo.total_in = zInfo.avail_in = size;
	zInfo.total_out = zInfo.avail_out = GetMaxCompressedLen(size);
	zInfo.next_in = (void *)array;
	*buffer = malloc(GetMaxCompressedLen(size));
	zInfo.next_out = *buffer;
	zInfo.zalloc = (alloc_func)0;
	zInfo.zfree = (free_func)0;
	zInfo.opaque = (voidpf)0;

	int nerr, nret = -1;
	nerr = deflateInit(&zInfo, Z_DEFAULT_COMPRESSION); // zlib function
	if (nerr == Z_OK) {
		nerr = deflate(&zInfo, Z_FINISH);              // zlib function
		if (nerr == Z_STREAM_END) {
			nret = zInfo.total_out;
		}
	}
	deflateEnd(&zInfo);    // zlib function
	return nret;
}

int uncompress_zlib(const void* buffer, int size, void* data,int dataSize)
{
	z_stream zInfo = { 0 };
	zInfo.total_in = zInfo.avail_in = size;
	zInfo.total_out = zInfo.avail_out = dataSize;
	zInfo.next_in = (Bytef*) buffer;
	zInfo.next_out = data;
	zInfo.zalloc = (alloc_func)0;
	zInfo.zfree = (free_func)0;

	int nerr, nret = -1;
	nerr = inflateInit(&zInfo);               // zlib function
	if (nerr == Z_OK) {
		nerr = inflate(&zInfo, Z_FINISH);     // zlib function
		if (nerr == Z_STREAM_END) {
			nret = zInfo.total_out;
		}
	}
	inflateEnd(&zInfo);   // zlib function
	return(nret); // -1 or len of output
}
