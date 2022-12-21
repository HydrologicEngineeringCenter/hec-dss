#ifndef __FORTRAN_STRING_LEN_SIZE_INCLUDED__
#define __FORTRAN_STRING_LEN_SIZE_INCLUDED__

#include <stdlib.h>
#if defined(_MSC_VER)
	#ifdef _WIN64
			typedef int64_t slen_t;
	#else
			typedef int32_t slen_t;
	#endif
#else
    # if __GNUC__
        #if __GNUC__ > 7 || defined(__APPLE__)
            typedef size_t slen_t;
        #else
            typedef int slen_t;
        #endif
    #else
        typedef int32_t slen_t;
    #endif
#endif

#endif //#ifndef __FORTRAN_STRING_LEN_SIZE_INCLUDED__
