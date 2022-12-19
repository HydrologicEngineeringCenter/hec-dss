#include "hecdssInternal.h"
#include "fortran_string_len_size.h"
#include <stdlib.h>
/* open a file using "c" i/o, typically from fortran
   set iaccess = 10 (decimal) for most apps.
   istat is returned -1 if error, otherwise 0 */

void openf_ (char *cname, int *iaccess, int *ihandl, int *istat, slen_t len_cname)
{
	char *path;
	path = stringFortToC(cname, len_cname);
	*istat = zopenDisk(path, ihandl, *iaccess, 0);
	free(path);
}

