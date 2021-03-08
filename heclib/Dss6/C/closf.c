#ifdef _MSC_VER
#include <io.h>
void closf_(int *ihandle, int *istat)
{
	*istat = (_close(*ihandle) != -1 ? 0 : -1);
}
#else
#include <unistd.h>
#include <stdint.h>
void closf_(int32_t *ihandle, int32_t *istat)
{
	*istat = (close(*ihandle) != -1 ? 0 : -1);
}
#endif

