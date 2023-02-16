#include <heclib.h>
#include <string.h>

#define KIHEAD 1000
#define KCHEAD 100

int zCopyRecord6Dynamic(long long* ifltabFrom, long long* ifltabTo, const char* pathnameFrom, const char* pathnameTo) {

	int nPathname;
	int kihead = KIHEAD;
	int kchead = KCHEAD;
	int kuhead = 0;
	int kdata = 0;
	int nihead = 0;
	int nchead = 0;
	int nuhead = 0;
	int ndata = 0;
	int found = 0;
	int iplan = 0;
	int itype = 0;
	int status = 0;
	int ihead[KIHEAD] = { 0 };
	int chead[KCHEAD] = { 0 };
	int* uhead = NULL;
	int* data = NULL;

	if (pathnameFrom == NULL || pathnameTo == NULL) {
		return -1;
	}
	nPathname = strlen(pathnameFrom);
	zcheck_(ifltabFrom, pathnameFrom, &nPathname, &kuhead, &kdata, &found, strlen(pathnameFrom));
	if (!found) {
		return -2;
	}
	itype = zdataType(ifltabFrom, pathnameFrom);
	if (itype == 0) {
		return -3;
	}
	if (kuhead > 0) {
		uhead = (int*)malloc(kuhead * sizeof(int));
	}
	if (kdata > 0) {
		data = (int*)malloc(kdata * sizeof(int));
	}
	zreadx(ifltabFrom, pathnameFrom, ihead, &kihead, &nihead, chead, &kchead, &nchead, uhead, &kuhead, &nuhead, data, &kdata, &ndata, &iplan, &found);
	nPathname = strlen(pathnameTo);
	zwritex_(ifltabTo, pathnameTo, &nPathname, ihead, &nihead, chead, &nchead, uhead, &nuhead, data, &ndata, &itype, &iplan, &status, &found, strlen(pathnameTo));

	free(uhead);
	free(data);
	return status;
}

#undef KIHEAD
#undef KCHEAD

void zcopyrecord6dynamic_(long long* ifltabFrom, long long* ifltabTo, const char* pathnameFrom, const char* pathnameTo, int* status, size_t pathnameFromLen, size_t pathnameToLen) {
	char* cPathnameFrom = stringFortToC(pathnameFrom, pathnameFromLen);
	char* cPathnameTo = stringFortToC(pathnameTo, pathnameToLen);
	*status = zCopyRecord6Dynamic(ifltabFrom, ifltabTo, cPathnameFrom, cPathnameTo);
	free(cPathnameFrom);
	free(cPathnameTo);
}

