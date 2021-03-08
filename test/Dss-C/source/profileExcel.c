#include "stdio.h"
#include "string.h"
#include "math.h"

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

// void printDouble(double *d)
// {
// 	int i, j, k;
// 	while (1) {
// 	printf("Enter start index and number to print, 0 to exit\n");
// 	scanf("%d", &i);
// 	scanf("%d", &j);
// 	if (j== 0) return;
// 	for (k=i; k<(j+i); k++) {
// 		printf("%d, %f\n",k,d[k]);
// 	}
// 	}
// }

int profileExcel(zStructTimeSeries *tss)
{
	/*int ihandl;
	_sopen_s (&ihandl, "profile.csv", 8, _SH_DENYNO, _S_IWRITE);
	if (ihandl <= 0) {
		printf("open failed\n");
		return;
	}
	*/

	FILE *stream;
	char startDate[20];
	char startTime[20];
	int i, j;
	int pos;

#ifdef _MSC_VER
	fopen_s( &stream, "profile.csv", "w" );
#else
    stream = fopen("profile.csv", "w");
#endif
	if (stream == 0) {
		printf("open failed\n");
		return -1;
	}
		
	for (i=0; i<tss->numberValues; i++) {
		if (tss->times) {
			minsToDateTime(tss->times[i], startDate, startTime, sizeof(startDate), sizeof(startTime));
			fprintf( stream, "%s,%s", startDate, startTime);
		}
		else {
			fprintf( stream, "%d", i);
		}
		for (j=0; j<tss->profileDepthsNumber; j++) {
			pos = (i * tss->profileDepthsNumber) + j;
			fprintf( stream, ",%f", tss->doubleProfileValues[pos]);
		}
		fprintf( stream, "\n");
	}
  
   fclose( stream );

   printf("profile.csv written");
   return 0;
	
}
