#include "stdio.h"
#include "string.h"
#include "math.h"

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

/*
  Read timeseries profile data from a csv file
  first row is 'depths' 
  date/time is not part of csv 
*/

#ifdef _MSC_VER
#define strtok_r strtok_s
#define strdup _strdup
#endif

int read_sizes_from_csv_file(const char* csvFilename, int* rows, int* columns);
int read_csv_file_into_array(const char* csvFilename, float* data, int rows, int columns, int rowsToSkip);


int read_profile_from_csv(zStructTimeSeries* tss, const char* csvFilename) {
#define MAX_ROW_SIZE 10000
	int rows = 0, columns = 0;
	read_sizes_from_csv_file(csvFilename, &rows, &columns);
	rows--;// header-row not included main data array
	printf("\nrows = %d, columns = %d", rows, columns);
	if (rows == 0 || columns == 0)
		return -1;

    float* profileDepths = (float*)calloc((size_t)columns, sizeof(float));
	float* values = (float*)calloc((size_t)columns * rows, sizeof(float));
	
	read_csv_file_into_array(csvFilename, profileDepths, 1, columns, 0);
	for (size_t i = 0; i < columns; i++)
	{
		//printf(" %f,", profileDepths[i]);
	}
	printf("\n\n----\n");
	read_csv_file_into_array(csvFilename,values, rows, columns,1);
	for (size_t i = 0; i < columns*rows; i++)
	{
		//printf(" %f,", values[i]);
	}
	tss->numberValues = rows;
	tss->floatProfileValues = values;
	tss->floatProfileDepths = profileDepths;
	tss->profileDepthsNumber = columns;

	tss->allocated[zSTRUCT_TS_profileFloatDepths] = 1;
	tss->allocated[zSTRUCT_TS_profileFloatValues] = 1;

	return 0;

}
int read_csv_file_into_array(const char* csvFilename, float* data, int rows, int columns, int rowsToSkip) {
	FILE* stream;
	char line[MAX_ROW_SIZE];

#ifdef _MSC_VER
	fopen_s(&stream, csvFilename, "r");
#else
	stream = fopen(csvFilename, "r");
#endif
	if (stream == 0)
		return -1;
	int skippedRows = 0;
	int col = 0;
	int row = 0;
	int pos = 0;
	while (fgets(line, MAX_ROW_SIZE, stream))
	{
		if (skippedRows < rowsToSkip) {
			skippedRows++;
			continue;
		}
		char* context = NULL;
		const char seps[] = ",\n";
		int col = 0;
		// parse csv text line
		float f = 0.0;
		char* s = strtok_r(line, seps, &context);
		
		while (s && col < columns && row < rows) { 

			if (strlen(s) > 0)
				f = (float)atof(s);
			else
				f = UNDEFINED_FLOAT;
			data[pos] = f;
			col++;
			pos++;
			s = strtok_r(NULL, seps, &context);
		}
		row++;
	}

	fclose(stream);
	return 0;
}

 
/*
  read_sizes_from_csv_file 

   csvFilename csv text file,  first row is column values (floating point), other rows are data
  *rows output value ,number of rows
  *columns output value , number of columns

*/
int read_sizes_from_csv_file(const char* csvFilename, int *rows, int*columns)
{
	printf("\nreading %s", csvFilename);
	*rows = 0;
	*columns = 0;
	FILE* stream;
	char line[MAX_ROW_SIZE];

#ifdef _MSC_VER
	fopen_s(&stream, csvFilename, "r");
#else
	stream = fopen(csvFilename, "r");
#endif
	if (stream == 0)
		return -1;
	const char seps[] = ",\n";
	while (fgets(line, MAX_ROW_SIZE, stream))
	{
		
		if (*rows == 1) { // use second row to count columns
			char* context = NULL;

			char* s = strtok_r(line, seps, &context);
			while (s) { // parse csv.
				(* columns)++;
				s = strtok_r(NULL, seps, &context);
			}
		}

//		printf("%s", tmp);
		(*rows)++;
	}

	fclose(stream);
	return 0;
}


int write_profile_to_csv(zStructTimeSeries *tss, const char* csvFilename)
{
	FILE *stream;
	char startDate[20];
	char startTime[20];
	int i, j;
	int pos;

#ifdef _MSC_VER
	fopen_s( &stream, csvFilename, "w" );
#else
    stream = fopen(csvFilename, "w");
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
			if( tss->doubleProfileValues)
			  fprintf( stream, ",%f", tss->doubleProfileValues[pos]);
			else if(tss->floatProfileValues)
				fprintf(stream, ",%f", tss->floatProfileValues[pos]);
			else 
				fprintf(stream, ",%f", UNDEFINED_FLOAT);
		}
		fprintf( stream, "\n");
	}
  
   fclose( stream );

   printf("%s written",csvFilename);
   return 0;
	
}
