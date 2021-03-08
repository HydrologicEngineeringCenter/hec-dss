#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "heclib.h"

int ExampleOpen();
int ExampleTimeSeries1();
int ExampleTimeSeries2();
int ExampleLogging();
int ExamplePairedData();
int ExamplePairedData2();
int ExamplePairedData3();
int ExampleText1();
int ExampleText2();
int ExampleText3();
int ExampleCatalog();
int ExampleSpeedTest();


void printTextStruct(zStructText *textStruct);

/**
	This program writes DSS files to C:/temp.  That directory must exist before running.
	This shows examples for HEC-DSS version 7.  HEC-DSS version 6 files may be used
	also, however, new features and data types cannot be stored in version 6 
	(however, the same calls to store or retrieve data are used.)

	Data is passed through structs.
*/

int mainx(int argc, int* argv[])
{	
	//zset("mlvl", "", 15);
	/*
	printf("\nBegin ExampleOpen\n\n");
	ExampleOpen();
	printf("\nCompleted ExampleOpen\n");
	printf("\nBegin ExampleTimeSeries1\n\n");
	ExampleTimeSeries1();
	printf("\nCompleted ExampleTimeSeries1\n");
	printf("\nBegin ExampleTimeSeries2\n\n");
	ExampleTimeSeries2();
	printf("\nCompleted ExampleTimeSeries2\n");
	printf("\nBegin ExamplePairedData\n\n");
	return 0;
	
	ExamplePairedData();
	printf("\nCompleted ExamplePairedData\n");
	*/
	
	printf("\nBegin ExamplePairedData2\n\n");
	ExamplePairedData2();
	printf("\nCompleted ExamplePairedData2\n");
	return 0;
	printf("\nBegin ExamplePairedData3\n\n");
	ExamplePairedData3();
	printf("\nCompleted ExamplePairedData3\n");
	printf("\nBegin ExampleText1\n\n");

	ExampleText1();
	printf("\nCompleted ExampleText1\n");
	printf("\nBegin ExampleText2\n\n");
	ExampleText2();
	printf("\nCompleted ExampleText2\n");
	printf("\nBegin ExampleText3\n\n");
	ExampleText3();
	printf("\nCompleted ExampleText3\n");

//	ExampleCatalog();
/*
	ExampleSpeedTest();

	
	SpeedTest();

*/
	printf("\nCompleted DSS C code examples\n\n");
	
}


int ExampleOpen()
{
	long long ifltab[250];
	int status;

	//  Open the DSS file.  
	//  If it doesn't exisit, it will be created as a version 7 file.
	//  See advanced for checking a file version and setting version to create.
#ifdef _MSC_VER
	status = zopen(ifltab, "j:/heclib/heclib7/test/ExampleOpen_pc.dss");
#else
	status = zopen(ifltab, "/netapp/perforce/depot/usr/hec/code/heclib7/test/ExampleOpen.dss");
#endif
	if (status != STATUS_OKAY) return status;

	//  Do DSS stuff (reads / writes)

	//  When all done (near end of program), close the file
	zclose(ifltab);

	return 0;
}


int ExampleTimeSeries1()
{
	long long ifltab[250];
	zStructTimeSeries *tss1, *tss2, *tss3, *tss4;
	float fvalues[200];
	double dvalues[300];
	int itimes[300];
	char cdate[13], ctime[10];
	int valueTime, status, i;

	//  Open the DSS file; Create if it doesn't exist
#ifdef _MSC_VER
	status = zopen(ifltab, "j:/heclib/heclib7/test/ExampleTimeSeries1pc.dss");
#else
	status = zopen(ifltab, "/netapp/perforce/depot/usr/hec/code/heclib7/test/ExampleTimeSeries1.dss");
#endif
	if (status != STATUS_OKAY) return status;

	//  Write a regular interval data set.  Gen up the data
	for (i = 0; i<200; i++) {
		fvalues[i] = (float)i;
	}
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/C Test/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	zstructFree((void *)tss1);
	if (status != STATUS_OKAY) return status;

	//  Write a irregular interval data set.  Gen up the data
	for (i = 0; i<300; i++) {
		dvalues[i] = (double)i;
		itimes[i] = i * 1440;
	}
	tss2 = zstructTsNewIrregDoubles("/Basin/Location/Flow//~1Day/C Example/", dvalues, 300, itimes, MINUTE_GRANULARITY, "20April2012", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss2, 0);
	zstructFree((void *)tss2);
	if (status != STATUS_OKAY) return status;

	//  Read the data back in
	tss3 = zstructTsNew("/Basin/Location/Flow/01Jan2001/1Hour/C Test/");
	status = ztsRetrieve(ifltab, tss3, -1, 1, 0);
	if (status != STATUS_OKAY) return status;
	//  Print out.  (Compute time for each ordinate, values are floats)
	valueTime = tss3->startTimeSeconds;
	for (i = 0; i<tss3->numberValues; i++) {
		getDateAndTime(valueTime, SECOND_GRANULARITY, tss3->startJulianDate,
			cdate, sizeof(cdate), ctime, sizeof(ctime));
		printf("Oridnate %d, for %s, %s, value is %f\n", i, cdate, ctime, tss3->floatValues[i]);
		valueTime += 3600;  //  Increment to next hour
	}

	tss4 = zstructTsNewTimes("/Basin/Location/Flow//~1Day/C Example/", "19April2012", "2400", "01July2013", "2400");
	status = ztsRetrieve(ifltab, tss4, 0, 2, 0);
	if (status != STATUS_OKAY) return status;
	//  Print out (values returned as doubles)
	for (i = 0; i<tss4->numberValues; i++) {
		getDateAndTime(tss4->times[i], tss4->timeGranularitySeconds, tss4->julianBaseDate,
			cdate, sizeof(cdate), ctime, sizeof(ctime));
		printf("Oridnate %d, for %s, %s, value is %f\n", i, cdate, ctime, tss4->doubleValues[i]);
	}

	zstructFree((void *)tss3);
	zstructFree((void *)tss4);
	zclose(ifltab);
	return 0;
}


//  Regular-interval time series data with quality and character notes.
int ExampleTimeSeries2()
{
	long long ifltab[250];
	zStructTimeSeries *tss1, *tss2;
	float fvalues[200];
	int quality[200];
	char cnotes1[10000];
	int noteCount;

	char cdate[13], ctime[10];
	int valueTime, status, i, j, n;
	int ich;

	//  Open the DSS file; Create if it doesn't exist
#ifdef _MSC_VER
	status = zopen(ifltab, "j:/heclib/heclib7/test/ExampleTimeSeries2pc.dss");
#else
	status = zopen(ifltab, "/netapp/perforce/depot/usr/hec/code/heclib7/test/ExampleTimeSeries2.dss");
#endif
	if (status != STATUS_OKAY) return status;



	//  Write a regular interval data set.  Gen up the data
	ich = 66; //ichar('A')
	noteCount = 0;
	for (i = 0; i<200; i++) {
		fvalues[i] = (float)i;
		quality[i] = i + 10;
		//  Character note
		n = i / 25;
		n = i - (n * 25) + 1;
		for (j = 0; j<n; j++) {
			cnotes1[noteCount++] = ich + j - 1 + 32;
		}
		cnotes1[noteCount++] = '\0';
	}
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/C with Quality and C notes/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	tss1->quality = quality;
	tss1->qualityElementSize = 1;
	tss1->cnotes = cnotes1;
	tss1->cnotesLengthTotal = noteCount;
	status = ztsStore(ifltab, tss1, 0);
	zstructFree((void *)tss1);
	if (status != STATUS_OKAY) {
		zclose(ifltab);
		return status;
	}

	//  Read the data back in
	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan2001/1Hour/C with Quality and C notes/");
	//  Remember:  last 3 argumenst: int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes)
	status = ztsRetrieve(ifltab, tss2, -1, 1, 1);
	if (status != STATUS_OKAY) return status;
	//  Print out.  (Compute time for each ordinate, values are floats)
	valueTime = tss2->startTimeSeconds;
	noteCount = 0;
	for (i = 0; i<tss2->numberValues; i++) {
		getDateAndTime(valueTime, SECOND_GRANULARITY, tss2->startJulianDate,
			cdate, sizeof(cdate), ctime, sizeof(ctime));
		printf("Oridnate %d, for %s, %s, value is %f, quality %d, notes ->%s<-\n", i, cdate, ctime, tss2->floatValues[i], tss2->quality[i], &tss2->cnotes[noteCount]);
		noteCount += (int)strlen(&tss2->cnotes[noteCount]) + 1;
		valueTime += 3600;  //  Increment to next hour
	}

	zstructFree((void *)tss2);
	zclose(ifltab);
	return 0;
}


void printPdStruct(zStructPairedData *pds, int startingCurve, int endingCurve,
	int startingRow, int endingRow);


int ExamplePairedData()
{
	//  Because of (historical) conventions, a column is a curve.
	//  Column 0 is the ordinates array, column 1 is the first curve

	long long ifltab[250];
	zStructPairedData *pds1, *pds2, *pds3, *pds4;
	float fordinates[200], fvalues[200];
	double dordinates[300], dvalues[5][300];
	char label[20], labels[100];
	int labelLength;
	int len;

	int status, i;

	//  Open the DSS file; Create if it doesn't exist
#ifdef _MSC_VER
	status = zopen(ifltab, "j:/heclib/heclib7/test/ExamplePairedDatapc.dss");
#else
	status = zopen(ifltab, "/netapp/perforce/depot/usr/hec/code/heclib7/test/ExamplePairedData.dss");
#endif
	if (status != STATUS_OKAY) return status;

	//  Write a rating table data set.  Gen up the data
	for (i = 0; i<200; i++) {
		fordinates[i] = (float)i;
		fvalues[i] = (float)(10 * i);
	}
	pds1 = zstructPdNewFloats("/Basin/Location/Stage-Flow///Test/", fordinates, fvalues, 200, 1,
		"Feet", "Unt", "cfs", "Unt");
	status = zpdStore(ifltab, pds1, 0);
	zstructFree((void *)pds1);
	if (status != STATUS_OKAY) return status;

	//  Write a Flow-Frequency curve.  Gen up the data
	for (i = 0; i<300; i++) {
		dordinates[i] = ((double)i / 3.0);
		dvalues[0][i] = 3000.0 - (double)(i) * 10.0;
		dvalues[1][i] = 6000.0 - (double)(i) * 20.0;
		dvalues[2][i] = 9000.0 - (double)(i) * 30.0;
		dvalues[3][i] = 12000.0 - (double)(i) * 40.0;
		dvalues[4][i] = 15000.0 - (double)(i) * 50.0;
	}
	pds2 = zstructPdNewDoubles("/Basin/Location/Freq-Flow///Test/", dordinates, (double *)dvalues, 300, 5,
		"Precent", "Freq", "cfs", "Flow");
	//  Gen up labels.  Because C doesn't do well with double dimensioned arrays, especially
	//  with varing length, we terminate each label with a null	
	stringCopy(labels, sizeof(labels), "Existing\0", 9);
	labelLength = (int)strlen(labels) + 1;
	for (i = 1; i<5; i++) {
		_snprintf_s(label, sizeof(label), _TRUNCATE, "Plan %d", i);
		len = (int)strlen(label);
		stringCopy(&labels[labelLength], sizeof(labels) - labelLength, label, len);
		labelLength += len + 1;
	}
	pds2->labels = labels;
	pds2->labelsLength = labelLength;
	pds2->boolIndependentIsXaxis = 1;
	status = zpdStore(ifltab, pds2, 0);
	zstructFree((void *)pds2);
	if (status != STATUS_OKAY) return status;

	//  Now retrieve the data
	pds3 = zstructPdNew("/Basin/Location/Stage-Flow///Test/");
	status = zpdRetrieve(ifltab, pds3, 1);
	//   Print the first 10 rows
	printf("\n\nExamplePairedData,  pds3 first 10 rows:\n");
	printPdStruct(pds3, 0, 0, 1, 10);
	zstructFree((void *)pds3);
	if (status != STATUS_OKAY) return status;

	pds4 = zstructPdNew("/Basin/Location/Freq-Flow///Test/");
	status = zpdRetrieve(ifltab, pds4, 2);
	//   Print the first 10 rows
	printf("\n\nExamplePairedData,  pds4 first 10 rows:\n");
	printPdStruct(pds4, 0, 0, 1, 10);
	zstructFree((void *)pds4);
	if (status != STATUS_OKAY) return status;

	zclose(ifltab);
	return 0;
}


void printPdStruct(zStructPairedData *pds, int startingCurve, int endingCurve,
	int startingRow, int endingRow);

int ExamplePairedData2a()
{
	long long ifltab[250];
	zStructPairedData *pds1, *pds2, *pds3, *pds4, *pds5, *pds6;
	zStructRecordSize *recordSize;
	double dordinates[500], dvalues[5000];
	char label[200];
	char pathname[100];

	int status, i, j, k, n;

	//  Open the DSS file; Create if it doesn't exist
#ifdef _MSC_VER
	status = zopen(ifltab, "j:/heclib/heclib7/test/ExamplePairedData2pca.dss");
#else
	status = zopen(ifltab, "/netapp/perforce/depot/usr/hec/code/heclib7/test/ExamplePairedData2.dss");
#endif
	if (status != STATUS_OKAY) return status;

	stringCopy(pathname, sizeof(pathname), "/Basin/Location/Stage-Damage///Test/", _TRUNCATE);

	//  Pre-allocate space for 500 rows for a family of 10 curves
	//  Gen up the oridnates
	for (i = 0; i<500; i++) {
		dordinates[i] = (double)(i + 1) * 10;
	}

	

	//  Now write each of the 10 curves separately 
	k = 0;
	n = 0;
	for (j = 0; j<10; j++) {
		for (i = 0; i<500; i++) {
			dvalues[k++] = ((double)(i + 1) * 100.0) + (double)(j + 1);
		}
		
		//  Gen up label, one per curve separated by a null (label1\0label2\0)
		_snprintf_s(&label[n], 19, _TRUNCATE, "This is Curve # %d", j );
		n += 17;
		label[n++] = '\0';
		
	}

	pds1 = zstructPdNewDoubles(pathname, dordinates, (double *)dvalues, 500, 10,
		"feet", "linear", "dollars", "linear");
	//  Allocate space for labels too.  We'll assume up to 20 characters per label 
	//  (including null) for each curve
	pds1->labels = label;
	pds1->labelsLength = n;
	status = zpdStore(ifltab, pds1, 0);	
	zstructFree((void *)pds1);
	if (status != STATUS_OKAY) return status;

	//  Get the data type, number of ordinates and curves for this data set
	recordSize = zstructRecordSizeNew(pathname);
	status = zgetRecordSize(ifltab, recordSize);
	if (status != STATUS_OKAY) {
		zstructFree((void *)zgetRecordSize);
		return status;
	}
	if (recordSize->dataType == DATA_TYPE_PD) {
		printf("Paired Data floats, number curves: %d, number ordinates: %d\n",
			recordSize->pdNumberCurves, recordSize->pdNumberOrdinates);
	}
	else if (recordSize->dataType == DATA_TYPE_PDD) {
		printf("Paired Data doubles, number curves: %d, number ordinates: %d\n",
			recordSize->pdNumberCurves, recordSize->pdNumberOrdinates);
	}
	else {
		printf("Not paired data, data type: %d\n", recordSize->dataType);
	}
	zstructFree((void *)recordSize);

	//  Now retrieve the data
	pds3 = zstructPdNew(pathname);
	status = zpdRetrieve(ifltab, pds3, 1);
	//  print a part of the struct
	printf("\n\nExamplePairedData2,  pds3 first 10 rows:\n");
	printPdStruct(pds3, 0, 0, 1, 10);
	zstructFree((void *)pds3);
	if (status != STATUS_OKAY) return status;

	//  If we only wanted to retrieve row 5-7, columns 3-8, then:
	pds4 = zstructPdNew(pathname);
	pds4->startingCurve = 5;
	pds4->endingCurve = 7;
	pds4->startingOrdinate = 3;
	pds4->endingOrdinate = 8;
	status = zpdRetrieve(ifltab, pds4, 1);
	//  See what we have
	printf("\n\nExamplePairedData2,  pds4 segment  row 3-8, columns 5-7:\n");
	printPdStruct(pds4, 0, 0, 0, 0);
	zstructFree((void *)pds4);
	if (status != STATUS_OKAY) return status;

	//  Retrieve a single curve
	pds5 = zstructPdNew(pathname);
	pds5->startingCurve = 3;
	pds5->endingCurve = 3;
	status = zpdRetrieve(ifltab, pds5, 1);
	//  See what we have
	printf("\n\nExamplePairedData2,  pds5 Curve 3 (up to row 10):\n");
	printPdStruct(pds5, 0, 0, 1, 10);
	zstructFree((void *)pds5);
	if (status != STATUS_OKAY) return status;

	//  Retrieve a single row (ordinate set)
	pds6 = zstructPdNew(pathname);
	pds6->startingOrdinate = 3;
	pds6->endingOrdinate = 3;
	status = zpdRetrieve(ifltab, pds6, 1);
	//  See what we have
	printf("\n\nExamplePairedData2,  pds6 Row 3:\n");
	printPdStruct(pds6, 0, 0, 0, 0);
	zstructFree((void *)pds6);
	if (status != STATUS_OKAY) return status;

	zclose(ifltab);
	return 0;
}

int ExamplePairedData2()
{
	long long ifltab[250];
	zStructPairedData *pds1, *pds2, *pds3, *pds4, *pds5, *pds6;
	zStructRecordSize *recordSize;
	double dordinates[500], dvalues[500];
	char label[20];
	char pathname[100];

	int status, i, j;

	//  Open the DSS file; Create if it doesn't exist
#ifdef _MSC_VER
	status = zopen(ifltab, "j:/heclib/heclib7/test/ExamplePairedData2pc.dss");
#else
	status = zopen(ifltab, "/netapp/perforce/depot/usr/hec/code/heclib7/test/ExamplePairedData2.dss");
#endif
	if (status != STATUS_OKAY) return status;

	stringCopy(pathname, sizeof(pathname), "/Basin/Location/Stage-Damage///Test/", _TRUNCATE);

	//  Pre-allocate space for 500 rows for a family of 10 curves
	//  Gen up the oridnates
	for (i = 0; i<500; i++) {
		dordinates[i] = (double)(i + 1) * 10;
	}

	pds1 = zstructPdNewDoubles(pathname, dordinates, (double *)dvalues, 500, 10,
		"feet", "linear", "dollars", "linear");
	//  Allocate space for labels too.  We'll assume up to 20 characters per label 
	//  (including null) for each curve
	pds1->labelsLength = (10 * 20);
	status = zpdStore(ifltab, pds1, 10);	// Flag 10 allocates space
	zstructFree((void *)pds1);
	if (status != STATUS_OKAY) return status;	

	//  Now write each of the 10 curves separately 
	for (j = 0; j<10; j++) {
		for (i = 0; i<500; i++) {
			dvalues[i] = ((double)(i + 1) * 100.0) + (double)(j + 1);
		}
		pds2 = zstructPdNew(pathname);

		//  First curve is #1, not #0 (by convention)
		pds2->startingCurve = j + 1;
		pds2->endingCurve = j + 1;
		pds2->doubleValues = dvalues;

		//  Gen up label, one per curve separated by a null (label1\0label2\0)
		_snprintf_s(label, sizeof(label), _TRUNCATE, "This is Curve # %d", j + 1);
		pds2->labels = label;

		pds2->labelsLength = (int)strlen(label);

		status = zpdStore(ifltab, pds2, 0);
		zstructFree((void *)pds2);
		if (status != STATUS_OKAY) return status;
		if (status == STATUS_OKAY) return status;
	}
	

	//  Get the data type, number of ordinates and curves for this data set
	recordSize = zstructRecordSizeNew(pathname);
	status = zgetRecordSize(ifltab, recordSize);
	if (status != STATUS_OKAY) {
		zstructFree((void *)zgetRecordSize);
		return status;
	}
	if (recordSize->dataType == DATA_TYPE_PD) {
		printf("Paired Data floats, number curves: %d, number ordinates: %d\n",
			recordSize->pdNumberCurves, recordSize->pdNumberOrdinates);
	}
	else if (recordSize->dataType == DATA_TYPE_PDD) {
		printf("Paired Data doubles, number curves: %d, number ordinates: %d\n",
			recordSize->pdNumberCurves, recordSize->pdNumberOrdinates);
	}
	else {
		printf("Not paired data, data type: %d\n", recordSize->dataType);
	}
	zstructFree((void *)recordSize);

	//  Now retrieve the data
	pds3 = zstructPdNew(pathname);
	status = zpdRetrieve(ifltab, pds3, 1);
	//  print a part of the struct
	printf("\n\nExamplePairedData2,  pds3 first 10 rows:\n");
	printPdStruct(pds3, 0, 0, 1, 10);
	zstructFree((void *)pds3);
	if (status != STATUS_OKAY) return status;

	//  If we only wanted to retrieve row 5-7, columns 3-8, then:
	pds4 = zstructPdNew(pathname);
	pds4->startingCurve = 5;
	pds4->endingCurve = 7;
	pds4->startingOrdinate = 3;
	pds4->endingOrdinate = 8;
	status = zpdRetrieve(ifltab, pds4, 1);
	//  See what we have
	printf("\n\nExamplePairedData2,  pds4 segment  row 3-8, columns 5-7:\n");
	printPdStruct(pds4, 0, 0, 0, 0);
	zstructFree((void *)pds4);
	if (status != STATUS_OKAY) return status;

	//  Retrieve a single curve
	pds5 = zstructPdNew(pathname);
	pds5->startingCurve = 3;
	pds5->endingCurve = 3;
	status = zpdRetrieve(ifltab, pds5, 1);
	//  See what we have
	printf("\n\nExamplePairedData2,  pds5 Curve 3 (up to row 10):\n");
	printPdStruct(pds5, 0, 0, 1, 10);
	zstructFree((void *)pds5);
	if (status != STATUS_OKAY) return status;

	//  Retrieve a single row (ordinate set)
	pds6 = zstructPdNew(pathname);
	pds6->startingOrdinate = 3;
	pds6->endingOrdinate = 3;
	status = zpdRetrieve(ifltab, pds6, 1);
	//  See what we have
	printf("\n\nExamplePairedData2,  pds6 Row 3:\n");
	printPdStruct(pds6, 0, 0, 0, 0);
	zstructFree((void *)pds6);
	if (status != STATUS_OKAY) return status;

	zclose(ifltab);
	return 0;
}


void printPdStruct(zStructPairedData *pds, int startingCurve, int endingCurve,
	int startingRow, int endingRow);

int ExamplePairedData3()
{
	long long ifltab[250];
	zStructPairedData *pds1, *pds2, *pds3, *pds4;
	double dordinates[500], dvalues[10][500];
	//char label[20], labels[100];
	//int labelLength;
	//int len;
	char pathname[100];
	int numberValues;
	int status, i, j;

	//  Open the DSS file; Create if it doesn't exist
#ifdef _MSC_VER
	status = zopen(ifltab, "j:/heclib/heclib7/test/ExamplePairedData3pc.dss");
#else
	status = zopen(ifltab, "/netapp/perforce/depot/usr/hec/code/heclib7/test/ExamplePairedData3.dss");
#endif
	if (status != STATUS_OKAY) return status;

	//  Gen up some data
	for (j = 0; j<500; j++) {
		dordinates[j] = (double)(j + 1) * 10;
		for (i = 0; i<10; i++) {
			dvalues[i][j] = ((double)(j + 1) * 100.0) + (double)(i + 1);
		}
	}

	stringCopy(pathname, sizeof(pathname), "/Basin/Location/Stage-Damage///PD 3/", _TRUNCATE);
	pds1 = zstructPdNewDoubles(pathname, dordinates, (double *)dvalues, 500, 10,
		"feet", "linear", "dollars", "linear");
	status = zpdStore(ifltab, pds1, 0);
	zstructFree((void *)pds1);
	if (status != STATUS_OKAY) return status;

	//  Now retrieve the data
	pds2 = zstructPdNew(pathname);
	status = zpdRetrieve(ifltab, pds2, 0);
	//  print a part of the struct
	printf("\n\nExamplePairedData3,  pds2 first 15 rows:\n");
	printPdStruct(pds2, 0, 0, 0, 15);
	zstructFree((void *)pds2);
	if (status != STATUS_OKAY) return status;

	//  Lets change a block of data within the curves
	//  It's easiest (not effiecent) to retrieve the data,
	//  change and store back.  Let's do that for row 5-10, columns 2-5:
	pds3 = zstructPdNew(pathname);
	pds3->startingCurve = 5;
	pds3->endingCurve = 9;
	pds3->startingOrdinate = 2;
	pds3->endingOrdinate = 5;
	status = zpdRetrieve(ifltab, pds3, 2);

	printf("\nThis is what we have read from the record\n");
	printPdStruct(pds3, 0, 0, 0, 0);

	//  We'll just use this as a single array for simplicity,
	//  instead of the doubly dimensioned array, which it really is.
	numberValues = pds3->numberCurvesInStruct * pds3->numberOrdinatesInStruct;
	//  Let's  make the values negative
	for (i = 0; i<numberValues; i++) {
		pds3->doubleValues[i] = -(pds3->doubleValues[i]);
	}
	printf("\nThis is what we will store (excluding ordinates):\n");
	printPdStruct(pds3, 0, 0, 0, 0);

	status = zpdStore(ifltab, pds3, 0);
	zstructFree((void *)pds3);
	if (status != STATUS_OKAY) return status;

	//  Now read and print the earlier block
	pds4 = zstructPdNew(pathname);
	status = zpdRetrieve(ifltab, pds4, 0);
	printf("\n\nThis is the first 15 rows after our store:\n");
	printPdStruct(pds4, 0, 0, 0, 15);
	zstructFree((void *)pds4);
	if (status != STATUS_OKAY) return status;

	zclose(ifltab);
	return 0;
}


//  An example of storing and retrieving a single text string

int ExampleText1()
{
	long long ifltab[250];
	char textStuff[] = "This is a text message that is written to HEC-DSS";
	zStructText *textStruct;
	int status;

	//  Open the DSS file.  
#ifdef _MSC_VER
	status = zopen(ifltab, "j:/heclib/heclib7/test/ExampleText1pc.dss");
#else
	status = zopen(ifltab, "/netapp/perforce/depot/usr/hec/code/heclib7/test/ExampleText1.dss");
#endif
	if (status != STATUS_OKAY) return status;

	//  Store the text string
	textStruct = zstructTextStringNew("/Group/Location/Message/d/e/f/", textStuff);
	status = ztextStore(ifltab, textStruct);
	zstructFree((void *)textStruct);
	if (status != STATUS_OKAY) return status;

	//  Retrieve
	textStruct = zstructTextNew("/Group/Location/Message/d/e/f/");
	status = ztextRetrieve(ifltab, textStruct);
	printf("text ==>%s<==\n", textStruct->textString);
	zstructFree((void *)textStruct);
	if (status != STATUS_OKAY) return status;

	//  When all done (near end of program), close the file
	zclose(ifltab);

	return 0;
}


//  An example of storing and retrieving a "text list".

int ExampleText2()
{
	long long ifltab[250];
	char line[80];
	char textList[1000];

	zStructText *textStruct;
	int i;
	size_t len;
	int cellLength;
	int count;
	int status;

	//  Open the DSS file.  
#ifdef _MSC_VER
	status = zopen(ifltab, "j:/heclib/heclib7/test/ExampleText2pc.dss");
#else
	status = zopen(ifltab, "/netapp/perforce/depot/usr/hec/code/heclib7/test/ExampleText2.dss");
#endif
	if (status != STATUS_OKAY) return status;

	//  Create the text struct
	textStruct = zstructTextNew("/Group/Location/My List/d/e/f/");

	//  Create our list.  We'll make the list 20 items long.
	//  Each item in the list will be varying length.
	count = 0;
	cellLength = 20;
	for (i = 0; i<20; i++) {
		_snprintf_s(line, sizeof(line), _TRUNCATE, "List Row %d, abcdefghijklmnopqrstuvwxyz", i);
		stringCopy(&textList[count], sizeof(textList) - count, line, cellLength);
		count += cellLength + 1;
		cellLength++;
		if (cellLength > 25) cellLength = 20;
	}
	textStruct->textTable = textList;
	textStruct->numberTableChars = count;
	textStruct->numberColumns = 1;
	textStruct->numberRows = 20;

	//  Store the text table
	status = ztextStore(ifltab, textStruct);
	zstructFree((void *)textStruct);
	textStruct = 0;
	if (status != STATUS_OKAY) return status;

	//  Now retrieve the text table
	textStruct = zstructTextNew("/Group/Location/My List/d/e/f/");
	status = ztextRetrieve(ifltab, textStruct);
	if (status != STATUS_OKAY) {
		zstructFree((void *)textStruct);
		return status;
	}

	//  Print the List (copy into line for clarity only)
	count = 0;
	for (i = 0; i<textStruct->numberRows; i++) {
		len = strnlen_hec(&textStruct->textTable[count], textStruct->numberTableChars - count);
		stringCopy(line, sizeof(line), &textStruct->textTable[count], len);
		printf("%s\n", line);
		count += (int)len + 1;
		if (count >= textStruct->numberTableChars) break;
	}

	zstructFree((void *)textStruct);

	//  When all done (near end of program), close the file
	zclose(ifltab);

	return 0;
}

//  An example of storing and retrieving a "text table" with labels.
//  Tables are saved in a columnar format (all column 1, then all column 2)
//  Which means they may have to be transposed to print nicely in C 
//  e.g., for a 4 x 6 table:
//	1	5	9	13	17	21
//	2	6	10	14	18	22
//	3	7	11	15	19	23
//	4	8	12	16	20	24

//  Also, by convention, the first row and first column is 1, 1, (not 0, 0)


int ExampleText3()
{
	long long ifltab[250];
	char line[80];
	char textTable[5000];
	char labels[200];
	zStructText *textStruct;
	int i, j;
	size_t len;
	size_t count;
	int status;

	//  Open the DSS file.  
#ifdef _MSC_VER
	status = zopen(ifltab, "j:/heclib/heclib7/test/ExampleText3pc.dss");
#else
	status = zopen(ifltab, "/netapp/perforce/depot/usr/hec/code/heclib7/test/ExampleText3.dss");
#endif
	if (status != STATUS_OKAY) return status;

	//  Create the text struct
	textStruct = zstructTextNew("/Group/Location/Col-Row/d/e/f/");
	//  Create our text table.  We'll do 5 columns by 10 rows, including ordinates
	textStruct->numberColumns = 5;
	textStruct->numberRows = 10;
	count = 0;
	for (i = 0; i<textStruct->numberColumns; i++) {
		for (j = 0; j<textStruct->numberRows; j++) {
			if (i == 0) {
				_snprintf_s(line, sizeof(line), _TRUNCATE, "Table Row %d; abcdefg", j + 1);
			}
			else {
				_snprintf_s(line, sizeof(line), _TRUNCATE, "col %d, row %d; abc", i + 1, j + 1);
			}
			stringCopy(&textTable[count], sizeof(textTable) - count, line, strlen(line));
			count += (int)strlen(line) + 1;
		}
	}
	textStruct->textTable = textTable;
	textStruct->numberTableChars = (int)count;

	//  Create label array
	count = 0;
	for (i = 0; i<5; i++) {
		_snprintf_s(line, sizeof(line), _TRUNCATE, "Column %d", i + 1);
		len = strnlen_hec(line, sizeof(line));
		stringCopy(&labels[count], sizeof(labels) - count, line, len);
		count += len + 1;
	}
	textStruct->labels = labels;
	textStruct->numberLabelChars = (int)count;

	//  If we wanted to, we could also store a text string (maybe a description?)
	_snprintf_s(line, sizeof(line), _TRUNCATE, "My text table");
	textStruct->textString = line;
	textStruct->numberTextChars = (int)strlen(line) + 1;  //  Be sure to include null terminator

														  //  Store the text table
	status = ztextStore(ifltab, textStruct);
	zstructFree((void *)textStruct);
	textStruct = 0;
	if (status != STATUS_OKAY) return status;

	//  Now retrieve the text table
	textStruct = zstructTextNew("/Group/Location/Col-Row/d/e/f/");
	status = ztextRetrieve(ifltab, textStruct);
	if (status != STATUS_OKAY) {
		zstructFree((void *)textStruct);
		return status;
	}

	//  Print the table 
	printTextStruct(textStruct);
	zstructFree((void *)textStruct);

	//  When all done (near end of program), close the file
	zclose(ifltab);
	return 0;
}
void printTextStruct(zStructText *textStruct)
{
	int i, j;
	size_t len;
	size_t count;
	//  int cellLoc[5][10];
	int **cellLoc;
	int pos;

	//  Print the textString
	if ((textStruct->numberTextChars > 0) && textStruct->textString) {
		printf("\n%s\n", textStruct->textString);
	}

	//  Print labels (column headers)
	if ((textStruct->numberLabelChars > 0) && textStruct->labels) {
		count = 0;
		printf(" |");
		for (i = 0; i<textStruct->numberColumns; i++) {
			printf("%s|", &textStruct->labels[count]);
			len = strnlen_hec(&textStruct->labels[count], (size_t)textStruct->numberLabelChars - count);
			count += len + 1;
			if (count >= textStruct->numberLabelChars) break;
		}
		printf("\n");
	}

	//  Since the table is stored in a columnar fashion and we wish to print out 
	//  rows, we need to transpose the table.  Lots of ways to do this
	//  Usually won't need to transpose in most uses
	//  cellLoc[column][row]
	if ((textStruct->numberTableChars > 0) && textStruct->textTable) {
		count = 0;
		//  Allocate space for the transposition array
		cellLoc = (int **)calloc(textStruct->numberColumns, 4);
		for (i = 0; i<textStruct->numberColumns; i++) {
			cellLoc[i] = (int *)calloc(textStruct->numberRows, 4);
		}
		for (j = 0; j<textStruct->numberColumns; j++) {
			for (i = 0; i<textStruct->numberRows; i++) {
				cellLoc[j][i] = (int)count;
				len = strnlen_hec(&textStruct->textTable[count], (size_t)textStruct->numberTableChars - count);
				count += len + 1;
			}
		}
		//  Print text table
		count = 0;
		for (i = 0; i<textStruct->numberRows; i++) {
			printf(" |");
			for (j = 0; j<textStruct->numberColumns; j++) {
				pos = cellLoc[j][i];
				printf("%s|", &textStruct->textTable[pos]);
			}
			printf("\n");
		}
		for (i = 0; i<textStruct->numberColumns; i++) {
			free(cellLoc[i]);
		}
		free(cellLoc);
	}
	printf("\n");
}

void printPdStruct(zStructPairedData *pds, int startingCurve, int endingCurve,
	int startingRow, int endingRow)
{
	//  Print the table in a paired data struct.
	//  To print the entire table, set startingCurve, etc. to 0.
	//  To print a section of the table, set those accordingly
	//  Where "1" is the first row or curve (not "0").  ("0" is use default)
	//  If the struct's startingCurve, etc. is set, then the passed
	//  in start, etc., is ignored and the entire (portion) table is printed

	int numberRows;
	int numberColumns;
	int numberOrdinates;
	int row;
	int column;
	int firstRow;
	int firstColumn;
	int i, j;
	int pos;
	int len;


	firstColumn = 1;
	firstRow = 1;


	if (pds->endingCurve > 0) {
		startingCurve = 1;
		endingCurve = pds->endingCurve - pds->startingCurve + 1;
		firstColumn = pds->startingCurve;
	}
	if (pds->endingOrdinate > 0) {
		startingRow = 1;
		endingRow = pds->endingOrdinate - pds->startingOrdinate + 1;
		firstRow = pds->startingOrdinate;
		numberOrdinates = endingRow - startingRow + 1;
	}
	else {
		numberOrdinates = pds->numberOrdinates;
	}


	if (startingRow == 0) startingRow = 1;
	if (endingRow == 0) endingRow = pds->numberOrdinates;
	if (startingCurve == 0) startingCurve = 1;
	if (endingCurve == 0) endingCurve = pds->numberCurves;

	if (endingCurve > pds->numberCurves) {
		endingCurve = pds->numberCurves;
	}
	if (endingRow > pds->numberOrdinates) {
		endingRow = pds->numberOrdinates;
	}

	numberRows = endingRow - startingRow + 1;
	//  Column 0 is the ordinate array
	numberColumns = endingCurve - startingCurve + 1;

	//  Data is stored in a column format, but we will be printing rows
	//  Need to compute correct position within the array

	printf(" Row \t  Ordinate \t");
	for (j = 0; j<numberColumns; j++) {
		column = j + startingCurve - 1;
		printf(" Curve %d \t", (column + firstColumn));
	}
	printf("\n");

	if (pds->labelsLength > 1) {
		printf(" Labels \t\t");
		pos = 0;
		for (i = 0; i<numberColumns; i++) {
			len = (int)strlen(&pds->labels[pos]);
			printf(" %s\t", &pds->labels[pos]);
			pos += len + 1;
			if (pos >= pds->labelsLength)
				break;
		}
		printf("\n");
	}

	for (i = 0; i<numberRows; i++) {
		row = i + startingRow - 1;
		printf(" %d \t", (row + firstRow));
		if (pds->floatOrdinates) {
			printf(" %f \t", pds->floatOrdinates[row]);
		}
		else {
			printf(" %f \t", pds->doubleOrdinates[row]);
		}
		for (j = 0; j<numberColumns; j++) {
			column = j + startingCurve - 1;
			pos = (column * numberOrdinates) + row;
			if (pds->floatValues) {
				printf(" %f \t", pds->floatValues[pos]);
			}
			else {
				printf(" %f \t", pds->doubleValues[pos]);
			}
		}
		printf("\n");
	}
}

