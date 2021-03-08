#include "stdio.h"
#include "string.h"

#include "zdssKeys.h"
#include "heclib.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "TestDssC.h"
#include "zStructAllocation.h"

#include "zStructSpatialTin.h"

char getaCharacter(int i) {
	int j;

	j = i/26;
	i = i - (j * 26);

	i += 97;
	if ((i >= 97) && (i <= 122)) {
		return i;
	}
	return 97;
}

int getStr(char *str, int pos) {
	int i, j;

	i = pos /12;
	j = pos - (i * 12);
	if (j < 0) j = -j;
	if (j < 5) j += 5;
	for (i=0; i<j; i++) {
		str[i] = getaCharacter(pos+i);
	}
	str[j+1] = '\0';
	return j+1;
}

char* getLabelPtr(char* labels, int labelIndex, int maxIndex){
	int i = 0, j = 0;
	char* thePtr = labels;
	while (i < maxIndex){
		if (j == labelIndex) return thePtr;
		if (*thePtr == '\0') j++;
		i++;
		thePtr++;
	}
	return NULL;
}

int testTin(long long *ifltab)
{

	int DEBUG=0;
	int status=0;
	int i;
	int count;
	char *labels;
	int* connectTo;
	zStructSpatialTin *tinStructStore;
	zStructSpatialTin *tinStructRetrieve;
	
	// Building a SHG-type grid as a TIN.
	// Origin is at 283,000 m east, 1,785,000 m north or center of cell (141, 892)
	float xMin = 283000.0, yMin = 1785000.0;
    // 2000m spacing for 40 rows and 50 columns
	int row = 0, column = 0, nrows = 40, ncols = 50, maxRowIndex = 39, maxColIndex = 49;
	float cellsize = 2000.0;
	int nconns = 0, nedges = 0, connIndex = 0;
	// maximum data value = 5 mm precip
	float maxVal = 5;
	int done = 0, nodeIndex = -1;
	char* labelForShow;


	if (zgetVersion(ifltab) != 7) {		
		return 0;
	}
	return 0;   //  Needs to be updated by Tom E.
	//zset("mlvl", "", 10);
	//_unlink("a6.dss");
	//status = zconvertVersion("sample7.dss", "a6.dss");
	//return status;

	tinStructStore = zstructSpatialTinNew("/a/b/c/01jan2001:1200/01jan2001:1300/f/");

	//   Gen data
	tinStructStore->numberPoints = 2000;
	tinStructStore->slendernessRatio = 0.0;
	tinStructStore->SRSType = 0; // WKT string to define coordinate system

	tinStructStore->xCoordinate =     (float *)calloc(tinStructStore->numberPoints, 4);
	tinStructStore->yCoordinate     = (float *)calloc(tinStructStore->numberPoints, 4);
	tinStructStore->value           = (float *)calloc(tinStructStore->numberPoints, 4);
	tinStructStore->numberConnections = (int *)calloc(tinStructStore->numberPoints, 4);

	// temporary array to hold the connections while they're being figured out.
	connectTo = (int *)calloc((tinStructStore->numberPoints) * 6, 4); // more than we need, by a bit, actually

	tinStructStore->allocated[zSTRUCT_TIN_xCoordinate] = 1;
	tinStructStore->allocated[zSTRUCT_TIN_yCoordinate] = 1;
	tinStructStore->allocated[zSTRUCT_TIN_value] = 1;
	tinStructStore->allocated[zSTRUCT_TIN_pointType] = 1;
	tinStructStore->allocated[zSTRUCT_TIN_numberConnections] = 1;
/*
	
	for (i=0; i<tinStructStore->numberPoints; i++) {
		row = i / ncols;
		column = i % ncols;

		// easy stuff (x, y, value) first
		tinStructStore->xCoordinate[i] = (float)(xMin + column * cellsize);
		tinStructStore->yCoordinate[i] = (float)(yMin + row * cellsize);
		tinStructStore->value[i] = maxVal * (1 - (float)abs(nrows / 2 - row) / (float)(nrows / 2));
		tinStructStore->pointType[i] = 2; // using 2 to indicate a cell in a grid

		// trickier topological stuff
		tinStructStore->connection[i] = connIndex;
		if (column != 0 && row != 0 && row != maxRowIndex && column != maxColIndex){
			// this is an interior point in the grid
			tinStructStore->numberConnections[i] = 6;
			nedges += 6;
			connectTo[connIndex++] = i - ncols - 1; // node to southwest (down one and over one)
			connectTo[connIndex++] = i - ncols; // node to south
			connectTo[connIndex++] = i - 1; // node to west
			connectTo[connIndex++] = i + 1; // node to east
			connectTo[connIndex++] = i + ncols; // node to north
			connectTo[connIndex++] = i + ncols + 1; // node to northeast (up one and over one)
		}
		else if (row == 0){
			// south edge of grid
			if (column == 0){
				//southwest corner
				tinStructStore->numberConnections[i] = 3;
				nedges += 3;
				connectTo[connIndex++] = i + 1; // node to east
				connectTo[connIndex++] = i + ncols; // node to north
				connectTo[connIndex++] = i + ncols + 1; // node to northeast (up one and over one)
			}
			else if (column == maxColIndex){
				//southeast corner
				tinStructStore->numberConnections[i] = 2;
				nedges += 2;
				connectTo[connIndex++] = i - 1; // node to west
				connectTo[connIndex++] = i + ncols; // node to north
			}
			else{
				//not on corner
				tinStructStore->numberConnections[i] = 4;
				nedges += 4;
				connectTo[connIndex++] = i - 1; // node to west
				connectTo[connIndex++] = i + 1; // node to east
				connectTo[connIndex++] = i + ncols; // node to north
				connectTo[connIndex++] = i + ncols + 1; // node to northeast (up one and over one)
			}
		}
		else if (row == maxRowIndex){
			// north edge of grid
			if (column == 0){
				//northwest corner
				tinStructStore->numberConnections[i] = 2;
				nedges += 2;
				connectTo[connIndex++] = i - ncols; // node to south
				connectTo[connIndex++] = i + 1; // node to north
			}
			else if (column == maxColIndex){
				//northeast corner
				tinStructStore->numberConnections[i] = 3;
				nedges += 3;
				connectTo[connIndex++] = i - ncols - 1; // node to southwest (down one and over one)
				connectTo[connIndex++] = i - ncols; // node to south
				connectTo[connIndex++] = i - 1; // node to west
			}
			else{
				//not on corner
				tinStructStore->numberConnections[i] = 4;
				nedges += 4;
				connectTo[connIndex++] = i - ncols - 1; // node to southwest (down one and over one)
				connectTo[connIndex++] = i - ncols; // node to south
				connectTo[connIndex++] = i - 1; // node to west
				connectTo[connIndex++] = i + 1; // node to east
			}
		}
		else if (column == 0){
			// west edge of grid, excluding corners
			tinStructStore->numberConnections[i] = 4;
			nedges += 4;
			connectTo[connIndex++] = i - ncols; // node to south
			connectTo[connIndex++] = i + 1; // node to east
			connectTo[connIndex++] = i + ncols; // node to north
			connectTo[connIndex++] = i + ncols + 1; // node to northeast (up one and over one)
		}
		else {
			// all that's left is the east edge, excluding the two corners
			tinStructStore->numberConnections[i] = 4;
			nedges += 4;
			connectTo[connIndex++] = i - ncols - 1; // node to southwest (down one and over one)
			connectTo[connIndex++] = i - ncols; // node to south
			connectTo[connIndex++] = i - 1; // node to west
			connectTo[connIndex++] = i + ncols; // node to north
		}

		// tinStructStore->numberConnections[i] = i + 1000;
		// tinStructStore->connectionIndex[i]   = i + 10000;
		//tinStructStore->labelIndex[i]        = i;
		// tinStructStore->connectTo[i]         = i + 1000000;
	}

	tinStructStore->connection = (int *)calloc(nedges, sizeof(int));
	for (i = 0; i < nedges; i++){
		tinStructStore->connection[i] = connectTo[i];
	}
	free(connectTo);
	//tinStructStore->connectTableNumber = nedges;
	//tinStructStore->allocated[zSTRUCT_TIN_connectTo] = 1;

	tinStructStore->SpatialReferenceSystem = mallocAndCopy("PROJCS[\"USA_Contiguous_Albers_Equal_Area_Conic_USGS_version\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\",0.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-96.0],PARAMETER[\"Standard_Parallel_1\",29.5],PARAMETER[\"Standard_Parallel_2\",45.5],PARAMETER[\"Latitude_Of_Origin\",23.0],UNIT[\"Meter\",1.0]]");
	tinStructStore->SRSName = mallocAndCopy("CONUS_ALBERS_USGS");
	tinStructStore->SRSUnits = mallocAndCopy("Meters");
	tinStructStore->units = mallocAndCopy("mm");
	tinStructStore->type = mallocAndCopy("PER-CUM");
	tinStructStore->timeZoneName = mallocAndCopy("UTC");

	tinStructStore->allocated[zSTRUCT_TIN_SpatialReferenceSystem] = 1;
	tinStructStore->allocated[zSTRUCT_TIN_SRSName] = 1;
	tinStructStore->allocated[zSTRUCT_TIN_SRSUnits] = 1;
	tinStructStore->allocated[zSTRUCT_TIN_units] = 1;
	tinStructStore->allocated[zSTRUCT_TIN_type] = 1;
	tinStructStore->allocated[zSTRUCT_TIN_timeZoneName] = 1;

	labels = (char *)calloc(tinStructStore->numberPoints * 16, sizeof(char));
	count = 0;
	for (i=0; i<tinStructStore->numberPoints; i++) {
		count += getStr(&labels[count], i);
	}
	tinStructStore->labelLen = count;
	tinStructStore->label = (char*)calloc(count, sizeof(char));
	for (i = 0; i < count; i++){
		//tinStructStore->pointLabel[i] = '\0';
		tinStructStore->label[i] = labels[i];
	}
	// free(labels);
	tinStructStore->allocated[zSTRUCT_TIN_label] = 1;

	
	printf("\nEnter an index number (non-numerical value exits): ");
	done = 10;
	nodeIndex = 123;
	while (!done){		
			printf(" Node %d is at (%4.1f, %4.1f) and has a value of %3.1f\n", nodeIndex, tinStructStore->xCoordinate[nodeIndex],
				tinStructStore->yCoordinate[nodeIndex], tinStructStore->value[nodeIndex]);
			printf(" Its first connection is located at index %d.\n", tinStructStore->connection[nodeIndex]);
			labelForShow = getLabelPtr(tinStructStore->label, tinStructStore->label[nodeIndex], tinStructStore->labelLen);
			if (labelForShow != NULL){
				printf(" Its label is: %s\n", labelForShow);
			}
			printf(" It connects to %d nodes: ", tinStructStore->numberConnections[nodeIndex]);
			for (i = 0; i < tinStructStore->numberConnections[nodeIndex] - 1; i++){
				printf(" %d, ", tinStructStore->connectTo[tinStructStore->connectionIndex[nodeIndex] + i]);
			}
			printf("and %d.\n", tinStructStore->connectTo[tinStructStore->connectionIndex[nodeIndex] + i]);
			printf("\nNext: ");
			done--;
			nodeIndex++;
		
	}
	//fflush(stdin);

	// zstructFree(tinStructStore);
	// return;

	status = zspatialTinStore(ifltab, tinStructStore);

	tinStructRetrieve = zstructSpatialTinNew("/a/b/c/01jan2001:1200/01jan2001:1300/f/");
	status = zspatialTinRetrieve(ifltab, tinStructRetrieve, 1);

	done = 10;
	nodeIndex = 123;
	printf("\nEnter an index number (non-numerical value exits): ");
	while (!done){
			printf(" Node %d is at (%4.1f, %4.1f) and has a value of %3.1f\n", nodeIndex, tinStructRetrieve->xCoordinate[nodeIndex],
				tinStructRetrieve->yCoordinate[nodeIndex], tinStructRetrieve->value[nodeIndex]);
			labelForShow = getLabelPtr(tinStructRetrieve->pointLabel, tinStructRetrieve->labelIndex[nodeIndex], tinStructRetrieve->pointLabelLen);
			if (labelForShow != NULL){
				printf(" Its label is: %s\n", labelForShow);
			}
			printf(" Its first connection is located at index %d.\n", tinStructRetrieve->connectionIndex[nodeIndex]);
			printf(" It connects to %d nodes: ", tinStructRetrieve->numberConnections[nodeIndex]);
			for (i = 0; i < tinStructRetrieve->numberConnections[nodeIndex] - 1; i++){
				printf(" %d, ", tinStructRetrieve->connectTo[tinStructRetrieve->connectionIndex[nodeIndex] + i]);
			}
			printf("and %d.\n", tinStructRetrieve->connectTo[tinStructRetrieve->connectionIndex[nodeIndex] + i]);
			printf("\nNext: ");
			done--;
			nodeIndex++;
	}

	free(labels);
	zstructFree(tinStructStore);
	zstructFree(tinStructRetrieve);
*/	
	return status;
}
