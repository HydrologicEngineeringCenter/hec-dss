#include "export.h"
#include "heclib.h"
#include "dss_file.h"


int exportTimeSeries(dss_file* dss,
  const char* path,
  const char* outputFile,
  const char* startDate, const char* startTime,
  const char* endDate, const char* endTime) {
  
  char cdate[13], ctime[10];
  zStructTimeSeries* tss = zstructTsNew(path);
  int status = ztsRetrieve(dss->ifltab, tss, 0, 2, 0);
  if (status != STATUS_OKAY) return status;

  FILE* f = fopen(outputFile, "w");
  fprintf(f, "Begin Header\n");
  fprintf(f, "path=%s\n",path);
  fprintf(f, "dataType=%d   #TO DO . 1=, 2=", tss->dataType);
  fprintf(f, "type=%s", tss->type);
  fprintf(f, "units=%s\n", tss->units);
  fprintf(f, "numberValues=%d\n", tss->numberValues);
  fprintf(f, "End Header\n");
  for (int i = 0; i < tss->numberValues; i++) {
    getDateAndTime(tss->times[i], tss->timeGranularitySeconds, tss->julianBaseDate,
      cdate, sizeof(cdate), ctime, sizeof(ctime));
    fprintf(f,"%s %s, %f\n", cdate, ctime, tss->doubleValues[i]);
  }
  fclose(f);
  zstructFree(tss);
  return 0;
}

int exportGrid() {

 /* zStructSpatialGrid* gridStruct = zstructSpatialGridNew(path);


  int status = zspatialGridRetrieve(ifltab, gridStruct, !metaDataOnly);

  printGridStruct(ifltab, -1, gridStruct);
  if (gridStruct->_data)
  {
    float* x = (float*)gridStruct->_data;
    size_t size = gridStruct->_numberOfCellsX * gridStruct->_numberOfCellsY;
    printf("---begin GRID DATA---\n");
    for (size_t i = 0; i < size; i++)
    {
      printf("%f ", x[i]);
    }
    printf("\n--- end GRID DATA---\n");
    zstructFree(gridStruct);
  }*/
  return -1;
}