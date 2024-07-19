#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "TestDssC.h"


int writeText(const char* filename, const char* path, char* text) {
  long long ifltab[250] = { 0 };
  int status = hec_dss_zopen(ifltab, filename);
  if (status != 0)
    return status;

  zStructText* t = zstructTextStringNew(path, text);

  status = ztextStore(ifltab, t);
  if (status != 0) {
    printf("\nError saving '%s' to %s under path: %s", text, filename, path);
  }
  else {
    printf("\n saved '%s' to %s under path: %s", text, filename, path);
  }

  zstructFree(t);
  return status;

}

/// <summary>
/// creates a new DSS file (existing file will be overwriten)
/// </summary>
int createDssFile(const char* filename, int version) {

  remove(filename);
  long long ifltab[250] = { 0 };
  int status = 0;
  if (version == 7) {
    status = hec_dss_zopen(ifltab, filename);
  } else {
    printf("Only version 7 supported.\n");
    return -1;
  }  
  if (status != 0)
    return status;

  status = zclose(ifltab);
  return status;
}

int readText(const char* filename, const char* path, const char* expectedText)
{
  long long ifltab[250] = { 0 };
  int status = hec_dss_zopen(ifltab, filename);
  if (status != 0)
    return status;

  zStructText* t2 = zstructTextNew(path);
  status = ztextRetrieve(ifltab, t2);
  if (status != 0) {
    printf("\nError reading text record: %s\n", path);
    return status;
  }
  int compare = strcmp(expectedText, t2->textString);
  if (compare == 0) {
    printf("Read '%s'\n", t2->textString);
  } 
  else {
    printf("Error expected '%s', found '%s'\n", expectedText, t2->textString);
  }
  status = zclose(ifltab);
  zstructFree(t2);
  return compare;
}


  int readExistingFiles(const char* path, const char* test_data) {

    int status = readText("dss7-windows-text.dss", path, test_data);
    if (status != 0)
      return status;

    status = readText("dss7-solaris-text.dss", path, test_data);
    if (status != 0)
      return status;

    status = readText("dss7-linux-text.dss", path, test_data);
    if (status != 0)
      return status;

    return status;
  }

  int testText()
  {
    int status =0;
    char* test_data = "ABCD1234abcd1234";
    const char* path = "//////TEXT/";

    printf("\n----- Reading Existing Files --------------\n");
    status = readExistingFiles(path, test_data);
    if (status != 0)
      return status;

    const char* dssfile7 = "temp-dss-7.dss";



    createDssFile(dssfile7, 7);
    writeText(dssfile7, path, test_data);
    status = readText(dssfile7, path, test_data);
    if (status != 0)
      return status;

    return 0;
  }
