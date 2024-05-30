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
  }
  else if (version == 6) {
    status = zopen6(ifltab, filename);
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

int readVersion6Text() {
  const char* path = "//INPUTGVSTRING///STRING/TESTALT1--0:TESTALT1-RESSIM-DEFAULT/";
  const char* expectedText = "\nRandomInputString\r";
  return readText("TestAlt1.dss", path,expectedText);

}

/*
* ResSim global variables use text records.
* conversion to DSS version 7 did not work for text records.
*/
int testConvert6To7_gv_issue() {
  const char* dssFilename6 = "TestAlt1.dss";
  const char* dssFilename7 = "TestAlt1-converted-to-7_temp.dss";
  remove(dssFilename7);
  int status = zconvertVersion(dssFilename6, dssFilename7);
  const char* path = "//INPUTSCALARTIMEDATE///DATE AND TIME/TESTALT1-RESSIM-DEFAULT/";
  status = readText(dssFilename7, path,"\n08Jun2023 1400\r");
  return status;

}

  int test_convert_to_dss7_from_scratch(){
  long long ifltab6[250] = { 0 };
  long long ifltab7[250] = { 0 };

  const char* path = "//INPUTGVSTRING///STRING/TESTALT1--0:TESTALT1-RESSIM-DEFAULT/";
  char* data = "\n\n\nabc def.";
  zStructText* t = zstructTextStringNew(path,data);

  const char* dssFilename6 = "test_text_simple_gv_ressim6.dss";
  const char* dssFilename7 = "test_text_simple_gv_ressim7.dss";
  const char* dssFilename7a = "test_text_simple_gv_ressim7_file_convert.dss";
  remove(dssFilename6);
  remove(dssFilename7);
  remove(dssFilename7a);

  int status = zopen6(ifltab6, dssFilename6);
  if (status != 0)
    return status;

  status = hec_dss_zopen(ifltab7, dssFilename7);
  if (status != 0)
    return status;

  ztextStore(ifltab6, t);
  zstructFree(t);
  zcopyRecord(ifltab6, ifltab7, path, path);

  zclose(ifltab6);
  zclose(ifltab7);

  status = zconvertVersion(dssFilename6, dssFilename7a);
  if (status != 0) {
    printf("\nError converting file\n");
    return status;
  }

  status = readText(dssFilename6, path, data);
  if (status != 0) {
    printf("\nError reading text from dss6\n");
    return status;
  }
  status = readText(dssFilename7, path, data);
  if (status != 0) {
    printf("\nError reading text from dss7\n");
    return status;
  }
  status = readText(dssFilename7a, path, data);
  if (status != 0) {
    printf("\nError reading text from dss7 converted file\n");
    return status;
  }

  return 0;
}

  int readExistingFiles(const char* path, const char* test_data) {

    int status = readText("dss6-windows-text.dss",path , test_data);
    if (status != 0)
      return status;

    status = readText("dss6-solaris-text.dss", path, test_data);
    if (status != 0)
      return status;

    status = readText("dss6-linux-text.dss", path, test_data);
    if (status != 0)
      return status;

    status = readText("dss7-windows-text.dss", path, test_data);
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

    const char* dssfile6 = "temp-dss-6.dss";
    const char* dssfile7 = "temp-dss-7.dss";

    createDssFile(dssfile6, 6);
    writeText(dssfile6,path ,test_data);
    status = readText(dssfile6, path,test_data);
    if (status != 0)
      return status;

    createDssFile(dssfile7, 7);
    writeText(dssfile7, path, test_data);
    status = readText(dssfile7, path, test_data);
    if (status != 0)
      return status;
    

    status = readVersion6Text();
    if (status != 0)
      return status;

    status = testConvert6To7_gv_issue();
    if (status != 0)
      return status;

    status = test_convert_to_dss7_from_scratch();
    if (status != 0)
      return status;

    return 0;
  }
