#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "TestDssC.h"


int readText(const char* filename, const char* path, const char* expectedText)
{
  long long ifltab[250] = { 0 };
  int status = hec_dss_zopen(ifltab, filename);
  if (status != 0)
    return status;

  int version = zgetVersion(ifltab);

  zStructText* t2 = zstructTextNew(path);
  status = ztextRetrieve(ifltab, t2);
  if (status != 0) {
    printf("\nError reading text record: %s", path);
    return status;
  }
  int compare = strcmp(expectedText, t2->textString);
  if (compare != 0) {
    printf("Error expected '%s', found '%s'",expectedText, t2->textString);
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
    printf("\nError converting file");
    return status;
  }

  status = readText(dssFilename6, path, data);
  if (status != 0) {
    printf("\nError reading text from dss6");
    return status;
  }
  status = readText(dssFilename7, path, data);
  if (status != 0) {
    printf("\nError reading text from dss7");
    return status;
  }
  status = readText(dssFilename7a, path, data);
  if (status != 0) {
    printf("\nError reading text from dss7 converted file");
    return status;
  }

  return 0;
}

  int testText()
  {
    int status = 0;

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
