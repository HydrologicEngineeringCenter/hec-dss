#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "hecdss.h"


static int require_eq(const char* what, int got, int exp) {
  if (got != exp) {
    fprintf(stderr, "%s failed: expected %d, got %d\n", what, exp, got);
    return 1;
  }
  return 0;
}

int main(int argc, char** argv) {

  const char* filename = "hec_dss_text_test.dss";
  unlink(filename);

  const char* path = "/UNIT/TEST/TEXT///FROM_MAIN/";
  const char* msg = "Hello DSS text record!";
  int failures = 0;
  int status = 0;
  dss_file* dss = NULL;

  status = hec_dss_open(filename, &dss);
  failures += require_eq("hec_dss_open", status, 0);

  if (!failures) {
    status = hec_dss_textStore(dss, path, msg, (int)strlen(msg));
    failures += require_eq("hec_dss_textStore", status, 0);
  }

  if (!failures) {
    char buf[256];
    memset(buf, 0, sizeof(buf));
    status = hec_dss_textRetrieve(dss, path, buf, (int)sizeof(buf));
    printf("\ntext: '%s'",buf);
    failures += require_eq("hec_dss_textRetrieve (ok)", status, 0);
    if (memcmp(buf, msg, strlen(msg)) != 0) {
      fprintf(stderr, "payload mismatch\n");
      failures++;
    }
  }

  if (!failures) {
    char small[4] = { 0 };
    status = hec_dss_textRetrieve(dss, path, small, (int)sizeof(small));
    printf("\nfirst 4: '%s'",small);
    failures += require_eq("hec_dss_textRetrieve (small buffer)", status, HEC_DSS_BUFFER_TOO_SMALL);
    if (memcmp(small, msg, sizeof(small)) != 0) {
      fprintf(stderr, "small buffer prefix mismatch\n");
      failures++;
    }
  }

  hec_dss_close(dss);

  if (failures == 0) {
    printf("Text tests PASS\n");
    return 0;
  }
  else {
    printf("Text tests FAIL (%d)\n", failures);
    return 1;
  }
}