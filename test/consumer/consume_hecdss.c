/* Consume libhecdss exactly as an external user would: include only the public
 * header (HECDSS_API resolves to dllimport / default visibility here, NOT dllexport)
 * and call the exported C API. This exercises the shared-library export / import-lib /
 * symbol-visibility and runtime-load path that the static-heclib tests never touch --
 * i.e. it tests the artifact we actually ship, through its public boundary. */
#include <stdio.h>
#include "hecdss.h"

int main(void) {
    const char *v = hec_dss_api_version();
    if (v == NULL) {
        fprintf(stderr, "FAIL: hec_dss_api_version() returned NULL\n");
        return 1;
    }
    printf("linked libhecdss; hec_dss_api_version() = %s\n", v);

    /* A second exported symbol, to confirm more than one entry resolves. */
    if (hec_dss_CONSTANT_MAX_PATH_SIZE() <= 0) {
        fprintf(stderr, "FAIL: hec_dss_CONSTANT_MAX_PATH_SIZE() <= 0\n");
        return 1;
    }
    return 0;
}
