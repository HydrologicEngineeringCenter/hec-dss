/*
 *  Regression test for the ztsInternalHeaderUnpack stack-buffer-overrun.
 *
 *  Builds a synthetic on-disk "internal header" and calls the parser directly.
 *
 *  Run under AddressSanitizer:
 *      cmake -B build-asan -DHECDSS_SANITIZE=ON && cmake --build build-asan
 *      ctest --test-dir build-asan -L hardening
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib.h"

#define HEADER_INTS  1024      /*  1024 ints = 4096 bytes vs the parser's 200-byte stack buffer */
#define TMPFILE      "ts_internal_header_unpack.dss"
#define TS_PATHNAME  "/Hardening/Synthetic/Flow//1Hour/Test/"
#define UNITS_DATA_INDEX  (INT_HEAD_units + 1)

/*  expected_* = NULL  -> assert the field is NULL after parse.
 *  expected_* = SKIP  -> don't check.
 *  otherwise          -> strcmp. */
#define SKIP ((const char *)-1)

struct test_case {
    const char *name;
    int         header_int_count;
    int         bad;
    const char *set_units,      *set_type,      *set_tz;
    const char *expected_units, *expected_type, *expected_tz;
};

static int failures;

/*  fill with non-NUL bytes (ZDSSZDSSZDSS...) */
static void make_bad_header(int *header, int header_count) {
    int hostile;
    memcpy(&hostile, "ZDSS", 4);
    for (int i = 0; i < header_count; i++) {
        header[i] = hostile;
    }
    header[0] = header[1] = header[2] = header[3] = header[INT_HEAD_units] = 0;
}

/*  DSS packs each units / type / timeZone string into an 8-byte word. */
static void pack_word(char *destination, int *ipos, const char *value) {
    int len = (int)strlen(value);
    for (int i = 0; i < 7; i++)
        destination[*ipos + i] = (i < len) ? value[i] : ' ';
    *ipos += 7;
    destination[*ipos] = '\0';
    (*ipos)++;
}

static void build_valid_header(int *header, const struct test_case *c) {
    memset(header, 0, HEADER_INTS * sizeof(int));
    if (c->set_units || c->set_type || c->set_tz) {
        char packed[64] = {0};
        int ipos = 0;
        pack_word(packed, &ipos, c->set_units ? c->set_units : "");
        pack_word(packed, &ipos, c->set_type  ? c->set_type  : "");
        pack_word(packed, &ipos, c->set_tz    ? c->set_tz    : "");
        memcpy(&header[UNITS_DATA_INDEX], packed, ipos);
    }
}

// Returns 1 if the values match or the check is skipped, and 0 on mismatch.
static int expect_str(const char *field, const char *got, const char *expected) {
    if (expected == SKIP) return 1;
    if (!expected) {
        if (!got) return 1;
        printf("  FAIL: %s = \"%s\" (expected NULL)\n", field, got);
        return 0;
    }
    if (got && strcmp(got, expected) == 0) return 1;
    printf("  FAIL: %s = \"%s\" (expected \"%s\")\n", field, got ? got : "(null)", expected);
    return 0;
}

static void run(const struct test_case *c, long long *ifltab) {
    int header[HEADER_INTS];
    if (c->bad) make_bad_header(header, HEADER_INTS); 
    else        build_valid_header(header, c);

    zStructTimeSeries *tss = zstructTsNewTimes(TS_PATHNAME, "01Jan2020", "0000", "01Jan2020", "0000");
    ztsProcessTimes(ifltab, tss, 0);

    int status = ztsInternalHeaderUnpack(tss, header, c->header_int_count);
    int ok = (status == STATUS_OKAY)
          && expect_str("units",    tss->units,        c->expected_units)
          && expect_str("type",     tss->type,         c->expected_type)
          && expect_str("timeZone", tss->timeZoneName, c->expected_tz);
    /*  Post-fix: units is capped at 199 chars by the stack buffer's terminator.
     *  Pre-fix: strlen walks past the buffer, units ends up longer. */
    if (c->bad && tss->units && (int)strlen(tss->units) > 199) {
        printf("  FAIL: units length %d exceeds parser buffer bound (199)\n",
               (int)strlen(tss->units));
        ok = 0;
    }
    printf("%s: %s\n", ok ? "PASS" : "FAIL", c->name);
    if (!ok) failures++;

    zstructFree(tss);
}

int main(void) {
    static const struct test_case cases[] = {
        /*  name           int_count       bad  set: units/type/tz       expected: units/type/tz   */
        { "valid",         26,             0,   "CFS",  "INST",  "UTC",  "CFS",  "INST",  "UTC"  },
        { "units only",    26,             0,   "CFS",  NULL,    NULL,   "CFS",  NULL,    NULL   },
        { "malformed",     HEADER_INTS,    1,   NULL,   NULL,    NULL,   SKIP,   NULL,    NULL   },
    };

    remove(TMPFILE);
    long long ifltab[250];
    memset(ifltab, 0, sizeof(ifltab));
    if (hec_dss_zopen(ifltab, TMPFILE) != STATUS_OKAY) {
        printf("FAIL: zopen %s\n", TMPFILE);
        return 1;
    }

    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++)
        run(&cases[i], ifltab);

    zclose(ifltab);
    remove(TMPFILE);

    printf("\n%s\n", failures ? "FAILED" : "All checks passed.");
    return failures ? 1 : 0;
}
