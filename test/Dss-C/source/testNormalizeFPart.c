#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <hecdssInternal.h>

typedef struct normalize_f_part_test_s {
	char* fPart;
	int   expected_status;
	char* expected_normalized;
} normalize_f_part_test;

int test_normalize_f_part() {
	normalize_f_part_test tests[] = {
		//-----------------------------------------//
		// Errors that indicate non-tagged F parts //
		//-----------------------------------------//
		{"normal_f_part",    1, NULL}, // Test  1 : normal F part with no tag
		{"normal|f_part",    1, NULL}, // Test  2 : normal F part with no tag
		{"normal:f_part",    2, NULL}, // Test  3 : normal F part with no tag
		{"c:normal|f_part|", 3, NULL}, // Test  4 : normal F part with no tag (would parse as tagged F part without second pipe char)
		{NULL,               4, NULL}, // Test  5 : empty F Part
		//---------------------------------------------------//
		// Errors that indicate F parts with invalid tagging //
		//---------------------------------------------------//
		{"C:000001|F:20220228-1200|V:20220228-143000|N:ResSim Only|R:----E0|User String",   11, NULL}, // Test  6 : Invalid tag character
		{"C:000001|T:20220228-1200|T:20220228-143000|N:ResSim Only|R:----E0|User String",   12, NULL}, // Test  7 : Duplicate tag character
		{"C:0000001|T:20220228-1200|V:20220228-143000|N:ResSim Only|R:----E0|User String",  13, NULL}, // Test  8 : Collection value too long
		{"C:000001|T:20220228-120000|V:20220228-143000|N:ResSim Only|R:----E0|User String", 14, NULL}, // Test  9 : Time of forecast includes seconds
		{"C:000001|T:20220229-1200|V:20220228-143000|N:ResSim Only|R:----E0|User String",   14, NULL}, // Test 10 : Time of forecast includes invalid date
		{"C:000001|T:20220228-1200|V:20220228-1430|N:ResSim Only|R:----E0|User String",     15, NULL}, // Test 11 : Version time doesn't include seconds
		{"C:000001|T:20220228-1200|V:20220228-243000|N:ResSim Only|R:----E0|User String",   15, NULL}, // Test 12 : Version time includes invalid time
		{"C:000001|T:20220228-1200|V:20220228-143000|N:ResSim Only|R:---E0|User String",    16, NULL}, // Test 13 : Run string has odd number of characters
		{"C:000001|T:20220228-1200|V:20220228-143000|N:ResSim Only|R:----00|User String",   16, NULL}, // Test 14 : Run model alternative is not alphabetic
		{"C:000001|T:20220228-1200|V:20220228-143000|N:ResSim Only|R:----EX|User String",   16, NULL}, // Test 15 : Run model trial number is not numeric
		//--------------------------//
		// 5 tags taken 5 at a time //
		//--------------------------//
		// canonical order
		{"c:000001|t:20220228-1200|n:ResSim Only|v:20200229-143000|r:----E0|User String",    0, "C:000001|T:20220228-1200|N:ResSim Only|V:20200229-143000|R:----E0|User String"}, // Test 16 : C,T,N,V,R
		// reverse canonical order
		{"r:----E0|v:20200229-143000|n:ResSim Only|t:20220228-1200|c:000001|User String",    0, "C:000001|T:20220228-1200|N:ResSim Only|V:20200229-143000|R:----E0|User String"}, // Test 17 : C,T,N,V,R
		//--------------------------//
		// 5 tags taken 4 at a time //
		//--------------------------//
		// canonical order
		{"c:000001|t:20220228-1200|n:ResSim Only|v:20220228-143000|User String", 0, "C:000001|T:20220228-1200|N:ResSim Only|V:20220228-143000|User String"}, // Test 18 : C,T,N,V
		{"c:000001|t:20220228-1200|n:ResSim Only|r:----E0|User String",          0, "C:000001|T:20220228-1200|N:ResSim Only|R:----E0|User String"},          // Test 19 : C,T,N,R
		{"c:000001|t:20220228-1200|v:20220228-143000|r:----E0|User String",      0, "C:000001|T:20220228-1200|V:20220228-143000|R:----E0|User String"},      // Test 20 : C,T,V,R
		{"c:000001|n:ResSim Only|v:20220228-143000|r:----E0|User String",        0, "C:000001|N:ResSim Only|V:20220228-143000|R:----E0|User String"},        // Test 21 : C,N,V,R
		{"t:20220228-1200|n:ResSim Only|v:20220228-143000|r:----E0|User String", 0, "T:20220228-1200|N:ResSim Only|V:20220228-143000|R:----E0|User String"}, // Test 22 : T,N,V,R
		// reverse canonical order
		{"v:20220228-143000|n:ResSim Only|t:20220228-1200|c:000001|User String", 0, "C:000001|T:20220228-1200|N:ResSim Only|V:20220228-143000|User String"}, // Test 23 : C,T,N,V
		{"r:----E0|n:ResSim Only|t:20220228-1200|c:000001|User String",          0, "C:000001|T:20220228-1200|N:ResSim Only|R:----E0|User String"},          // Test 24 : C,T,N,R
		{"r:----E0|v:20220228-143000|t:20220228-1200|c:000001|User String",      0, "C:000001|T:20220228-1200|V:20220228-143000|R:----E0|User String"},      // Test 25 : C,T,V,R
		{"r:----E0|v:20220228-143000|n:ResSim Only|c:000001|User String",        0, "C:000001|N:ResSim Only|V:20220228-143000|R:----E0|User String"},        // Test 26 : C,N,V,R
		{"r:----E0|v:20220228-143000|n:ResSim Only|t:20220228-1200|User String", 0, "T:20220228-1200|N:ResSim Only|V:20220228-143000|R:----E0|User String"}, // Test 27 : T,N,V,R
		//--------------------------//
		// 5 tags taken 3 at a time //
		//--------------------------//
		// canonical order
		{"c:000001|t:20220228-1200|n:ResSim Only|User String",          0, "C:000001|T:20220228-1200|N:ResSim Only|User String"},          // Test 28 : C,T,N
		{"c:000001|t:20220228-1200|v:20220228-143000|User String",      0, "C:000001|T:20220228-1200|V:20220228-143000|User String"},      // Test 29 : C,T,V
		{"c:000001|t:20220228-1200|r:----E0|User String",               0, "C:000001|T:20220228-1200|R:----E0|User String"},               // Test 30 : C,T,R
		{"c:000001|n:ResSim Only|v:20220228-143000|User String",        0, "C:000001|N:ResSim Only|V:20220228-143000|User String"},        // Test 31 : C,N,V
		{"c:000001|n:ResSim Only|r:----E0|User String",                 0, "C:000001|N:ResSim Only|R:----E0|User String"},                 // Test 32 : C,N,R
		{"c:000001|v:20220228-143000|r:----E0|User String",             0, "C:000001|V:20220228-143000|R:----E0|User String"},             // Test 33 : C,V,R
		{"t:20220228-1200|n:ResSim Only|v:20220228-143000|User String", 0, "T:20220228-1200|N:ResSim Only|V:20220228-143000|User String"}, // Test 34 : T,N,V
		{"t:20220228-1200|n:ResSim Only|r:----E0|User String",          0, "T:20220228-1200|N:ResSim Only|R:----E0|User String"},          // Test 35 : T,N,R
		{"t:20220228-1200|v:20220228-143000|r:----E0|User String",      0, "T:20220228-1200|V:20220228-143000|R:----E0|User String"},      // Test 36 : T,V,R
		{"n:ResSim Only|v:20220228-143000|r:----E0|User String",        0, "N:ResSim Only|V:20220228-143000|R:----E0|User String"},        // Test 37 : N,V,R
		// reverse canonical order
		{"n:ResSim Only|t:20220228-1200|c:000001|User String",          0, "C:000001|T:20220228-1200|N:ResSim Only|User String"},          // Test 38 : C,T,N
		{"v:20220228-143000|t:20220228-1200|c:000001|User String",      0, "C:000001|T:20220228-1200|V:20220228-143000|User String"},      // Test 39 : C,T,V
		{"r:----E0|t:20220228-1200|c:000001|User String",               0, "C:000001|T:20220228-1200|R:----E0|User String"},               // Test 40 : C,T,R
		{"v:20220228-143000|n:ResSim Only|c:000001|User String",        0, "C:000001|N:ResSim Only|V:20220228-143000|User String"},        // Test 41 : C,N,V
		{"r:----E0|n:ResSim Only|c:000001|User String",                 0, "C:000001|N:ResSim Only|R:----E0|User String"},                 // Test 42 : C,N,R
		{"r:----E0|v:20220228-143000|c:000001|User String",             0, "C:000001|V:20220228-143000|R:----E0|User String"},             // Test 43 : C,V,R
		{"v:20220228-143000|n:ResSim Only|t:20220228-1200|User String", 0, "T:20220228-1200|N:ResSim Only|V:20220228-143000|User String"}, // Test 44 : T,N,V
		{"r:----E0|n:ResSim Only|t:20220228-1200|User String",          0, "T:20220228-1200|N:ResSim Only|R:----E0|User String"},          // Test 45 : T,N,R
		{"r:----E0|v:20220228-143000|t:20220228-1200|User String",      0, "T:20220228-1200|V:20220228-143000|R:----E0|User String"},      // Test 46 : T,V,R
		{"r:----E0|v:20220228-143000|n:ResSim Only|User String",        0, "N:ResSim Only|V:20220228-143000|R:----E0|User String"},        // Test 47 : N,V,R
		//--------------------------//
		// 5 tags taken 2 at a time //
		//--------------------------//
		// canonical order
		{"c:000001|t:20220228-1200|User String",          0, "C:000001|T:20220228-1200|User String"},          // Test 48 : C,T
		{"c:000001|n:ResSim Only|User String",            0, "C:000001|N:ResSim Only|User String"},            // Test 49 : C,N
		{"c:000001|v:20220228-143000|User String",        0, "C:000001|V:20220228-143000|User String"},        // Test 50 : C,V
		{"c:000001|r:----E0|User String",                 0, "C:000001|R:----E0|User String"},                 // Test 51 : C,R
		{"t:20220228-1200|n:ResSim Only|User String",     0, "T:20220228-1200|N:ResSim Only|User String"},     // Test 52 : T,N
		{"t:20220228-1200|v:20220228-143000|User String", 0, "T:20220228-1200|V:20220228-143000|User String"}, // Test 53 : T,V
		{"t:20220228-1200|r:----E0|User String",          0, "T:20220228-1200|R:----E0|User String"},          // Test 54 : T,R
		{"n:ResSim Only|v:20220228-143000|User String",   0, "N:ResSim Only|V:20220228-143000|User String"},   // Test 55 : N,V
		{"n:ResSim Only|r:----E0|User String",            0, "N:ResSim Only|R:----E0|User String"},            // Test 56 : N,R
		{"v:20220228-143000|r:----E0|User String",        0, "V:20220228-143000|R:----E0|User String"},        // Test 57 : V,R
		// reverse canonical order
		{"t:20220228-1200|c:000001|User String",          0, "C:000001|T:20220228-1200|User String"},          // Test 58 : C,T
		{"n:ResSim Only|c:000001|User String",            0, "C:000001|N:ResSim Only|User String"},            // Test 59 : C,N
		{"v:20220228-143000|c:000001|User String",        0, "C:000001|V:20220228-143000|User String"},        // Test 60 : C,V
		{"r:----E0|c:000001|User String",                 0, "C:000001|R:----E0|User String"},                 // Test 61 : C,R
		{"n:ResSim Only|t:20220228-1200|User String",     0, "T:20220228-1200|N:ResSim Only|User String"},     // Test 62 : T,N
		{"v:20220228-143000|t:20220228-1200|User String", 0, "T:20220228-1200|V:20220228-143000|User String"}, // Test 63 : T,V
		{"r:----E0|t:20220228-1200|User String",          0, "T:20220228-1200|R:----E0|User String"},          // Test 64 : T,R
		{"v:20220228-143000|n:ResSim Only|User String",   0, "N:ResSim Only|V:20220228-143000|User String"},   // Test 65 : N,V
		{"r:----E0|n:ResSim Only|User String",            0, "N:ResSim Only|R:----E0|User String"},            // Test 66 : N,R
		{"r:----E0|v:20220228-143000|User String",        0, "V:20220228-143000|R:----E0|User String"},        // Test 67 : V,R
		//--------------------------//
		// 5 tags taken 1 at a time //
		//--------------------------//
		{"c:000001|User String",          0, "C:000001|User String"},          // Test 68 : C tag
		{"t:20220228-1200|User String",   0, "T:20220228-1200|User String"},   // Test 69 : T tag
		{"n:ResSim Only|User String",     0, "N:ResSim Only|User String"},     // Test 70 : N tag
		{"v:20220228-143000|User String", 0, "V:20220228-143000|User String"}, // Test 71 : V tag
		{"r:----E0|User String",          0, "R:----E0|User String"},          // Test 72 : R tag
	};
	int status = 0;
	char* normalized = NULL;
	for (int i = 0; i < sizeof(tests) / sizeof(tests[0]); ++i) {
		int _status = normalizeFPart(&normalized, tests[i].fPart);
		printf("Test %2d : Status = %2d : %s --> %s\n", i+1, _status, tests[i].fPart, normalized);
		if (_status != tests[i].expected_status) {
			status = -1;
		}
		if (normalized) {
			if (tests[i].expected_normalized == NULL) {
				status = -1;
			}
			else if (strcmp(tests[i].expected_normalized, normalized)) {
				status = -1;
			}
			free(normalized);
		}
		else if (tests[i].expected_normalized != NULL) {
			status = -1;
		}
	}
	return status;
}