#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "TestDssC.h"


int test_getFileFromPath(void)
{
	char buf[MAX_FILENAME_LENGTH];
	char *result;
	const char *longPath;
	const char *expected = "final_deliverable.dss";

#if defined(_WIN32) || defined(_MSC_VER)
     //  354 chars 
	longPath = "C:\\Users\\engineer\\Documents\\USACE_Projects\\HEC-RAS_modeling\\Mississippi_River_Basin\\Upper_Mississippi_Subbasin_Studies\\FY2026_Q1_2026_Continuing_Authorities_Program\\rainfall_runoff_calibration_runs\\scenario_with_updated_SCS_curve_numbers\\output_hecdss_files\\validated_against_usgs_gage_records_2020_through_2025\\final_review_package\\final_deliverable.dss";
#else
	longPath = "/home/engineer/work/USACE_Projects/HEC-RAS_modeling/Mississippi_River_Basin/Upper_Mississippi_Subbasin_Studies/FY2026_Q1_2026_Continuing_Authorities_Program/rainfall_runoff_calibration_runs/scenario_with_updated_SCS_curve_numbers/output_hecdss_files/validated_against_usgs_gage_records_2020_through_2025/final_review_package/final_deliverable.dss";
#endif

	result = getFileFromPath(buf, sizeof(buf), longPath);
	if (result == NULL || strcmp(result, expected) != 0) {
		printf("\nFAIL: getFileFromPath(%d-char path) -> \"%s\"",
			(int)strlen(longPath), result ? result : "<NULL>");
		return -1;
	}

	printf("\ntest_getFileFromPath: OK (%d-char input)", (int)strlen(longPath));
	return 0;
}
