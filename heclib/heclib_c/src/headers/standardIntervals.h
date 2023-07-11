#pragma once
//-------------------------------------------------//
// definitions of time series intervals in seconds //
//-------------------------------------------------//
enum INTERVAL_SECONDS {
    SECS_IN_1_YEAR = 31536000,    // 365 days (representative, not actual)
    SECS_IN_1_MONTH = 2592000,    //  30 days (representative, not actual)
    SECS_IN_SEMI_MONTH = 1296000, //  15 days (representative, not actual)
    SECS_IN_TRI_MONTH = 864000,   //  10 days (representative, not actual)
    SECS_IN_1_WEEK = 604800,
    SECS_IN_1_DAY = 86400,
    SECS_IN_12_HOUR = 43200,
    SECS_IN_8_HOUR = 28800,
    SECS_IN_6_HOUR = 21600,
    SECS_IN_4_HOUR = 14400,
    SECS_IN_3_HOUR = 10800,
    SECS_IN_2_HOUR = 7200,
    SECS_IN_1_HOUR = 3600,
    SECS_IN_30_MINUTE = 1800,
    SECS_IN_20_MINUTE = 1200,
    SECS_IN_15_MINUTE = 900,
    SECS_IN_12_MINUTE = 720,
    SECS_IN_10_MINUTE = 600,
    SECS_IN_6_MINUTE = 360,
    SECS_IN_5_MINUTE = 300,
    SECS_IN_4_MINUTE = 240,
    SECS_IN_3_MINUTE = 180,
    SECS_IN_2_MINUTE = 120,
    SECS_IN_1_MINUTE = 60,
    SECS_IN_30_SECOND = 30,
    SECS_IN_20_SECOND = 20,
    SECS_IN_15_SECOND = 15,
    SECS_IN_10_SECOND = 10,
    SECS_IN_6_SECOND = 6,
    SECS_IN_5_SECOND = 5,
    SECS_IN_4_SECOND = 4,
    SECS_IN_3_SECOND = 3,
    SECS_IN_2_SECOND = 2,
    SECS_IN_1_SECOND = 1
};

enum INTERVAL_MINUTES {
    MINS_IN_1_YEAR = 525600,    // 365 days (representative, not actual)
    MINS_IN_1_MONTH = 43200,    //  30 days (representative, not actual)
    MINS_IN_SEMI_MONTH = 21600, //  15 days (representative, not actual)
    MINS_IN_TRI_MONTH = 14400,  //  10 days (representative, not actual)
    MINS_IN_1_WEEK = 10080,
    MINS_IN_1_DAY = 1440,
    MINS_IN_12_HOUR = 720,
    MINS_IN_8_HOUR = 480,
    MINS_IN_6_HOUR = 360,
    MINS_IN_4_HOUR = 240,
    MINS_IN_3_HOUR = 180,
    MINS_IN_2_HOUR = 120,
    MINS_IN_1_HOUR = 60,
    MINS_IN_30_MINUTE = 30,
    MINS_IN_20_MINUTE = 20,
    MINS_IN_15_MINUTE = 15,
    MINS_IN_12_MINUTE = 12,
    MINS_IN_10_MINUTE = 10,
    MINS_IN_6_MINUTE = 6,
    MINS_IN_5_MINUTE = 5,
    MINS_IN_4_MINUTE = 4,
    MINS_IN_3_MINUTE = 3,
    MINS_IN_2_MINUTE = 2,
    MINS_IN_1_MINUTE = 1
};
enum INTERVAL_HOURS {
    HOURS_IN_1_YEAR = 8760,    // 365 days (representative, not actual)
    HOURS_IN_1_MONTH = 360,    //  30 days (representative, not actual)
    HOURS_IN_SEMI_MONTH = 360, //  15 days (representative, not actual)
    HOURS_IN_TRI_MONTH = 240,  //  10 days (representative, not actual)
    HOURS_IN_1_WEEK = 168,
    HOURS_IN_1_DAY = 24,
    HOURS_IN_12_HOUR = 12,
    HOURS_IN_8_HOUR = 8,
    HOURS_IN_6_HOUR = 6,
    HOURS_IN_4_HOUR = 4,
    HOURS_IN_3_HOUR = 3,
    HOURS_IN_2_HOUR = 2,
    HOURS_IN_1_HOUR = 1
};

enum INTERVAL_DAYS {
    DAYS_IN_1_YEAR = 365,    // representative, not actual
    DAYS_IN_1_MONTH = 30,    // representative, not actual
    DAYS_IN_SEMI_MONTH = 15, // representative, not actual
    DAYS_IN_TRI_MONTH = 10,  // representative, not actual
    DAYS_IN_1_WEEK = 7,
    DAYS_IN_1_DAY = 1
};

enum BLOCK_SIZE {
    BLOCK_1_DAY = 1,
    BLOCK_1_MONTH,  // 2
    BLOCK_1_YEAR,   // 3
    BLOCK_1_DECADE, // 4
    BLOCK_1_CENTURY // 5
};

enum INTERVAL_OPERATION {
    EPART_TO_SECONDS_TO_EPART, // 0
    EPART_TO_SECONDS,          // 1
    SECONDS_TO_EPART,          // 2
    BEGIN_ENUMERATION,         // 3
    CONTINUE_ENUMERATION       // 4
};

static const char eParts7[][12] = {
	"1Year",      "1Month",    "Semi-Month", "Tri-Month",
	"1Week",      "1Day",      "12Hour",     "8Hour",
	"6Hour",      "4Hour",     "3Hour",      "2Hour",
	"1Hour",      "30Minute",  "20Minute",   "15Minute",
	"12Minute",   "10Minute",  "6Minute",    "5Minute",
	"4Minute",    "3Minute",   "2Minute",    "1Minute",
	"30Second",   "20Second",  "15Second",   "10Second",
	"6Second",    "5Second",   "4Second",    "3Second",
	"2Second",    "1Second",
	"IR-Century", "IR-Decade", "IR-Year",	 "IR-Month",  "IR-Day" };

static int secondsInInterval7[39] = {
	SECS_IN_1_YEAR,      SECS_IN_1_MONTH,    SECS_IN_SEMI_MONTH,  SECS_IN_TRI_MONTH,
	SECS_IN_1_WEEK,      SECS_IN_1_DAY,      SECS_IN_12_HOUR,     SECS_IN_8_HOUR,
	SECS_IN_6_HOUR,      SECS_IN_4_HOUR,     SECS_IN_3_HOUR,      SECS_IN_2_HOUR,
	SECS_IN_1_HOUR,      SECS_IN_30_MINUTE,  SECS_IN_20_MINUTE,   SECS_IN_15_MINUTE,
	SECS_IN_12_MINUTE,   SECS_IN_10_MINUTE,  SECS_IN_6_MINUTE,    SECS_IN_5_MINUTE,
	SECS_IN_4_MINUTE,    SECS_IN_3_MINUTE,   SECS_IN_2_MINUTE,    SECS_IN_1_MINUTE,
	SECS_IN_30_SECOND,   SECS_IN_20_SECOND,  SECS_IN_15_SECOND,   SECS_IN_10_SECOND,
	SECS_IN_6_SECOND,    SECS_IN_5_SECOND,   SECS_IN_4_SECOND,    SECS_IN_3_SECOND,
	SECS_IN_2_SECOND,    SECS_IN_1_SECOND,
	-BLOCK_1_CENTURY,      -BLOCK_1_DECADE,      -BLOCK_1_YEAR,         -BLOCK_1_MONTH,      -BLOCK_1_DAY };

static const char eParts6[][12] = {
	"1YEAR",      "1MON",      "SEMI-MONTH", "TRI-MONTH",
	"1WEEK",      "1DAY",      "12HOUR",     "8HOUR",
	"6HOUR",      "4HOUR",     "3HOUR",      "2HOUR",
	"1HOUR",      "30MIN",     "20MIN",      "15MIN",
	"12MIN",	  "10MIN",     "6MIN",       "5MIN",
	"4MIN",       "3MIN",      "2MIN",       "1MIN",
	"IR-CENTURY", "IR-DECADE", "IR-YEAR",	 "IR-MONTH",   "IR-DAY" };

static int secondsInInterval6[29] = {
	SECS_IN_1_YEAR,      SECS_IN_1_MONTH,    SECS_IN_SEMI_MONTH,  SECS_IN_TRI_MONTH,
	SECS_IN_1_WEEK,      SECS_IN_1_DAY,      SECS_IN_12_HOUR,     SECS_IN_8_HOUR,
	SECS_IN_6_HOUR,      SECS_IN_4_HOUR,     SECS_IN_3_HOUR,      SECS_IN_2_HOUR,
	SECS_IN_1_HOUR,      SECS_IN_30_MINUTE,  SECS_IN_20_MINUTE,   SECS_IN_15_MINUTE,
	SECS_IN_12_MINUTE,   SECS_IN_10_MINUTE,  SECS_IN_6_MINUTE,    SECS_IN_5_MINUTE,
	SECS_IN_4_MINUTE,    SECS_IN_3_MINUTE,   SECS_IN_2_MINUTE,    SECS_IN_1_MINUTE,
    -BLOCK_1_CENTURY,      -BLOCK_1_DECADE,      -BLOCK_1_YEAR,         -BLOCK_1_MONTH,      -BLOCK_1_DAY };
