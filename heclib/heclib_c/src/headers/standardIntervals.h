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
    SECS_IN_8_HOURS = 28800,
    SECS_IN_6_HOURS = 21600,
    SECS_IN_4_HOURS = 14400,
    SECS_IN_3_HOURS = 10800,
    SECS_IN_2_HOURS = 7200,
    SECS_IN_1_HOUR = 3600,
    SECS_IN_30_MINUTES = 1800,
    SECS_IN_20_MINUTES = 1200,
    SECS_IN_15_MINUTES = 900,
    SECS_IN_12_MINUTES = 720,
    SECS_IN_10_MINUTES = 600,
    SECS_IN_6_MINUTES = 360,
    SECS_IN_5_MINUTES = 300,
    SECS_IN_4_MINUTES = 240,
    SECS_IN_3_MINUTES = 180,
    SECS_IN_2_MINUTES = 120,
    SECS_IN_1_MINUTE = 60,
    SECS_IN_30_SECONDS = 30,
    SECS_IN_20_SECONDS = 20,
    SECS_IN_15_SECONDS = 15,
    SECS_IN_10_SECONDS = 10,
    SECS_IN_6_SECONDS = 6,
    SECS_IN_5_SECONDS = 5,
    SECS_IN_4_SECONDS = 4,
    SECS_IN_3_SECONDS = 3,
    SECS_IN_2_SECONDS = 2,
    SECS_IN_1_SECOND = 1
};

enum INTERVAL_MINUTES {
    MINS_IN_1_YEAR = 525600,    // 365 days (representative, not actual)
    MINS_IN_1_MONTH = 43200,    //  30 days (representative, not actual)
    MINS_IN_SEMI_MONTH = 21600, //  15 days (representative, not actual)
    MINS_IN_TRI_MONTH = 14400,  //  10 days (representative, not actual)
    MINS_IN_1_WEEK = 10080,
    MINS_IN_1_DAY = 1440,
    MINS_IN_12_HOURS = 720,
    MINS_IN_8_HOURS = 480,
    MINS_IN_6_HOURS = 360,
    MINS_IN_4_HOURS = 240,
    MINS_IN_3_HOURS = 180,
    MINS_IN_2_HOURS = 120,
    MINS_IN_1_HOUR = 60,
    MINS_IN_30_MINUTES = 30,
    MINS_IN_20_MINUTES = 20,
    MINS_IN_15_MINUTES = 15,
    MINS_IN_12_MINUTES = 12,
    MINS_IN_10_MINUTES = 10,
    MINS_IN_6_MINUTES = 6,
    MINS_IN_5_MINUTES = 5,
    MINS_IN_4_MINUTES = 4,
    MINS_IN_3_MINUTES = 3,
    MINS_IN_2_MINUTES = 2,
    MINS_IN_1_MINUTE = 1
};
enum INTERVAL_HOURS {
    HOURS_IN_1_YEAR = 8760,    // 365 days (representative, not actual)
    HOURS_IN_1_MONTH = 360,    //  30 days (representative, not actual)
    HOURS_IN_SEMI_MONTH = 360, //  15 days (representative, not actual)
    HOURS_IN_TRI_MONTH = 240,  //  10 days (representative, not actual)
    HOURS_IN_1_WEEK = 168,
    HOURS_IN_1_DAY = 24,
    HOURS_IN_12_HOURS = 12,
    HOURS_IN_8_HOURS = 8,
    HOURS_IN_6_HOURS = 6,
    HOURS_IN_4_HOURS = 4,
    HOURS_IN_3_HOURS = 3,
    HOURS_IN_2_HOURS = 2,
    HOURS_IN_1_HOUR = 1
};

enum INTERVAL_DAYS {
    DAYS_IN_1_YEAR = 365,    // representative, not actual
    DAYS_IN_1_MONTH = 30,    // representative, not actual
    DAYS_IN_SEMI_MONTH = 15, // representative, not actual
    DAYS_IN_TRI_MONTH = 10,  // representative, not actual
    DAYS_IN_1_WEEK = 7
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
	SECS_IN_1_YEAR,       SECS_IN_1_MONTH,     SECS_IN_SEMI_MONTH,   SECS_IN_TRI_MONTH,
	SECS_IN_1_WEEK,       SECS_IN_1_DAY,       SECS_IN_12_HOUR,      SECS_IN_8_HOURS,
	SECS_IN_6_HOURS,      SECS_IN_4_HOURS,     SECS_IN_3_HOURS,      SECS_IN_2_HOURS,
	SECS_IN_1_HOUR,       SECS_IN_30_MINUTES,  SECS_IN_20_MINUTES,   SECS_IN_15_MINUTES,
	SECS_IN_12_MINUTES,   SECS_IN_10_MINUTES,  SECS_IN_6_MINUTES,    SECS_IN_5_MINUTES,
	SECS_IN_4_MINUTES,    SECS_IN_3_MINUTES,   SECS_IN_2_MINUTES,    SECS_IN_1_MINUTE,
	SECS_IN_30_SECONDS,   SECS_IN_20_SECONDS,  SECS_IN_15_SECONDS,   SECS_IN_10_SECONDS,
	SECS_IN_6_SECONDS,    SECS_IN_5_SECONDS,   SECS_IN_4_SECONDS,    SECS_IN_3_SECONDS,
	SECS_IN_2_SECONDS,    SECS_IN_1_SECOND,
	-BLOCK_1_CENTURY,     -BLOCK_1_DECADE,     -BLOCK_1_YEAR,        -BLOCK_1_MONTH,      -BLOCK_1_DAY };

static const char eParts6[][12] = {
	"1YEAR",      "1MON",      "SEMI-MONTH", "TRI-MONTH",
	"1WEEK",      "1DAY",      "12HOUR",     "8HOUR",
	"6HOUR",      "4HOUR",     "3HOUR",      "2HOUR",
	"1HOUR",      "30MIN",     "20MIN",      "15MIN",
	"12MIN",	  "10MIN",     "6MIN",       "5MIN",
	"4MIN",       "3MIN",      "2MIN",       "1MIN",
	"IR-CENTURY", "IR-DECADE", "IR-YEAR",	 "IR-MONTH",   "IR-DAY" };

static int secondsInInterval6[29] = {
	SECS_IN_1_YEAR,       SECS_IN_1_MONTH,     SECS_IN_SEMI_MONTH,   SECS_IN_TRI_MONTH,
	SECS_IN_1_WEEK,       SECS_IN_1_DAY,       SECS_IN_12_HOUR,      SECS_IN_8_HOURS,
	SECS_IN_6_HOURS,      SECS_IN_4_HOURS,     SECS_IN_3_HOURS,      SECS_IN_2_HOURS,
	SECS_IN_1_HOUR,       SECS_IN_30_MINUTES,  SECS_IN_20_MINUTES,   SECS_IN_15_MINUTES,
	SECS_IN_12_MINUTES,   SECS_IN_10_MINUTES,  SECS_IN_6_MINUTES,    SECS_IN_5_MINUTES,
	SECS_IN_4_MINUTES,    SECS_IN_3_MINUTES,   SECS_IN_2_MINUTES,    SECS_IN_1_MINUTE,
    -BLOCK_1_CENTURY,     -BLOCK_1_DECADE,     -BLOCK_1_YEAR,        -BLOCK_1_MONTH,      -BLOCK_1_DAY };
