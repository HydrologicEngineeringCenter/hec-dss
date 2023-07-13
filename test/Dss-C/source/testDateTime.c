#include "stdio.h"
#include "string.h"

#include "heclib.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "standardIntervals.h"
#include "TestDssC.h"



int getStyle(int i, int *twoDigitYear);
int compareDates(long long *ifltab, int jul1, int jul2, int sec1, int sec2, const char *mess);

int testDateTime()
{
	int DEBUG=0;

	int i;
	int j;
	int status;
	int numberPer;
	int secondsPastMidnight;
	int millsPastSecond;
	int timeStyle;
	int year, month, day;
	int iyear, imonth, iday;
	int julian;
	int jul;
	int zero;
	int one;
	int style;
	int twoDigitYear;
	int len;
	int itime;
	int seconds;
	int incNumber;
	float fseconds;
	long long ifltab[10];

	char cdate[20];
	char cdate2[20];
	char ctime[20];
	char ctimeIn[20];
	float s;

	int intervalSeconds;
	char Epart[30];
	int operation;
	int julianStart;
	int secondsStart; 
	int julianEnd; 
	int secondsEnd;
	int julianStart2;
	int secondsStart2; 
	int julianEnd2; 
	int secondsEnd2;
	int jul2;
	int number;
	int prtDate;


	zero = 0;
	one = 1;
	ifltab[0] = 0;


	

	secondsPastMidnight = SECS_IN_8_HOURS + SECS_IN_30_MINUTES + 45;  // 08:30:45
	millsPastSecond = 987;

	timeStyle = 0;
	stringCopy(ctimeIn, sizeof(ctimeIn), "0830", 4);
	secondsToTimeString(secondsPastMidnight, millsPastSecond, timeStyle, ctime, sizeof(ctime));
	status = zcompareStrings(ifltab, ctimeIn, ctime, (int)strlen(ctimeIn), 0, 1,  "testDateTime, secondsToTimeString failure loc 1, time ");
	if (status != STATUS_OKAY) return status;

	s = (float)30600.0;
	fseconds = timeStringToSecondsMills(ctime);
	status = zcompareFloats(ifltab, s, fseconds, 1,  "testDateTime, timeStringToSecondsMills failure loc 2, time ");
	if (status != STATUS_OKAY) return status;

	itime = 30600;
	seconds = timeStringToSeconds(ctime);
	status = zcompareInts(ifltab, itime, seconds, 1,  "testDateTime, timeStringToSecondsMills failure loc 3, time ");
	if (status != STATUS_OKAY) return status;


	timeStyle = 1;
	stringCopy(ctimeIn, sizeof(ctimeIn), "08:30", 5);
	secondsToTimeString(secondsPastMidnight, millsPastSecond, timeStyle, ctime, sizeof(ctime));
	status = zcompareStrings(ifltab, ctimeIn, ctime, (int)strlen(ctimeIn), 0, 1,  "testDateTime, secondsToTimeString failure loc 11, time ");
	if (status != STATUS_OKAY) return status;
	
	s = (float)30600.0;
	fseconds = timeStringToSecondsMills(ctime);
	status = zcompareFloats(ifltab, s, fseconds, 1,  "testDateTime, timeStringToSecondsMills failure loc 12, time ");
	if (status != STATUS_OKAY) return status;

	itime = 30600;
	seconds = timeStringToSeconds(ctime);
	status = zcompareInts(ifltab, itime, seconds, 1,  "testDateTime, timeStringToSecondsMills failure loc 13, time ");
	if (status != STATUS_OKAY) return status;

	
	timeStyle = 2;
	stringCopy(ctimeIn, sizeof(ctimeIn), "08:30:45", 8);
	secondsToTimeString(secondsPastMidnight, millsPastSecond, timeStyle, ctime, sizeof(ctime));
	status = zcompareStrings(ifltab, ctimeIn, ctime, (int)strlen(ctimeIn), 0, 1,  "testDateTime, secondsToTimeString failure loc 21, time ");
	if (status != STATUS_OKAY) return status;
		
	s = (float)30645.0;
	fseconds = timeStringToSecondsMills(ctime);
	status = zcompareFloats(ifltab, s, fseconds, 1,  "testDateTime, timeStringToSecondsMills failure loc 22, time  ");
	if (status != STATUS_OKAY) return status;

	itime = 30645;
	seconds = timeStringToSeconds(ctime);
	status = zcompareInts(ifltab, itime, seconds, 1,  "testDateTime, timeStringToSecondsMills failure loc 23, time ");
	if (status != STATUS_OKAY) return status;

	
	timeStyle = 3;
	stringCopy(ctimeIn, sizeof(ctimeIn), "08:30:45.987", 12);
	secondsToTimeString(secondsPastMidnight, millsPastSecond, timeStyle, ctime, sizeof(ctime));
	status = zcompareStrings(ifltab, ctimeIn, ctime, (int)strlen(ctimeIn), 0, 1,  "testDateTime, secondsToTimeString failure loc 31, time ");
	if (status != STATUS_OKAY) return status;
			
	s = (float)30645.987;
	fseconds = timeStringToSecondsMills(ctime);
	status = zcompareFloats(ifltab, s, fseconds, 1,  "testDateTime, timeStringToSecondsMills failure loc 32, time  ");
	if (status != STATUS_OKAY) return status;

	itime = 30645;
	seconds = timeStringToSeconds(ctime);
	status = zcompareInts(ifltab, itime, seconds, 1,  "testDateTime, timeStringToSecondsMills failure loc 33, time ");
	if (status != STATUS_OKAY) return status;



	iyear = 2008;
	imonth = 11;
	iday = 28;  //  28Nov2008
	julian = yearMonthDayToJulian (iyear, imonth, iday);  
	julianToYearMonthDay (julian, &year, &month, &day);

	status = zcompareInts(ifltab, iyear, year, 1,  "testDateTime, julianToYearMonthDay, failure loc 100, year ");
	if (status != STATUS_OKAY) return status;
	
	status = zcompareInts(ifltab, imonth, month, 1,  "testDateTime, julianToYearMonthDay, failure loc 101, month ");
	if (status != STATUS_OKAY) return status;
	
	status = zcompareInts(ifltab, iday, day, 1,  "testDateTime, julianToYearMonthDay, failure loc 102, day ");
	if (status != STATUS_OKAY) return status;




	number = julianToDate(julian, 4, cdate, sizeof(cdate));
	status = zcompareInts(ifltab, number, 9, 1,  "testDateTime, julianToDate, failure loc 120 status ");
	if (status != STATUS_OKAY) return status;

	status = zcompareStrings(ifltab, "28Nov2008", cdate, (int)strlen(cdate), 0, 1, "testDateTime, julianToDate, failure loc 121, date ");
	if (status != STATUS_OKAY) return status;

	jul = dateToJulian(cdate);
	status = zcompareInts(ifltab, julian, jul, 1,  "testDateTime, dateToJulian, failure loc 122, julian ");
	if (status != STATUS_OKAY) return status;	

	//  Now loop it, using table
	for (j=0; j<6; j++) {
		if (j==1) {
			julian = yearMonthDayToJulian (2000, 1, 1);
		}
		if (j==2) {
			julian = yearMonthDayToJulian (2000, 12, 31);
		}
		if (j==3) {
			julian = yearMonthDayToJulian (2008, 2, 29);  //   leap day
		}
		if (j==4) {
			julian = yearMonthDayToJulian (2008, 2, 28);
		}
		if (j==5) {
			julian = yearMonthDayToJulian (2008, 3, 1);
		}
		for (i=0; i<49; i++) {
			style = getStyle(i, &twoDigitYear);

			number = julianToDate(julian, style, cdate, sizeof(cdate));
			if (number <= 2) 
				status = -1;
			else
				status = 0;
			if (zcheckStatus(ifltab, status, 1,  "testDateTime, julianToDate loop, failure loc 200 status ")) return status;

			//  code only works for 2 digit year!!
			if (twoDigitYear) {
				len = (int)strlen(cdate);
				cdate[len] = cdate[len-2];
				cdate[len+1] = cdate[len-1];
				cdate[len-2] = '2';
				cdate[len-1] = '0';
				cdate[len+2] = '\0';
			}

			jul = dateToJulian(cdate);
			status = zcompareInts(ifltab, julian, jul, 1,  "testDateTime, dateToJulian, failure loc 201, julian ");	
			if (status != STATUS_OKAY) {
				printf("Iteration %d\n", i);
				printf("date = %s\n", cdate);
				return status;	
			}

			//  julian to year month day
			julianToYearMonthDay (jul, &year, &month, &day);

			//  year month day to date
			number = yearMonthDayToDate(year, month, day, style, cdate2, sizeof(cdate2));
			if (number <= 2) 
				status = -1;
			else
				status = 0;
			if (zcheckStatus(ifltab, status, 1,  "testDateTime, yearMonthDayToDate loop, failure loc 202 status ")) return status;

			//  year month day to julian
			julian = yearMonthDayToJulian (year, month, day); 
			status = zcompareInts(ifltab, julian, jul, 1,  "testDateTime, yearMonthDayToJulian, failure loc 203, julian ");
			if (status != STATUS_OKAY) return status;
		}
	}

	//  Print all intervals
	operation = BEGIN_ENUMERATION;
	while (ztsGetStandardInterval(7,&intervalSeconds, Epart, sizeof(Epart), &operation) == STATUS_OKAY) {
		if (intervalSeconds > 0) {
			printf("Regular Interval = %s,  intervalSeconds = %d\n", Epart, intervalSeconds);
		}
		else {
			printf("Irregular Interval = %s\n", Epart);
		}
	}
	operation = BEGIN_ENUMERATION;
	while (ztsGetStandardInterval(6,&intervalSeconds, Epart, sizeof(Epart), &operation) == STATUS_OKAY) {
		if (intervalSeconds > 0) {
			printf("Regular Interval = %s,  intervalSeconds = %d\n", Epart, intervalSeconds);
		}
		else {
			printf("Irregular Interval = %s\n", Epart);
		}
	}


	//  Test incrementTime.  
	//  For every time interval, add 1000, subtract 2000, then compare with original
	//  Compare with version 6 answers
	//  Then redo with different starting date and time
	//  Then redo with different offsets


	for (j=0; j<6; j++) {
		
		for (i=0; i<6; i++) {

			if (i == 0) {
				incNumber = 1000;
			}
			else if (i == 1) {
				incNumber = 1;
			}
			else if (i == 2) {
				incNumber = 2;
			}
			else if (i == 3) {
				incNumber = 0;
			}
			else if (i == 4) {
				incNumber = 500;
			}
			else if (i == 5) {
				incNumber = 999;
			}

			
			prtDate = 1;

			operation = BEGIN_ENUMERATION;
			while (ztsGetStandardInterval(7, &intervalSeconds, Epart, sizeof(Epart), &operation) == STATUS_OKAY) {
				if (operation < 0) break;
				if (intervalSeconds <= 0) break;

				if (j == 0) {
					julianStart = dateToJulian("16Sep1955");
					secondsStart = timeStringToSeconds("2400");
				}
				else if (j == 1) {
					julianStart = dateToJulian("16Sep1955");
					secondsStart = timeStringToSeconds("1234");
				}
				else if (j == 2) {
					julianStart = dateToJulian("04JUL1776");
					secondsStart = timeStringToSeconds("2400");
				}
				else if (j == 3) {
					julianStart = dateToJulian("25Dec2012");
					secondsStart = timeStringToSeconds("2400");
				}
				else if (j == 4) {
					julianStart = dateToJulian("01JAN3000");
					secondsStart = timeStringToSeconds("2400");
				}
				else if (j == 5) {
					julianStart = dateToJulian("04JUL3000");
					secondsStart = timeStringToSeconds("2400");
				} 
				if (DEBUG) {
					if (prtDate) {
						prtDate = 0;
						julianToDate(julianStart, style, cdate, sizeof(cdate));
						secondsToTimeString(secondsStart, 0, 2, ctime, sizeof(ctime));
						printf("\n\nBegin test loop, date = %s,  time = %s, julian = %d, seconds = %d\n", cdate, ctime, julianStart, secondsStart);
						printf("number to increment = %d\n", incNumber);
					}
				}
				

				if (DEBUG) printf("\nE part = %s\n", Epart);
				status = incrementTime(intervalSeconds, incNumber, julianStart, secondsStart, &julianEnd, &secondsEnd);
				if (zcheckStatus(ifltab, status, 1,  "testDateTime, incrementTime loop, failure loc 300 status ")) return status;
				julianToDate(julianEnd, style, cdate, sizeof(cdate));
				secondsToTimeString(secondsEnd, 0, 2, ctime, sizeof(ctime));
				if (DEBUG) printf("Loc 300a, date = %s,  time = %s,  julian = %d\n", cdate, ctime, julianEnd);

				numberPer = numberPeriods(intervalSeconds, julianStart, secondsStart, julianEnd, secondsEnd);
				status = zcompareInts(ifltab, incNumber, numberPer, 1,  "testDateTime, numberPeriods, failure loc 301 ");	
				if (status != STATUS_OKAY) {
					printf("Iteration i: %d,  j: %d\n", i, j);
					printf("date = %s\n", cdate);
					return status;	
				}

				number = incNumber;
				julianStart2 = julianStart;
				secondsStart2 = secondsStart;
				inctim2_(&intervalSeconds, &number, &julianStart2, &secondsStart2, &julianEnd2, &secondsEnd2);
				julianToDate(julianEnd2, style, cdate, sizeof(cdate));
				secondsToTimeString(secondsEnd2, 0, 2, ctime, sizeof(ctime));
				if (DEBUG) printf("Loc 300b, date = %s,  time = %s,  julian = %d\n", cdate, ctime, julianEnd);
				status = compareDates(ifltab, julianEnd, julianEnd2, secondsEnd, secondsEnd2,  "testDateTime, incrementTime, failure loc 302");
				if (status != STATUS_OKAY) return status;

				julianStart2 = julianEnd;
				secondsStart2 = secondsEnd;

				number = (-2 *incNumber);
				status = incrementTime(intervalSeconds, number, julianEnd, secondsEnd, &julianEnd, &secondsEnd);
				if (zcheckStatus(ifltab, status, 1,  "testDateTime, incrementTime loop, failure loc 311 status ")) return status;
				julianToDate(julianEnd, style, cdate, sizeof(cdate));
				secondsToTimeString(secondsEnd, 0, 2, ctime, sizeof(ctime));
				if (DEBUG) printf("Loc 301a, date = %s,  time = %s,  julian = %d\n", cdate, ctime, julianEnd);

				numberPer = numberPeriods(intervalSeconds, julianStart2, secondsStart2, julianEnd, secondsEnd);
				status = zcompareInts(ifltab, number, numberPer, 1,  "testDateTime, numberPeriods, failure loc 312 ");	
				if (status != STATUS_OKAY) {
					printf("Iteration i: %d,  j: %d\n", i, j);
					printf("date = %s\n", cdate);
					return status;	
				}

				inctim2_(&intervalSeconds, &number, &julianStart2, &secondsStart2, &julianEnd2, &secondsEnd2);
				julianToDate(julianEnd2, style, cdate, sizeof(cdate));
				secondsToTimeString(secondsEnd2, 0, 2, ctime, sizeof(ctime));
				if (DEBUG) printf("Loc 301b, date = %s,  time = %s,  julian = %d\n", cdate, ctime, julianEnd);
				status = compareDates(ifltab, julianEnd, julianEnd2, secondsEnd, secondsEnd2,  "testDateTime, incrementTime, failure loc 313");
				if (status != STATUS_OKAY) return status;

				julianStart2 = julianEnd;
				secondsStart2 = secondsEnd;

				status = incrementTime(intervalSeconds, incNumber, julianEnd, secondsEnd, &julianEnd, &secondsEnd);
				if (zcheckStatus(ifltab, status, 1,  "testDateTime, incrementTime loop, failure loc 321 status ")) return status;
				julianToDate(julianEnd, style, cdate, sizeof(cdate));
				secondsToTimeString(secondsEnd, 0, 2, ctime, sizeof(ctime));
				if (DEBUG) printf("Loc 302a, date = %s,  time = %s,  julian = %d\n", cdate, ctime, julianEnd);

				numberPer = numberPeriods(intervalSeconds, julianStart2, secondsStart2, julianEnd, secondsEnd);
				status = zcompareInts(ifltab, incNumber, numberPer, 1,  "testDateTime, numberPeriods, failure loc 322 ");	
				if (status != STATUS_OKAY) {
					printf("Iteration i: %d,  j: %d\n", i, j);
					printf("date = %s\n", cdate);
					return status;	
				}

				number = incNumber;
				inctim2_(&intervalSeconds, &number, &julianStart2, &secondsStart2, &julianEnd2, &secondsEnd2);
				julianToDate(julianEnd2, style, cdate, sizeof(cdate));
				secondsToTimeString(secondsEnd2, 0, 2, ctime, sizeof(ctime));
				if (DEBUG) printf("Loc 302b, date = %s,  time = %s,  julian = %d\n", cdate, ctime, julianEnd);
				status = compareDates(ifltab, julianEnd, julianEnd2, secondsEnd, secondsEnd2,  "testDateTime, incrementTime, failure loc 331");
				if (status != STATUS_OKAY) return status;

				status = compareDates(ifltab, julianStart, julianEnd, secondsStart, secondsEnd,  "testDateTime, incrementTime, failure loc 332");
				if (status != STATUS_OKAY) return status;

				numberPer = numberPeriods(intervalSeconds, julianStart, secondsStart, julianEnd, secondsEnd);
				status = zcompareInts(ifltab, 0, numberPer, 1,  "testDateTime, numberPeriods, failure loc 333 ");	
				if (status != STATUS_OKAY) {
					printf("Iteration i: %d,  j: %d\n", i, j);
					printf("date = %s\n", cdate);
					return status;	
				}

			}
		}
	}

	printf("Please wait - long conversion loops\n");

	//  Test julian to year month day and visa versa for extreem dates
	jul = 3000000;  // 1,000,000   (year 4637)
	for (i=0; i<2000000; i++) {   // -3,000,000 (year -6323)
		status = 0;
		jul--;
		julianToYearMonthDay(jul, &year, &month, &day);
		if (month < 1) {
			printf("julianToYearMonthDay test failed at loc 101, month: %d\n", month);
			status = -1;
		}
		if (month > 12) {
			printf("julianToYearMonthDay test failed at loc 102, month: %d\n", month);
			status = -1;
		}
		if (day < 1) {
			printf("julianToYearMonthDay test failed at loc 103, day: %d\n", day);
			status = -1;
		}
		if (day > 31) {
			printf("julianToYearMonthDay test failed at loc 104, day: %d\n", day);
			status = -1;
		}
		if (!status) {
			julian = yearMonthDayToJulian(year, month, day);		
			if (julian != jul) {
				printf("julianToYearMonthDay / yearMonthDayToJulian test failed at loc 105\n");
				status = -1;
			}
		}
		if (status) {
			printf("i = %d\n", i);
			printf("jul = %d\n", jul);
			printf("julian = %d\n", julian);
			printf("year = %d\n", year);
			printf("month = %d\n", month);
			printf("day = %d\n", day);
			return -1;
		}
	}

	jul = dateToJulian("20Jan10001");

	julianToDate(jul, 0, cdate, sizeof(cdate));
	printf("Starting Date: %s\n", cdate);
	for (i=0; i<9000000; i++) {
		julianToYearMonthDay(jul, &year, &month, &day);
		jul2 = yearMonthDayToJulian(year, month, day);
		julianToDate(jul, 0, cdate, sizeof(cdate));
		if( i % 100000 == 0)
		   printf(" %d, %d  %d  %d\n", jul, year, month, day);
		if (jul2 != jul) {
			printf ("Julian miss-match; Failure.  Original jul %d, computed jul %d,  date: %s\n", jul, jul2, cdate);
			julianToYearMonthDay(jul, &year, &month, &day);
			jul2 = yearMonthDayToJulian(year, month, day);
			return -1;
		}
		jul--;
	}
	printf("Ending Date: %s\n", cdate);
	

	printf("Completed date and time functions test successfully!\n");

	return 0; 
}

int compareDates(long long *ifltab, int jul1, int jul2, int sec1, int sec2, const char *mess) 
{
	int status;

	status = zcompareInts(ifltab, jul1, jul2, 1, mess);
	if (status != STATUS_OKAY) {
		printf("Error occured for julian dates\n");
		return status;
	}

	status = zcompareInts(ifltab, sec1, sec2, 1, mess);
	if (status != STATUS_OKAY) {
		printf("Error occured for seconds past midnight\n");
		return status;
	}

	return status;
}

int getStyle(int i, int  *twoDigitYear)
{
	*twoDigitYear = 0;
	if (i<20) {
		//  Do not do styles that do not return a day
		if (i==3) i--;
		if (i==13) i--;
		if (i==6) i--;
		if (i==16) i--;
		if (i==9) i--;
		if (i==19) i--;
		if (i > 9) *twoDigitYear = 1;
		return i;
	}
	if (i<40) {
		i += 80;
		//  Do not do styles that do not return a day
		if (i==103) i--;
		if (i==113) i--;
		if (i==106) i--;
		if (i==116) i--;
		if (i==109) i--;
		if (i==119) i--;
		if (i > 109) *twoDigitYear = 1;
		return i;
	}
	if (i==40) {
		*twoDigitYear = 1;
		return -1;
	}
	if (i==41) {
		*twoDigitYear = 1;
		return -2;
	}
	if (i==42) {
		*twoDigitYear = 1;
		return -11;
	}
	if (i==43) {
		*twoDigitYear = 1;
		return -12;
	}
	if (i==44) {
		return -13;
	}
	if (i==45) {
		return -101;
	}
	if (i==46) {
		return -102;
	}
	if (i==47) {
		return -111;
	}
	if (i==48) {
		return -112;
	}
	return 0;
}