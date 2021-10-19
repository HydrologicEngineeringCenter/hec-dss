#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <math.h>

#include "hecdssFort.h"

#ifdef _MSC_VER
	#define unlink _unlink
	typedef __int32 int32_t;
#else
	#include <unistd.h>
	#include <stdint.h>
	#define stricmp strcasecmp
#endif

#ifndef max
	#define max(a, b) (a > b ? a : b)
	#define min(a, b) (a < b ? a : b)
#endif

#ifndef FALSE
	#define FALSE 0
	#define TRUE !FALSE
#endif

char *Months[] = {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};

#define PART_PAD_LEN 67
#define LINE_LEN 500


#define CLEANUP(text) { \
	if (text != NULL && strlen(text)) { \
		fortwrite("", munit, TRUE); \
		fortwrite("Catalog Sort Error: ", munit, FALSE); \
		fortwrite(text, munit, TRUE); \
	} \
	for (i = 0; i < header_line_count; ++i) free(header_lines[i]); \
	free(header_lines); \
	if (line_data != NULL) free(line_data); \
	if (lines != NULL) free(lines); \
	if (done != NULL) free(done); \
	if (catfile != NULL) fclose(catfile); \
	if (condcatfile != NULL) fclose(condcatfile); \
	if (temp_filenames != NULL) { \
		for (i = 0; i < tempfile_count; ++i) { \
			if (tempfiles[i] != NULL) { \
				fclose(tempfiles[i]); \
				free(tempfiles[i]); \
			} \
			unlink(temp_filenames[i]); \
			free(temp_filenames[i]); \
		} \
		free(temp_filenames); \
		free(tempfiles); \
	} \
}
#define F2C(f, c, flen, clen) { \
	int len = flen < clen ? flen : clen - 1; \
	int i; \
	strncpy(c, f, len); \
	c[len] = '\0'; \
	for (i = len-1; i > -1; --i) { \
		if (c[i] != ' ' ) break; \
		c[i] = '\0'; \
	} \
}

#define C2F(c, f, flen) { \
	int i = strlen(c); \
	int len = i > flen ? flen : i; \
	strncpy(f, c, len); \
	for (i = len; i < flen; ++i) f[i] = ' '; \
}

#define ABORT(text) {CLEANUP(text); return;}

int32_t get_month(char *text, int32_t len) {

	int32_t month = 0;
	int32_t i;

	if (len >= 9
		 && isdigit(text[0])
		 && isdigit(text[1])
		 && isdigit(text[5])
		 && isdigit(text[6])
		 && isdigit(text[7])
		 && isdigit(text[8])) {
		for (i = 0; i < 12; ++i) {
			if (!strncmp(&text[2], Months[i], 3)) {
				month = i + 1;
				break;
			}
		}
	}
	return month;

}

void restore_date(char *text) {

	int32_t year, month, day;
	char extra[PART_PAD_LEN];

	if (strlen(text) >= 9
		 && text[0] == ' '
		 && isdigit(text[1])
		 && isdigit(text[2])
		 && isdigit(text[3])
		 && isdigit(text[4])
		 && isdigit(text[5])
		 && isdigit(text[6])
		 && isdigit(text[7])
		 && isdigit(text[8])) {
		month = (text[5] - '0') * 10 +
			(text[6] - '0');
		if (month > 0 && month < 13) {
			year = (text[1] - '0') * 1000 +
					 (text[2] - '0') * 100  +
					 (text[3] - '0') * 10   +
					 (text[4] - '0');
			day =  (text[7] - '0') * 10 +
					 (text[8] - '0');
			if (text[9]) {
				strcpy(extra, &text[9]);
				sprintf(text, "%2.2d%3s%4.4d%s", day, Months[month-1], year, extra);
			}
			else {
				sprintf(text, "%2.2d%3s%4.4d", day, Months[month-1], year);
			}
		}
	}
}

int32_t catsort_compare(const void *vp1, const void *vp2) {

	return strcmp(*(char **)vp1, *(char **)vp2);
}

char *ljust(char *dst, const char *src, int32_t length) {

	int32_t i;

	strncpy(dst, src, length);
	for (i = strlen(dst); i < length; ++i) dst[i] = ' ';
	dst[length] = '\0';
	return dst;
}

char *elapsed(char *dst, time_t t1, time_t t2) {

	int32_t h, m, s = (int32_t)(t2 - t1);
	h = s / 3600;
	m = (s % 3600) / 60;
	s %= 60;
	sprintf(dst, "%2.2d:%2.2d:%2.2d", h, m, s);
	return dst;
}

void fortwrite(char *message, int32_t munit, int32_t advance) {
	fortranwrite_(&munit, message, strlen(message));
}

void catsort(
	char    *catfilename,
	int32_t *sort_order,
	int32_t  make_condensed,
	int32_t *interrupt,
	int32_t *current_numb,
	int32_t  output,
	int32_t  munit) {

	FILE *catfile = NULL;
	FILE *condcatfile = NULL;
	FILE **tempfiles = NULL;
	char **temp_filenames = NULL;
	char line[4096];
	char outbuf[4096];
	char format[32];
	char **header_lines = NULL;
	char *line_data = NULL;
	char **lines = NULL;
	char parts[6][PART_PAD_LEN];
	char misc_info[80];
	int32_t tempfile_count = 0;
	int32_t part_order[6];
	int32_t max_len[6] = {0, 0, 0, 0, 0, 0};
	int32_t max_tag = 0;
	int32_t header_line_count = 0;
	int32_t pathname_count = 0;
	int32_t line_count = 0;
	int32_t total_line_count = 0;
	int32_t offset = 0;
	int32_t first_pos;
	int32_t pos;
	int32_t month;
	int32_t group_count = 1;
	int32_t max_lines;
	int32_t i, j, k;
	int32_t *done = NULL;
	time_t t1, t2, t3;

	t1 = t2 = time(NULL);
	//-------------------------------------------------------//
	// verify the sort order is ABCFED for condensed catalog //
	//-------------------------------------------------------//
	if (make_condensed) {
		if (sort_order[0] != 1
			 || sort_order[1] != 2
			 || sort_order[2] != 3
			 || sort_order[3] != 6
			 || sort_order[4] != 5
			 || sort_order[5] != 4) ABORT("Invalid sort order for condensed catalog.");
	}

	//------------------------------------//
	// invert the sort order for indexing //
	//------------------------------------//
	for (i = 0; i < 6; ++i) part_order[sort_order[i]-1] = i;

	//-----------------------//
	// open the catalog file //
	//-----------------------//
	catfile = fopen(catfilename, "r");
	if (!catfile) ABORT("Cannot open catalog for reading.");

	//-----------------------//
	// read the header lines //
	//-----------------------//
	if (*interrupt != 0) ABORT("Catalog sort interrupted");
	fgets(line, sizeof(line), catfile);
	while(atoi(line) == 0) {
		line[strlen(line)-1] = '\0';
		if (!strncmp(line, "     Number of Records:", 23)) pathname_count = atoi(&line[23]);
		i = header_line_count++;
		if (i) header_lines = (char **)realloc(header_lines, header_line_count * sizeof(char*));
		else   header_lines = (char **)malloc((size_t)header_line_count * sizeof(char*));
		if (header_lines == NULL) ABORT("Memory allocation error.");
		if ((header_lines[i] = strdup(line)) == NULL) ABORT("Memory allocation error.");
		fgets(line, sizeof(line), catfile);
	}
	if (pathname_count == 0) ABORT("Cannot determine record count.");

	//--------------------------//
	// allocate memory for sort //
	//--------------------------//
	if (*interrupt != 0) ABORT("Catalog sort interrupted");
	for (group_count = 1;; ++group_count) {
		if (group_count == 1) {
			max_lines = pathname_count;
		}
		else {
			max_lines = ceil((double)pathname_count / (double)group_count);
			if (group_count >= max_lines) ABORT("Too many sort groups required.");
		}
		line_data = (char *)malloc((size_t)max_lines * LINE_LEN * sizeof(char));
		if (line_data == NULL) continue;
		lines = (char **)malloc((size_t)max_lines * sizeof(char *));
		if (lines != NULL) break;
		free(line_data);
	}

	//--------------------------------------------//
	// read the pathames and reformat for sorting //
	//--------------------------------------------//
	if (output) {
		fortwrite("Sorting catalog:", munit, TRUE);
		if (group_count > 1) {
			sprintf(outbuf, "  ==> Subset 1 of %d", group_count);
			fortwrite(outbuf, munit, TRUE);
		}
		fortwrite("  Reading...", munit, FALSE);
	}
	first_pos = strchr(line, '/') - line;
	while (1) {
		if (*interrupt != 0) ABORT("Catalog sort interrupted");
		pos = first_pos;
		offset = 0;
		line[strlen(line)-1] = '\0';
		/*----------------------------------------------*/
		/* copy the tag name and other misc info - we   */
		/* will put this after the pathname for sorting */
		/*----------------------------------------------*/
		strncpy(misc_info, &line[8], pos-8);
		misc_info[pos-8] = '\0';
		if (make_condensed) {
			/*----------------------------------------------------*/
			/* keep track of max tag length for condensed catalog */
			/*----------------------------------------------------*/
			for (i = 0; misc_info[i]; ++i) if (misc_info[i] != ' ') break;
			for (j = i; misc_info[j]; ++j) if (misc_info[j] == ' ') break;
			if (j - i > max_tag) max_tag = j - i;
		}
		/*------------------------------------------------------*/
		/* put pathname parts in sort order, remove slashes and */
		/* pad with at least one space to ensure proper sorting */
		/*------------------------------------------------------*/
		memset(parts, ' ', sizeof(parts));
		for (k = 0; k < 6; ++k) {
			j = part_order[k];
			offset += pos+1;
			pos = strchr(&line[offset], '/') - &line[offset];
			/*------------------------------------------------------*/
			/* keep track of max part lengths for condensed catalog */
			/*------------------------------------------------------*/
			if (make_condensed) {
				if (pos > max_len[k]) max_len[k] = pos;
			}
			month = get_month(&line[offset], pos);
			if (month) {
				/*---------------------------------------------------*/
				/* replace any date (DDMMMYYYY) part with a sortable */
				/* version that we can restore after the sort        */
				/*---------------------------------------------------*/
				parts[j][0] = ' ';
				strncpy(&parts[j][1], &line[offset+5], 4);
				parts[j][5] = month < 10 ? '0' : '1';
				parts[j][6] = '0' + month % 10;
				strncpy(&parts[j][7], &line[offset], 2);
				if (pos > 9) {
					strncpy(&parts[j][9], &line[offset+9], pos-9);
				}
			}
			else if (!strncmp(&line[offset], "IR-", 3)
				 && (!strncmp(&line[offset+3], "DAY/", 4)
					  || !strncmp(&line[offset+3], "MONTH/",  6)
					  || !strncmp(&line[offset+3], "YEAR/",   5)
					  || !strncmp(&line[offset+3], "DECADE/", 7))) {
				/*---------------------------------------*/
				/* make sure irregular time series sorts */
				/* before regular time series            */
				/*---------------------------------------*/
				parts[j][0] = parts[j][1] = ' ';
				strncpy(&parts[j][2], &line[offset], pos);
			}
			else {
				strncpy(parts[j], &line[offset], pos);
			}
		}
		i = line_count++;
		++total_line_count;
		lines[i] = &line_data[i*LINE_LEN];
		for (j = 0; j < 6; ++j) strncpy(lines[i] + j * PART_PAD_LEN, parts[j], PART_PAD_LEN);
		strcpy(lines[i] + sizeof(parts), misc_info);
		if (total_line_count == pathname_count) break;
		if (line_count == max_lines) {
			//------------------------------------------------------------------//
			// sort the lines currently in memory and spill to a temporary file //
			//------------------------------------------------------------------//
			if (*interrupt != 0) ABORT("Catalog sort interrupted");
			j = tempfile_count++;
			if (output) {
				sprintf(outbuf, "%s", elapsed(line, t2, (t3 = time(NULL))));
				fortwrite(outbuf, munit, TRUE);
				fortwrite("  Sorting...", munit, FALSE);
			}
			qsort(lines, line_count, sizeof(char *), catsort_compare);
			if (output) {
				sprintf(outbuf, "%s", elapsed(line, t3, (t2 = time(NULL))));
				fortwrite(outbuf, munit, TRUE);
				fortwrite("  Writing...", munit, FALSE);
			}
			temp_filenames = (char **)realloc(temp_filenames, tempfile_count * sizeof(char *));
			tempfiles = (FILE **)realloc(tempfiles, tempfile_count * sizeof(FILE *));
			if (!(temp_filenames[j] = tempnam(NULL, "sort"))) ABORT("Could not create temporary filename.");
			if (!(tempfiles[j] = fopen(temp_filenames[j], "w"))) ABORT("Could not create temporary file.");
			for (i = 0; i < line_count; ++i) fwrite(lines[i], LINE_LEN, 1, tempfiles[j]);
			fclose(tempfiles[j]);
			line_count = 0;
			if (output) {
				sprintf(outbuf, "%s", elapsed(line, t2, (t3 = time(NULL))));
				fortwrite(outbuf, munit, TRUE);
				sprintf(outbuf, "  ==> Subset %d of %d",  j+2, group_count);
				fortwrite(outbuf, munit, TRUE);
				fortwrite("  Reading...", munit, FALSE);
			}
			t2 = t3;
		}
		if (!fgets(line, sizeof(line), catfile)) ABORT("Error reading unsorted catalog.");
	}
	fclose(catfile);
	catfile = NULL;

	//--------------------------------------//
	// sort the data in currently in memory //
	//--------------------------------------//
	if (*interrupt != 0) ABORT("Catalog sort interrupted");
	if (output) {
		sprintf(outbuf, "%s", elapsed(line, t2, (t3 = time(NULL))));
		fortwrite(outbuf, munit, TRUE);
		fortwrite("  Sorting...", munit, FALSE);
	}
	qsort(lines, line_count, sizeof(char *), catsort_compare);
	if (output) {
		sprintf(outbuf, "%s", elapsed(line, t3, (t2 = time(NULL))));
		fortwrite(outbuf, munit, TRUE);
		fortwrite("  Writing...", munit, FALSE);
	}
	if (tempfile_count > 0) {
		j = tempfile_count++;
		temp_filenames = (char **)realloc(temp_filenames, tempfile_count * sizeof(char *));
		tempfiles = (FILE **)realloc(tempfiles, tempfile_count * sizeof(FILE *));
		if (!(temp_filenames[j] = tempnam(NULL, "sort"))) ABORT("Could not create temporary filename.");
		if (!(tempfiles[j] = fopen(temp_filenames[j], "w"))) ABORT("Could not create temporary file.");
		for (i = 0; i < line_count; ++i) fwrite(lines[i], LINE_LEN, 1, tempfiles[j]);
		fclose(tempfiles[j]);
	}
	//----------------------------------------------------------//
	// output the catalog header, noting the correct sort order //
	//----------------------------------------------------------//
	*current_numb = -3;
	if (*interrupt != 0) ABORT("Catalog sort interrupted");
	catfile = fopen(catfilename, "w");
	if (!catfile) ABORT("Cannot open catalog for writing.");
	for (i = 0; i < header_line_count; ++i) {
		if (!strncmp(header_lines[i], "     Pathnames Not Sorted", 25)
			 || !strncmp(header_lines[i], "     Sort Order:", 16)) {
			fprintf(catfile, "     Sort Order: ");
			for (j = 0; j < 6; ++j) fprintf(catfile, "%c", '@' + sort_order[j]);
			fprintf(catfile, "\n");
		}
		else {
			if (header_lines[i][0] == '\0') {
				fprintf(catfile, "\n");
			}
			else {
				fprintf(catfile, "%s\n", header_lines[i]);
			}
		}
	}

	//---------------------------//
	// output the pathname lines //
	//---------------------------//
	if (tempfile_count > 0) {
		//-----------------//
		// prime the merge //
		//-----------------//
		done = malloc((size_t)tempfile_count * sizeof(int32_t));
		for (j = 0; j < tempfile_count; ++j) {
			if (!(tempfiles[j] = fopen(temp_filenames[j], "r"))) ABORT("Could not open temporary file.");
			if (!fread(lines[j], LINE_LEN, 1, tempfiles[j])) ABORT("Error reading temporary file.");
			done[j] = 0;
		}
		if (output) {
			sprintf(outbuf, "%s", elapsed(line, t2, (t3 = time(NULL))));
			fortwrite(outbuf, munit, TRUE);
			sprintf(outbuf, "  ==> Combining %d subsets", tempfile_count);
			fortwrite(outbuf, munit, TRUE);
			fortwrite("  Merging...", munit, FALSE);
		}
		t2 = t3;
	}
	sprintf(format, "%%6d  %%s/%%s/%%s/%%s/%%s/%%s/%%s/\n");
	for (i = 0; i < total_line_count; ++i) {
		if (*interrupt != 0) ABORT("Catalog sort interrupted");
		if (tempfile_count > 0) {
			for (j = 0; j < tempfile_count; ++j) {
				if (!done[j]) {
					k = j;
					break;
				}
			}
			for (++j; j < tempfile_count; ++j) {
				if (memcmp(lines[j], lines[k], LINE_LEN) < 0) k = j;
			}
			memcpy(line, lines[k], LINE_LEN);
			if (!fread(lines[k], LINE_LEN, 1, tempfiles[k])) {
				done[k] = 1;
				fclose(tempfiles[k]);
			}
		}
		else {
			memcpy(line, lines[i], LINE_LEN);
		}
		strcpy(misc_info, line + sizeof(parts));
		//---------------------------------------------------------//
		// put the parts back into ABCDEF order and null-terminate //
		//---------------------------------------------------------//
		for (j = 0; j < 6; ++j) {
			int32_t ii;
			k = sort_order[j] - 1;
			strncpy(parts[k], line + j * PART_PAD_LEN, PART_PAD_LEN);
			for (ii = PART_PAD_LEN-1; ii >= 0; --ii) {
				if (parts[k][ii] != ' ') break;
				parts[k][ii] = '\0';
			}
			if (parts[k][0] == ' ') {
				if (parts[k][1] == ' ') {
					memmove(&parts[k][0], &parts[k][2], strlen(parts[k])-1);
				}
				else {
					//-------------------------------------------------//
					// restore sortable dates back to DDMMMYYYY format //
					//-------------------------------------------------//
					restore_date(&parts[k][0]);
				}
			}
		}
		if (i == 999999) sprintf(format, "%%7d %%s/%%s/%%s/%%s/%%s/%%s/%%s/\n");
		sprintf(line, format, i+1, misc_info, parts[0], parts[1], parts[2], parts[3], parts[4], parts[5]);
		fprintf(catfile, "%s", line);
	}
	for (j = 0; j < tempfile_count; ++j) {
		if (!done[j]) {
			fclose(tempfiles[j]);
			tempfiles[j] = NULL;
		}
	}
	fclose(catfile);
	catfile = NULL;


	//-----------------------------------------------------------------//
	// output a condensed catalog from the sorted catalog if specified //
	//-----------------------------------------------------------------//
	if (*interrupt != 0) ABORT("Catalog sort interrupted");
	if (make_condensed) {
		char last_parts[6][PART_PAD_LEN];
		char range_start[PART_PAD_LEN];
		const char * placeholder = "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ";
		int32_t yr, mon, day, last_yr, last_mon, last_day, gap, any_gaps;
		int32_t first_non_matching_part;

		//----------------------------//
		// switch from *.dsc to *.dsd //
		//----------------------------//
		strcpy(line, catfilename);
		line[strlen(line)-1] = 'd';
		condcatfile = fopen(line, "w");
		if (!condcatfile) ABORT("Cannot open condensed catalog for writing.");
		if(!(catfile = fopen(catfilename, "r"))) ABORT("Could not open new catalog file for reading.");
		if (output) {
			sprintf(outbuf, "%s", elapsed(line, t2, (t3 = time(NULL))));
			fortwrite(outbuf, munit, TRUE);
			fortwrite("Creating condensed catalog:", munit, TRUE);
			fortwrite("  Writing...", munit, FALSE);
		}
		t2 = t3;

		//-------------------------//
		// output the header lines //
		//-------------------------//
		for (i = 0; i < header_line_count; ++i) {
			if(!fgets(line, LINE_LEN, catfile)) ABORT("Could not read catalog file.");
			if (strstr(header_lines[i], "Catalog of Record Pathnames")) {
				j = strchr(header_lines[i], 'H') - header_lines[i];
				strncpy(line, header_lines[i], j);
				sprintf(&line[j],
					"HECDSS Condensed %s\n",
					strstr(header_lines[i], "Catalog of Record Pathnames"));
				fprintf(condcatfile, line);
			}
			else if (!strncmp(header_lines[i], "     Pathnames Not Sorted", 25)
				 || !strncmp(header_lines[i], "     Sort Order:", 16)) {
				fprintf(condcatfile, "     Sort Order: ");
				for (j = 0; j < 6; ++j) fprintf(condcatfile, "%c", '@' + sort_order[j]);
				fprintf(condcatfile, "\n");
			}
			else if (strstr(header_lines[i], "Ref.")) {
				if(!fgets(line, LINE_LEN, catfile)) ABORT("Could not read catalog file.");
				if(!fgets(line, LINE_LEN, catfile)) ABORT("Could not read catalog file.");
				break;
			}
			else {
				fprintf(condcatfile, "%s\n", header_lines[i]);
			}
		}
		fprintf(condcatfile, " %s  ", ljust(line, "Tag", max(3, max_tag)));
		fprintf(condcatfile, "%s  ", ljust(line, "A Part", max(6, max_len[0])));
		fprintf(condcatfile, "%s  ", ljust(line, "B Part", max(6, max_len[1])));
		fprintf(condcatfile, "%s  ", ljust(line, "C Part", max(6, max_len[2])));
		fprintf(condcatfile, "%s  ", ljust(line, "F Part", max(6, max_len[5])));
		fprintf(condcatfile, "%s  ", ljust(line, "E Part", max(6, max_len[4])));
		fprintf(condcatfile, "D Part\n");

		//---------------------------//
		// output the data set lines //
		//---------------------------//
		for (i = 0; i < 6; ++i) last_parts[i][0] = '\0';
		yr = mon = day = last_yr = last_mon = last_day = gap = any_gaps = 0;
		for (i = 0; i < total_line_count; ++i) {
			if (*interrupt != 0) ABORT("Catalog sort interrupted");
			if(!fgets(line, LINE_LEN, catfile)) ABORT("Could not read catalog file.");
			pos = first_pos;
			offset = 0;
			if (gap) any_gaps = 1;

			//----------------------------------//
			// put parts in sort order (ABCFED) //
			//----------------------------------//
			for (j = 0; j < 6; ++j) {
				k = part_order[j];
				offset += pos+1;
				pos = strchr(&line[offset], '/') - &line[offset];
				strncpy(parts[k], &line[offset], pos);
				parts[k][pos] = '\0';
				if (strlen(parts[k]) == 0) strcpy(parts[k], "(null)");
			}

			//----------------------------------------------------------------------------//
			// determine the first (in sort order) part to differ from the previous parts //
			//----------------------------------------------------------------------------//
			for (k = 0; k < 6; ++k) {
				if (strcmp(parts[k], last_parts[k])) {
					first_non_matching_part = k;
					break;
				}
			}

			//-----------------------------------//
			// determine if the E part is a date //
			//-----------------------------------//
			mon = get_month(parts[5], strlen(parts[5]));
			if (mon) {
				day = atoi(parts[5]);
				yr = atoi(&parts[5][5]);
			}
			if (first_non_matching_part < 5 || mon == 0) {
				//---------------------------------------------------//
				// not a data set continuation, so output a new line //
				//---------------------------------------------------//
				if (last_yr) {
					//---------------------------------------//
					// finish the previous line if necessary //
					//---------------------------------------//
					if (range_start[0] && strcmp(last_parts[5], range_start)) {
						fprintf(condcatfile, " - %s", last_parts[5]);
						if (gap) fprintf(condcatfile, " *");
					}
				}
				//----------------//
				// output the tag //
				//----------------//
				for (k = sizeof(parts); line[k] == ' '; ++k) ;
				fprintf(condcatfile, "\n %s  ", ljust(line, &line[k], max(3, max_tag)));
				//--------------------------------------------//
				// output the placeholders for matching parts //
				//--------------------------------------------//
				for (j = 0; j < first_non_matching_part; ++j) {
					fprintf(condcatfile, "%s   ", ljust(line, placeholder, max(5, max_len[part_order[j]]-1)));
				}
				//---------------------------------------//
				// output parts after the matching parts //
				//---------------------------------------//
				for (; j < 6; ++j) {
					if (j < 5) {
						fprintf(condcatfile, "%s  ", ljust(line, parts[j], max(6, max_len[part_order[j]])));
					}
					else {
						fprintf(condcatfile, "%s", parts[j]);
					}
				}
				//---------------------------------------//
				// save info for comparison on next line //
				//---------------------------------------//
				if (mon == 0) range_start[0] = '\0';
				else strcpy(range_start, parts[5]);
				last_yr  = yr;
				last_mon = mon;
				last_day = day;
				yr = mon = day = gap = 0;
			}
			else if (yr && last_yr) {
				//-----------------------------------------------------------------//
				// data set continuation, determine whether any blocks are skipped //
				//-----------------------------------------------------------------//
				if      (!strcmp(parts[4], "1MIN")
							|| !strcmp(parts[4], "2MIN")
							|| !strcmp(parts[4], "3MIN")
							|| !strcmp(parts[4], "4MIN")
							|| !strcmp(parts[4], "5MIN")
							|| !strcmp(parts[4], "6MIN")
							|| !strcmp(parts[4], "10MIN")
							|| !strcmp(parts[4], "12MIN")
					 || !strcmp(parts[4], "IR-DAY")) {
					//----------------------//
					// block length = 1 Day //
					//----------------------//
					if (yr - last_yr > 1) {
						gap += 1;
					}
					else if (yr - last_yr == 1) {
						gap += last_mon != 12 || last_day != 31 || mon != 1 || day != 1;
					}
					else {
						if (mon - last_mon > 1) {
							gap += 1;
						}
						else if (mon - last_mon == 1) {
							if (day != 1) gap += 1;
							switch (last_mon) {
								case 1  :
								case 3  :
								case 5  :
								case 7  :
								case 8  :
								case 10 :
								case 12 :
									gap += last_day != 31;
									break;

								case 4  :
								case 6  :
								case 9  :
								case 11 :
									gap += last_day != 30;
									break;

								case 2  :
									if (last_yr % 4 || (!(last_yr % 100) && last_yr % 400)) {
										gap += last_day != 28;
									}
									else {
										gap += last_day != 29;
									}
							}
						}
						else {
							gap += day - last_day > 1;
						}
					}
				}
				else if (!strcmp(parts[4], "15MIN")
					 || !strcmp(parts[4], "20MIN")
					 || !strcmp(parts[4], "30MIN")
					 || !strcmp(parts[4], "1HOUR")
					 || !strcmp(parts[4], "2HOUR")
					 || !strcmp(parts[4], "3HOUR")
					 || !strcmp(parts[4], "4HOUR")
					 || !strcmp(parts[4], "6HOUR")
					 || !strcmp(parts[4], "8HOUR")
					 || !strcmp(parts[4], "12HOUR")
					 || !strcmp(parts[4], "IR-MONTH")) {
					//------------------------//
					// block length = 1 Month //
					//------------------------//
					gap += ((yr - last_yr) * 365 + (mon - last_mon) * 30 + day - last_day) > 45;  // ~= 1.5 months
				}
				else if (!strcmp(parts[4], "1DAY")
					 || !strcmp(parts[4], "IR-YEAR")) {
					//-----------------------//
					// block length = 1 Year //
					//-----------------------//
					gap += ((yr - last_yr) * 365 + (mon - last_mon) * 30 + day - last_day) > 540; // ~= 1.5 years
				}
				else if (!strcmp(parts[4], "1WEEK")
					 || !strcmp(parts[4], "TRI-MONTH")
					 || !strcmp(parts[4], "SEMI-MONTH")
					 || !strcmp(parts[4], "1MON")
					 || !strcmp(parts[4], "IR-DECADE")) {
					//-------------------------//
					// block length = 1 Decade //
					//-------------------------//
					gap += ((yr - last_yr) * 365 + (mon - last_mon) * 30 + day - last_day) > 5400;  // ~= 15 years
				}
				else if (!strcmp(parts[4], "1YEAR")) {
					//--------------------------//
					// block length = 1 Century //
					//--------------------------//
					gap += ((yr - last_yr) * 365 + (mon - last_mon) * 30 + day - last_day) > 54000;  // ~= 150 years
				}
				//---------------------------------------//
				// save info for comparison on next line //
				//---------------------------------------//
				last_yr  = yr;
				last_mon = mon;
				last_day = day;
			}
			else {
				gap = 0;
			}
			//----------------------------------------//
			// save parts for comparison on next line //
			//----------------------------------------//
			memcpy(last_parts, parts, sizeof(parts));
		}
		//---------------------------------------//
		// finish up the last line, if necessary //
		//---------------------------------------//
		if (last_yr) {
			if (range_start[0] && strcmp(last_parts[5], range_start)) {
				fprintf(condcatfile, " - %s", last_parts[5]);
				if (gap) fprintf(condcatfile, " *");
			}
		}
		fprintf(condcatfile, "\n");

		//----------------------------------------------//
		// output the missing periods note if necessary //
		//----------------------------------------------//
		if (any_gaps) fprintf(condcatfile, "   *  Record time span has missing periods.\n");
		fclose(condcatfile);
		condcatfile = NULL;
		fclose(catfile);
		catfile = NULL;
	}

	//------------//
	// wrap it up //
	//------------//
	CLEANUP("");
	if (output) {
		char buf[256];
		sprintf(buf, "%s", elapsed(line, t2, (t3 = time(NULL))));
		fortwrite(buf, munit, TRUE);
		fortwrite("  Total.....", munit, FALSE);
		sprintf(buf, "%s", elapsed(line, t1, t3));
		fortwrite(buf, munit, TRUE);
	}
}

void catsort_(
	char *filename,
	int32_t *sort_order,
	int32_t *make_condensed,
	int32_t *interrupt,
	int32_t *current_numb,
	int32_t *output,
	int32_t *munit,
	int32_t filename_len) {

	char *catfilename = NULL;
	char outbuf[128];
	int32_t i;
	if (filename_len < 5) {
		fortwrite("", *munit, TRUE);
		fortwrite("Catalog Sort Error: Invalid catalog file name.", *munit, TRUE);
		return;
	}
	catfilename = (char *)malloc((size_t)filename_len + 1);
	if (catfilename == NULL) {
		fortwrite("", *munit, TRUE);
		fortwrite("Catalog Sort Error: Memory allocation error.", *munit, TRUE);
		return;
	}
	strncpy(catfilename, filename, filename_len);
	catfilename[filename_len] = '\0';
	for (i = filename_len-1; i >= 0; --i) {
		if (catfilename[i] != ' ') break;
		catfilename[i] = '\0';
	}
	if (strlen(catfilename) < 5 || stricmp(&catfilename[strlen(catfilename)-4], ".dsc")) {
		fortwrite("", *munit, TRUE);
		fortwrite("Catalog Sort Error: Invalid catalog file name.", *munit, TRUE);
		return;
	}

	catsort(catfilename, sort_order, *make_condensed, interrupt, current_numb, *output, *munit);

	free(catfilename);
	return;
}


// void main(int32_t argc, char* argv[]) {
//
//    int32_t sort_order[] = {1, 2, 3, 6, 5, 4};
//    catsort(argv[1], sort_order, 1);
// }

