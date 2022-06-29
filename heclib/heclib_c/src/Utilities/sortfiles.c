#include <stdio.h>
#include <sys/types.h>
#ifdef _MSC_VER
#include <process.h>
#else
#include <unistd.h>
#endif
#include "heclib.h"

/* sort.c - sort lines of text (with all kinds of options).
   Copyright 1989 Free Software Foundation
		  Written December 1988 by Mike Haertel.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation. */

/* MS-DOS port (c) 1990 by Thorsten Ohl, td12@ddagsi3.bitnet
   This port is also distributed under the terms of the
   GNU General Public License as published by the
   Free Software Foundation.

   $Header: e:/gnu/sort/RCS/sort.c'v 0.3.0.4 90/08/26 19:03:05 tho Exp $
 */



static char version[] = "GNU sort, version 0.3";
/* std.h - automagically adapt to old and new compilers.
   In the Public Domain; written by Mike Haertel. */

/* MS-DOS port (c) 1990 by Thorsten Ohl, td12@ddagsi3.bitnet
   This port is distributed under the terms of the
   GNU General Public License as published by the
   Free Software Foundation.

   $Header: e:/gnu/sort/RCS/std.h'v 0.3.0.2 90/08/26 19:03:25 tho Exp $
 */

#ifdef _MSC_VER
#define __STDC__ 1
#endif
#if __STDC__

#include <stddef.h>
#include <limits.h>

typedef void *PTR;
typedef const void *PTRCONST;

#define AND ,
#define DEFUN(F, L, P) F(P)
#define EXFUN(F, P) F P

#else

#define const
#define volatile

/* The following approximations of <stddef.h> and <limits.h> are appropriate
   for most machines. */
#ifdef _MSC_VER
typedef int ptrdiff_t;
typedef unsigned int size_t;
#define NULL 0
#endif
//#define offsetof(T, M) ((size_t)&((T *) 0)->(M))

#define CHAR_BIT 8
#define SCHAR_MIN -128
#define SCHAR_MAX 127
#define UCHAR_MAX 255
#define CHAR_MIN ((char) UCHAR_MAX < 0 ? SCHAR_MIN : 0)
#define CHAR_MAX ((char) UCHAR_MAX < 0 ? SCHAR_MAX : UCHAR_MAX)
#define SHRT_MIN -32768
#define SHRT_MAX 32767
#define USHRT_MAX 65535
#define INT_MIN (sizeof (int) == 2 ? -32768 : -2147483648)
#define INT_MAX (sizeof (int) == 2 ? 32767 : 2147483647)
#define UINT_MAX (sizeof (int) == 2 ? 65535 : 4294967295)
#define LONG_MIN -2147483648L
#define LONG_MAX 2147483647L
#define ULONG_MAX 4294967295

typedef char *PTR;
typedef const char *PTRCONST;

#define AND ;
#define DEFUN(F, L, P) F L P ;
#define EXFUN(F, P) F()

#endif

/* Deal with <ctype.h> lossage. */
#include <ctype.h>

#ifndef isascii

#define ISALNUM(C) isalnum(C)
#define ISALPHA(C) isalpha(C)
#define ISCNTRL(C) iscntrl(C)
#define ISDIGIT(C) isdigit(C)
#define ISGRAPH(C) isgraph(C)
#define ISLOWER(C) islower(C)
#define ISPRINT(C) isprint(C)
#define ISPUNCT(C) ispunct(C)
#define ISSPACE(C) isspace(C)
#define ISUPPER(C) isupper(C)
#define ISXDIGIT(C) isxdigit(C)
#define TOLOWER(C) tolower(C)
#define TOUPPER(C) toupper(C)

#else

#define ISALNUM(C) (isascii(C) && isalnum(C))
#define ISALPHA(C) (isascii(C) && isalpha(C))
#define ISCNTRL(C) (isascii(C) && iscntrl(C))
#define ISDIGIT(C) (isascii(C) && isdigit(C))
#define ISGRAPH(C) (isascii(C) && isgraph(C))
#define ISLOWER(C) (isascii(C) && islower(C))
#define ISPRINT(C) (isascii(C) && isprint(C))
#define ISPUNCT(C) (isascii(C) && ispunct(C))
#define ISSPACE(C) (isascii(C) && isspace(C))
#define ISUPPER(C) (isascii(C) && isupper(C))
#define ISXDIGIT(C) (isascii(C) && isxdigit(C))
#define TOLOWER(C) (ISUPPER(C) ? tolower(C) : (C))
#define TOUPPER(C) (ISLOWER(C) ? toupper(C) : (C))

#endif

/* Declaring this here should be safe.  Some losing <errno.h>'s don't. */
#ifndef _MSC_VER
extern int errno;
#endif /* not _MSC_VER */

/* Adapt variable arguments to new implementations (with <stdarg.h>)
   or old (which are assumed to have <varargs.h>). */

#if __STDC__

#include <stdarg.h>

#define VA_ALIST ...
#define VA_DCL ...
#define VA_LIST va_list
#define VA_START(AP, LASTARG) va_start(AP, LASTARG)
#define VA_ARG(AP, TYPE) va_arg(AP, TYPE)
#define VA_END(AP) va_end(AP)

#define VA_DEFUN(F, L, P) F(P)

#else

#include <varargs.h>

#define VA_ALIST va_alist
#define VA_DCL va_dcl
#define VA_LIST va_list
#define VA_START(AP, LASTARG) va_start(AP)
#define VA_ARG(AP, TYPE) va_arg(AP, TYPE)
#define VA_END(AP) va_end(AP)

#define VA_DEFUN(F, L, P) F L P

#endif

/* Declarations of traditional library routines. */
#ifdef _MSC_VER
#include <stdlib.h>
extern void *xmalloc (size_t size);
extern void *xrealloc (void *ptr, size_t size);
#else /* not _MSC_VER */
extern double EXFUN(atof, (const char *));
extern int EXFUN(atoi, (const char *));
extern long int EXFUN(atol, (const char *));
extern int EXFUN(rand, (void));
#if !defined __linux__ && !defined __APPLE__
extern void EXFUN(srand, (int));
#endif
//extern PTR EXFUN(calloc, (size_t, size_t));
extern void EXFUN(free, (PTR));
//extern PTR EXFUN(malloc, (size_t));
//extern PTR EXFUN(realloc, (PTR, size_t));
extern void EXFUN(abort, (void));
extern void EXFUN(exit, (int));
extern char *EXFUN(getenv, (const char *));
extern int EXFUN(system, (const char *));
extern void EXFUN(qsort, (PTR, size_t, size_t,
			  int EXFUN((*), (PTRCONST, PTRCONST))));
extern int EXFUN(abs, (int));
extern long int EXFUN(labs, (long int));

#ifdef X_strerror
extern char *EXFUN(strerror, (int));
#endif
#endif /* not _MSC_VER */
#ifndef _MSC_VER
//#include "unix.h"
#endif /* not _MSC_VER */

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>

#ifdef _MSC_VER
#include <process.h>
static void cleanup (void);
static void assert_lines (int lines);
void *xmalloc (size_t size);
void *xrealloc (void *ptr, size_t size);
static FILE *xfopen (char *file, char *how);
static void xfclose (FILE *fp);
static void xfwrite (char *buf, int size, int nelem, FILE *fp);
static char *tempname (void);
static void zaptemp (char *name);
static void inittables (void);
static void initbuf (struct buffer *buf, int alloc);
static int fillbuf (struct buffer *buf, FILE *fp);
static void initlines (struct lines *lines, int alloc);
static char *begfield (struct line *line, struct keyfield *key);
static char *limfield (struct line *line, struct keyfield *key);
static void findlines (struct buffer *buf, struct lines *lines);
static int fraccompare (char *a, char *b);
static int numcompare (char *a, char *b);
static int getmonth (char *s, int len);
static int keycompare (struct line *a, struct line *b);
static int compare (struct line *a, struct line *b);
static int checkfp (FILE *fp);
static void mergefps (FILE **fps, int nfps, FILE *ofp);
static void sortlines (struct line *lines, int nlines, struct line *temp);
static int check (char **files, int nfiles);
static void merge (char **files, int nfiles, FILE *ofp);
static void sort (char **files, int nfiles, FILE *ofp);
static void insertkey (struct keyfield *key);
static void usage (void);
static void badfieldspec (char *s);
static void inthandler (void);
int main (int argc, char **argv);
#endif /* _MSC_VER */

/* A few useful macros. */
#define ISBLANK(c) ((c) == ' ' || (c) == '\t')
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define UCHAR_LIM (UCHAR_MAX + 1)
#define UCHAR(c) ((unsigned char) (c))

/* Table of digits. */
static int digits[UCHAR_LIM];

/* Table of white space. */
static int blanks[UCHAR_LIM];

/* Table of non-printing characters. */
static int nonprinting[UCHAR_LIM];

/* Table of non-dictionary characters (not letters, digits, or blanks). */
static int nondictionary[UCHAR_LIM];

/* Translation table folding upper case to lower. */
static char fold_tolower[UCHAR_LIM];

/* Table mapping 3-letter month names to integers.
   Alphabetic order allows binary search. */
static struct month {
  char *name;
  int val;
} monthtab[] = {
  "apr", 4,
  "aug", 8,
  "dec", 12,
  "feb", 2,
  "jan", 1,
  "jul", 7,
  "jun", 6,
  "mar", 3,
  "may", 5,
  "nov", 11,
  "oct", 10,
  "sep", 9
};

/* During the merge phase, the number of files to merge at once. */
#ifdef _MSC_VER
/* 14 (= 20 - 5 - 1) is a hard upper limit for MeSsy DOS versions <3.2
   (and a soft one for later versions...) */
#define NMERGE 12
#else /* not _MSC_VER */
#define NMERGE 16
#endif /* not _MSC_VER */

/* Initial buffer size for in core sorting.  Will not grow unless a
   line longer than this is seen. */
#ifdef _MSC_VER
static int sortalloc = 32767;
#else /* not _MSC_VER */
static int sortalloc = 524288;
#endif /* not _MSC_VER */

/* Initial buffer size for in core merge buffers.  Bear in mind that
   up to NMERGE * mergealloc bytes may be allocated for merge buffers. */
static int mergealloc = 16384;

/* Guess of average line length. */
static int linelength = 30;

/* Prefix for temporary file names. */
#ifdef _MSC_VER
static char *prefix;
#else /* not _MSC_VER */
static char *prefix = "/tmp";
#endif /* not _MSC_VER */

/* Flag to reverse the order of all comparisons. */
static int reverse;

/* Tab character separating fields.  If NUL, then fields are separated
   by the empty string between a non-whitespace character and a whitespace
   character. */
static char tab;

/* Flag to remove consecutive duplicate lines from the output.
   Only the last of a sequence of equal lines will be output. */
static int unique;

/* Lines are held in core as counted strings. */
struct line
{
  char *text;			/* Text of the line. */
  int length;			/* Length not including final newline. */
  char *keybeg;			/* Start of first key. */
  char *keylim;			/* Limit of first key. */
};

#ifdef _MSC_VER
/* Using _huge line arrays under MS-DOS is terribly inefficient, so we
   impose an upper limit on the number of lines cosidered at once.  The
   user can break the input into digestable pieces by using the `-S' option
   to adjust the input buffer size.
   This only matters for files with very short lines ( < 8 chars).  */
static int maxlines
  = (int) (((1L << 16) - 1L) / sizeof (struct line));

void
assert_lines (int lines)
{
  if (lines >= maxlines)
    {
      fprintf (stderr,
	       "sort: the number of lines per input buffer is restricted in\n"
	       "      the MS-DOS version.  For files with short lines, use\n"
	       "      the `-S <num>' option to reduce the buffer size.\n");
      cleanup ();
      exit (-1);
    }
}
#endif /* _MSC_VER */

/* Arrays of lines. */
struct lines
{
  struct line *lines;		/* Dynamically allocated array of lines. */
  int used;			/* Number of slots used. */
  int alloc;			/* Number of slots allocated. */
};

/* Input buffers. */
struct buffer
{
  char *buf;			/* Dynamically allocated buffer. */
  int used;			/* Number of bytes used. */
  int alloc;			/* Number of bytes allocated. */
  int left;			/* Number of bytes left after line parsing. */
};

/* Lists of key field comparisons to be tried. */
static struct keyfield
{
  int sword;			/* Zero-origin 'word' to start at. */
  int schar;			/* Additional characters to skip. */
  int skipsblanks;		/* Skip leading white space at start. */
  int eword;			/* Zero-origin first word after field. */
  int echar;			/* Additional characters in field. */
  int skipeblanks;		/* Skip trailing white space at finish. */
  int *ignore;			/* Boolean array of characters to ignore. */
  char *translate;		/* Translation applied to characters. */
  int numeric;			/* Flag for numeric comparison. */
  int month;			/* Flag for comparison by month name. */
  int reverse;			/* Reverse the sense of comparison. */
  struct keyfield *next;	/* Next keyfield to try. */
} keyhead;

/* The list of temporary files. */
static struct tempnode
{
  char *name;
  struct tempnode *next;
} temphead;

/* Clean up any remaining temporary files. */
static void
cleanup()
{
  struct tempnode *node;

  for (node = temphead.next; node; node = node->next)
    remove(node->name);
}

/* Interfaces to library routines. */
#ifdef _MSC_VER

void *
xmalloc (size_t size)
{
  void *r = malloc (size);

  if (r)
    return r;
  fprintf (stderr, "sort: memory exausted\n");
  cleanup ();
  exit (-1);
}

void *
xrealloc (void *ptr, size_t size)
{
  void *r = realloc (ptr, size);

  if (r)
    return r;
  fprintf (stderr, "sort: memory exhausted\n");
  cleanup ();
  exit (-1);
}

#else /* not _MSC_VER */

static char *xmalloc(size_t n)
{
  char *r = malloc(n);

  if (r)
    return r;
  fprintf(stderr, "sort: memory exausted\n");
  cleanup();
  exit(-1);
}

static char *
xrealloc(p, n)
     char *p;
     int n;
{
  char *r = realloc(p, n);

  if (r)
    return r;
  fprintf(stderr, "sort: memory exhausted\n");
  cleanup();
  exit(-1);
}

#endif /* not _MSC_VER */

static FILE *
xfopen(file, how)
     char *file, *how;
{
  FILE *fp = strcmp(file, "-") ? fopen(file, how) : stdin;

  if (fp)
    return fp;
  fprintf(stderr, "sort: cannot open %s (%s)\n", file, how);
 // fprintf(stderr, "sort: cannot open %s (%s): %s\n", file, how, strerror(errno))  //
  cleanup();
  exit(-1);
}

static void
xfclose(fp)
     FILE *fp;
{
  fflush(fp);
  if (fp != stdin && fp != stdout)
    if (fclose(fp) != 0)
      {
	fprintf(stderr, "sort: error in fclose\n");
	perror( "Error ");
	cleanup();
	exit(-1);
      }
}

static void
xfwrite(buf, size, nelem, fp)
     char *buf;
     int size, nelem;
     FILE *fp;
{
  if (fwrite(buf, size, nelem, fp) < 0)
    {
      fprintf(stderr, "sort: error in fwrite\n");
	  perror( "Error ");
      cleanup();
      exit(-1);
    }
}

/* Return a name for a temporary file. */
static char *
tempname()
{
  static int seq=0;
  int len = (int)strlen(prefix);
  size_t nameLen = len + 22;
  char *name = (char *)xmalloc(nameLen);
  struct tempnode *node =
    (struct tempnode *) xmalloc(sizeof (struct tempnode));

#ifdef _MSC_VER
  if (len && prefix[len - 1] != '\\')
    sprintf_s(name, nameLen, "%s\\srt%5.5x.%3.3x", prefix, _getpid(), ++seq);
  else
    sprintf_s(name, nameLen, "%ssrt%5.5x.%3.3x", prefix, _getpid(), ++seq);
#else /* not _MSC_VER */
  if (len && prefix[len - 1] != '/')
    sprintf(name, "%s/sort%6.6d%5.5d", prefix, getpid(), ++seq);
  else
    sprintf(name, "%ssort%6.6d%5.5d", prefix, getpid(), ++seq);
#endif /* not _MSC_VER */
  node->name = name;
  node->next = temphead.next;
  temphead.next = node;
  return name;
}

/* Search through the list of temporary files for the given name;
   remove it if it is found on the list. */
static void
zaptemp(name)
     char *name;
{
  struct tempnode *node, *temp;

  for (node = &temphead; node->next; node = node->next)
    if (!strcmp(name, node->next->name))
      break;
  if (node->next)
    {
      temp = node->next;
      remove(temp->name);
      free(temp->name);
      node->next = temp->next;
      free((char *) temp);
    }
}

/* Initialize the character class tables. */
static void
inittables()
{
  int i;

  for (i = 0; i < UCHAR_LIM; ++i)
    {
      if (ISBLANK(i))
	blanks[i] = 1;
      if (ISDIGIT(i))
	digits[i] = 1;
      if (!ISPRINT(i))
	nonprinting[i] = 1;
      if (!ISALNUM(i) && !ISBLANK(i))
	nondictionary[i] = 1;
      if (ISUPPER(i))
	fold_tolower[i] = tolower(i);
      else
	fold_tolower[i] = i;
    }
}

/* Initialize BUF allocating ALLOC bytes initially. */
static void
initbuf(buf, alloc)
     struct buffer *buf;
     int alloc;
{
  buf->alloc = alloc;
  buf->buf = xmalloc(buf->alloc);
  buf->used = buf->left = 0;
}

/* Fill BUF reading from FP, moving buf->left bytes from the end
   of buf->buf to the beginning first.	If EOF is reached and the
   file wasn't terminated by a newline, supply one.  Return a count
   of bytes actually read. */
static int
fillbuf(buf, fp)
     struct buffer *buf;
     FILE *fp;
{
  int cc, total = 0;

  memmove(buf->buf, buf->buf + buf->used - buf->left, buf->left);
  buf->used = buf->left;

  while (!feof(fp) && !memchr(buf->buf, '\n', buf->used))
    {
      if (buf->used == buf->alloc)
#ifdef _MSC_VER
	{
	  fprintf (stderr,
		   "sort: lines longer than 32k are not supported under\n"
		   "      MS-DOS because of performance considerations.\n");
	  cleanup ();
	  exit (-1);
	}
#else /* not _MSC_VER */
	buf->buf = xrealloc(buf->buf, buf->alloc *= 2);
#endif /* not _MSC_VER */
      cc = (int)fread(buf->buf + buf->used, 1, buf->alloc - buf->used, fp);
      if (cc < 0)
	{
	  fprintf(stderr, "sort: read error \n");
	  perror("Error ");
	  cleanup();
	  exit(-1);
	}
      buf->used += cc;
      total += cc;
    }

  if (feof(fp) && buf->used && buf->buf[buf->used - 1] != '\n')
    {
      if (buf->used == buf->alloc)
#ifdef _MSC_VER
	{
	  fprintf (stderr,
		   "sort: lines longer than 32k are not supported under\n"
		   "      MS-DOS because of performance considerations.\n");
	  cleanup ();
	  exit (-1);
	}
#else /* not _MSC_VER */
	buf->buf = xrealloc(buf->buf, buf->alloc *= 2);
#endif /* not _MSC_VER */
      buf->buf[buf->used++] = '\n';
      ++total;
    }

  return total;
}

/* Initialize LINES, allocating space for ALLOC lines initially. */
static void
initlines(lines, alloc)
     struct lines *lines;
     int alloc;
{
  lines->alloc = alloc;
#ifdef _MSC_VER
  assert_lines (lines->alloc);
#endif /* _MSC_VER */
  lines->lines = (struct line *) xmalloc(lines->alloc * sizeof (struct line));
  lines->used = 0;
}

/* Return a pointer to the first character of a field. */
static char *
begfield(line, key)
     struct line *line;
     struct keyfield *key;
{
  register char *ptr = line->text, *lim = ptr + line->length;
  register int sword = key->sword, schar = key->schar;

  if (tab)
    while (ptr < lim && sword--)
      {
	while (ptr < lim && *ptr != tab)
	  ++ptr;
	if (ptr < lim)
	  ++ptr;
      }
  else
    while (ptr < lim && sword--)
      {
	while (ptr < lim && blanks[UCHAR(*ptr)])
	  ++ptr;
	while (ptr < lim && !blanks[UCHAR(*ptr)])
	  ++ptr;
      }

  if (key->skipsblanks)
    while (ptr < lim && blanks[UCHAR(*ptr)])
      ++ptr;

  while (ptr < lim && schar--)
    ++ptr;

  return ptr;
}

/* Find the limit of a field; i.e., a pointer to the first character
   after the field. */
static char *
limfield(line, key)
     struct line *line;
     struct keyfield *key;
{
  register char *ptr = line->text, *lim = ptr + line->length;
  register int eword = key->eword, echar = key->echar;

  if (tab)
    while (ptr < lim && eword--)
      {
	while (ptr < lim && *ptr != tab)
	  ++ptr;
	if (ptr < lim && (eword || key->skipeblanks))
	  ++ptr;
      }
  else
    while (ptr < lim && eword--)
      {
	while (ptr < lim && blanks[UCHAR(*ptr)])
	  ++ptr;
	while (ptr < lim && !blanks[UCHAR(*ptr)])
	  ++ptr;
      }

  if (key->skipeblanks)
    while (ptr < lim && blanks[UCHAR(*ptr)])
      ++ptr;

  while (ptr < lim && echar--)
    ++ptr;

  return ptr;
}

/* Find the lines in BUF, storing pointers and lengths in LINES.
   Also replace newlines with NULs. */
static void
findlines(buf, lines)
     struct buffer *buf;
     struct lines *lines;
{
  register char *beg = buf->buf, *lim = buf->buf + buf->used, *ptr;
  struct keyfield *key = keyhead.next;

  lines->used = 0;

  while (beg < lim && (ptr = memchr(beg, '\n', lim - beg)))
    {
      /* There are various places in the code that rely on a NUL
	 being at the end of in-core lines; NULs inside the lines
	 will not cause trouble, though. */
      *ptr = '\0';

      if (lines->used == lines->alloc)
	lines->lines =
	  (struct line *) xrealloc((char *) lines->lines,
				   (lines->alloc *= 2) * sizeof (struct line));

#ifdef _MSC_VER
      assert_lines (lines->alloc);
#endif /* _MSC_VER */

      lines->lines[(int)lines->used].text = beg;
      lines->lines[(int)lines->used].length = (int)(ptr - beg);

      /* Precompute the position of the first key for efficiency. */
      if (key)
	{
	  if (key->eword >= 0)
	    lines->lines[lines->used].keylim =
	      limfield(&lines->lines[lines->used], key);
	  else
	    lines->lines[lines->used].keylim = ptr;

	  if (key->sword >= 0)
	    lines->lines[lines->used].keybeg =
	      begfield(&lines->lines[lines->used], key);
	  else
	    {
	      if (key->skipsblanks)
		while (blanks[UCHAR(*beg)])
		  ++beg;
	      lines->lines[lines->used].keybeg = beg;
	    }
	}

      ++lines->used;
      beg = ptr + 1;
    }

  buf->left = (int)(lim - beg);
}

/* Compare two strings containing decimal fractions < 1.  Each string
   should begin with a decimal point followed immediately by the digits
   of the fraction.  Strings not of this form are considered to be zero. */
static int
fraccompare(a, b)
     register char *a, *b;
{
  register unsigned char tmpa = UCHAR(*a), tmpb = UCHAR(*b);

  if (tmpa == '.' && tmpb == '.')
    {
      do
	tmpa = UCHAR(*++a), tmpb = UCHAR(*++b);
      while (tmpa == tmpb && digits[tmpa]);
      if (digits[tmpa] && digits[tmpb])
	return tmpa - tmpb;
      if (digits[tmpa])
	{
	  while (tmpa == '0')
	    tmpa = UCHAR(*++a);
	  if (digits[tmpa])
	    return 1;
	  return 0;
	}
      if (digits[tmpb])
	{
	  while (tmpb == '0')
	    tmpb = UCHAR(*++b);
	  if (digits[tmpb])
	    return -1;
	  return 0;
	}
      return 0;
    }
  else if (tmpa == '.')
    {
      do
	tmpa = UCHAR(*++a);
      while (tmpa == '0');
      if (digits[tmpa])
	return 1;
      return 0;
    }
  else if (tmpb == '.')
    {
      do
	tmpb = UCHAR(*++b);
      while (tmpb == '0');
      if (digits[tmpb])
	return -1;
      return 0;
    }
  return 0;
}

/* Compare two strings as numbers without explicitly converting them to
   machine numbers.  Comparatively slow for short strings, but asymptotically
   hideously fast. */
static int
numcompare(a, b)
     register char *a, *b;
{
  register int tmpa, tmpb, loga, logb, tmp;

  tmpa = UCHAR(*a), tmpb = UCHAR(*b);

  if (tmpa == '-')
    {
      tmpa = UCHAR(*++a);
      if (tmpb != '-')
	{
	  if (digits[tmpa] && digits[tmpb])
	    return -1;
	  return 0;
	}
      tmpb = UCHAR(*++b);

      while (tmpa == '0')
	tmpa = UCHAR(*++a);
      while (tmpb == '0')
	tmpb = UCHAR(*++b);

      while (tmpa == tmpb && digits[tmpa])
	tmpa = UCHAR(*++a), tmpb = UCHAR(*++b);

      if (tmpa == '.' && !digits[tmpb] || tmpb == '.' && !digits[tmpa])
	return -fraccompare(a, b);

      if (digits[tmpa])
	for (loga = 1; digits[UCHAR(*++a)]; ++loga)
	  ;
      else
	loga = 0;

      if (digits[tmpb])
	for (logb = 1; digits[UCHAR(*++b)]; ++logb)
	  ;
      else
	logb = 0;

      if (tmp = logb - loga)
	return tmp;

      if (! loga)
	return 0;

      return tmpb - tmpa;
    }
  else if (tmpb == '-')
    {
      if (digits[UCHAR(tmpa)] && digits[UCHAR(*++b)])
	return 1;
      return 0;
    }
  else
    {
      while (tmpa == '0')
	tmpa = UCHAR(*++a);
      while (tmpb == '0')
	tmpb = UCHAR(*++b);

      while (tmpa == tmpb && digits[tmpa])
	tmpa = UCHAR(*++a), tmpb = UCHAR(*++b);

      if (tmpa == '.' && !digits[tmpb] || tmpb == '.' && !digits[tmpa])
	return fraccompare(a, b);

      if (digits[tmpa])
	for (loga = 1; digits[UCHAR(*++a)]; ++loga)
	  ;
      else
	loga = 0;

      if (digits[tmpb])
	for (logb = 1; digits[UCHAR(*++b)]; ++logb)
	  ;
      else
	logb = 0;

      if (tmp = loga - logb)
	return tmp;

      if (! loga)
	return 0;

      return tmpa - tmpb;
    }
}

/* Return an integer <= 12 associated with a month name (0 if the name
   is not recognized. */
static int
getmonth(s, len)
     char *s;
     int len;
{
  char month[4];
  register int i, lo = 0, hi = 12;

  if (len < 3)
    return 0;

  for (i = 0; i < 3; ++i)
    month[i] = fold_tolower[UCHAR(s[i])];
  month[3] = '\0';

  while (hi - lo > 1)
    if (strcmp(month, monthtab[(lo + hi) / 2].name) < 0)
      hi = (lo + hi) / 2;
    else
      lo = (lo + hi) / 2;
  if (!strcmp(month, monthtab[lo].name))
    return monthtab[lo].val;
  return 0;
}

/* Compare two lines trying every key in sequence until there
   are no more keys or a difference is found. */
static int
keycompare(a, b)
     struct line *a, *b;
{
  register char *texta, *textb, *lima, *limb, *translate;
  register int *ignore;
  struct keyfield *key;
  int diff = 0, iter = 0, lena, lenb;

  for (key = keyhead.next; key; key = key->next, ++iter)
    {
      ignore = key->ignore;
      translate = key->translate;

      /* Find the beginning and limit of each field. */
      if (iter)
	{
	  if (key->eword >= 0)
	    lima = limfield(a, key), limb = limfield(b, key);
	  else
	    lima = a->text + a->length, limb = b->text + b->length;

	  if (key->sword >= 0)
	    texta = begfield(a, key), textb = begfield(b, key);
	  else
	    {
	      texta = a->text, textb = b->text;
	      if (key->skipsblanks)
		{
		  while (texta < lima && blanks[UCHAR(*texta)])
		    ++texta;
		  while (textb < limb && blanks[UCHAR(*textb)])
		    ++textb;
		}
	    }
	}
      else
	{
	  /* For the first iteration only, the key positions have
	     been precomputed for us. */
	  texta = a->keybeg, lima = a->keylim;
	  textb = b->keybeg, limb = b->keylim;
	}

      /* Find the lengths. */
      lena = (int)(lima - texta), lenb = (int)(limb - textb);
      if (lena < 0)
	lena = 0;
      if (lenb < 0)
	lenb = 0;

      /* Actually compare the fields. */
      if (key->numeric)
	{
	  if (*lima || *limb)
	    {
	      char savea = *lima, saveb = *limb;

	      *lima = *limb = '\0';
	      diff = numcompare(texta, textb);
	      *lima = savea, *limb = saveb;
	    }
	  else
	    diff = numcompare(texta, textb);

	  if (diff)
	    return key->reverse ? -diff : diff;
	  continue;
	}
      else if (key->month)
	{
	  diff = getmonth(texta, lena) - getmonth(textb, lenb);
	  if (diff)
	    return key->reverse ? -diff : diff;
	  continue;
	}
      else if (ignore && translate)
	while (texta < lima && textb < limb)
	  {
	    while (texta < lima && ignore[UCHAR(*texta)])
	      ++texta;
	    while (textb < limb && ignore[UCHAR(*textb)])
	      ++textb;
	    if (texta < lima && textb < limb &&
		translate[UCHAR(*texta++)] != translate[UCHAR(*textb++)])
	      {
		diff = translate[UCHAR(*--texta)] - translate[UCHAR(*--textb)];
		break;
	      }
	  }
      else if (ignore)
	while (texta < lima && textb < limb)
	  {
	    while (texta < lima && ignore[UCHAR(*texta)])
	      ++texta;
	    while (textb < limb && ignore[UCHAR(*textb)])
	      ++textb;
	    if (texta < lima && textb < limb && *texta++ != *textb++)
	      {
		diff = *--texta - *--textb;
		break;
	      }
	  }
      else if (translate)
	while (texta < lima && textb < limb)
	  {
	    if (translate[UCHAR(*texta++)] != translate[UCHAR(*textb++)])
	      {
		diff = translate[UCHAR(*--texta)] - translate[UCHAR(*--textb)];
		break;
	      }
	  }
      else
	diff = memcmp(texta, textb, MIN(lena, lenb));

      if (diff)
	return key->reverse ? -diff : diff;
      if (diff = lena - lenb)
	return key->reverse ? -diff : diff;
    }

  return 0;
}

/* Compare two lines, returning negative, zero, or positive depending on
   whether A compares less than, equal to, or greater than B. */
static int
compare(a, b)
     register struct line *a, *b;
{
  int diff, tmpa, tmpb, min;

  if (keyhead.next)
    {
      if (diff = keycompare(a, b))
	return diff;
      if (!unique)
	{
	  tmpa = a->length, tmpb = b->length;
	  diff = memcmp(a->text, b->text, MIN(tmpa, tmpb));
	  if (! diff)
	    diff = tmpa - tmpb;
	}
    }
  else
    {
      tmpa = a->length, tmpb = b->length;
      min = MIN(tmpa, tmpb);
      if (min == 0)
	diff = tmpa - tmpb;
      else
	{
	  char *ap = a->text, *bp = b->text;

	  diff = *ap - *bp;
	  if (diff == 0)
	    {
	      diff = memcmp(ap, bp, min);
	      if (diff == 0)
		diff = tmpa - tmpb;
	    }
	}
    }

  return reverse ? -diff : diff;
}

/* Check that the lines read from the given FP come in order.  Return
   1 if they do and 0 if there is a disorder. */
static int
checkfp(fp)
     FILE *fp;
{
  struct buffer buf;		/* Input buffer. */
  struct lines lines;		/* Lines scanned from the buffer. */
  struct line temp;		/* Copy of previous line. */
  int cc;			/* Character count. */
  int cmp;			/* Result of calling compare. */
  int alloc, i, success = 1;

  initbuf(&buf, mergealloc);
  initlines(&lines, mergealloc / linelength + 1);
  alloc = linelength;
  temp.text = xmalloc(alloc);

  cc = fillbuf(&buf, fp);
  findlines(&buf, &lines);

  if (cc)
    do
      {
	/* Compare each line in the buffer with its successor. */
	for (i = 0; i < lines.used - 1; ++i)
	  {
	    cmp = compare(&lines.lines[i], &lines.lines[i + 1]);
	    if (unique && cmp >= 0 || cmp > 0)
	      {
		success = 0;
		goto finish;
	      }
	  }

	/* Save the last line of the buffer and refill the buffer. */
	if (lines.lines[lines.used - 1].length > alloc)
	  {
	    while (lines.lines[lines.used - 1].length + 1 > alloc)
	      alloc *= 2;
	    temp.text = xrealloc(temp.text, alloc);
	  }
	memcpy(temp.text, lines.lines[lines.used - 1].text,
	       lines.lines[lines.used - 1].length + 1);
	temp.length = lines.lines[lines.used - 1].length;

	cc = fillbuf(&buf, fp);
	if (cc)
	  {
	    findlines(&buf, &lines);
	    /* Make sure the line saved from the old buffer contents is
	       less than or equal to the first line of the new buffer. */
	    cmp = compare(&temp, &lines.lines[0]);
	    if (unique && cmp >= 0 || cmp > 0)
	      {
		success = 0;
		break;
	      }
	  }
      }
    while (cc);

 finish:
  xfclose(fp);
  free(buf.buf);
  free((char *) lines.lines);
  free(temp.text);
  return success;
}

/* Merge lines from FPS onto OFP.  NFPS cannot be greater than NMERGE.
   Close FPS before returning. */
static void
mergefps(fps, nfps, ofp)
     FILE *fps[], *ofp;
     register int nfps;
{
  struct buffer buffer[NMERGE]; /* Input buffers for each file. */
  struct lines lines[NMERGE];	/* Line tables for each buffer. */
  struct line saved;		/* Saved line for unique check. */
  int savedflag = 0;		/* True if there is a saved line. */
  int savealloc;		/* Size allocated for the saved line. */
  int cur[NMERGE];		/* Current line in each line table. */
  int ord[NMERGE];		/* Table representing a permutation of fps,
				   such that lines[ord[0]].lines[cur[ord[0]]]
				   is the smallest line and will be next
				   output. */
  register int i, j, t;

  /* Allocate space for a saved line if necessary. */
  if (unique)
    saved.text = xmalloc(savealloc = linelength);

  /* Read initial lines from each input file. */
  for (i = 0; i < nfps; ++i)
    {
      initbuf(&buffer[i], mergealloc);
      /* If a file is empty, eliminate it from future consideration. */
      while (i < nfps && !fillbuf(&buffer[i], fps[i]))
	{
	  xfclose(fps[i]);
	  --nfps;
	  for (j = i; j < nfps; ++j)
	    fps[j] = fps[j + 1];
	}
      if (i == nfps)
	free(buffer[i].buf);
      else
	{
	  initlines(&lines[i], mergealloc / linelength + 1);
	  findlines(&buffer[i], &lines[i]);
	  cur[i] = 0;
	}
    }

  /* Set up the ord table according to comparisons among input lines.
     Since this only reorders two items if one is strictly greater than
     the other, it is stable. */
  for (i = 0; i < nfps; ++i)
    ord[i] = i;
  for (i = 1; i < nfps; ++i)
    if (compare(&lines[ord[i - 1]].lines[cur[ord[i - 1]]],
		&lines[ord[i]].lines[cur[ord[i]]]) > 0)
      t = ord[i - 1], ord[i - 1] = ord[i], ord[i] = t, i = 0;

  /* Repeatedly output the smallest line until no input remains. */
  while (nfps)
    {
      /* If uniqified output is turned out, output only the last of
	 an identical series of lines. */
      if (unique)
	{
	  if (savedflag && compare(&saved, &lines[ord[0]].lines[cur[ord[0]]]))
	    {
	      xfwrite(saved.text, 1, saved.length, ofp);
	      putc('\n', ofp);
	    }
	  if (savealloc < lines[ord[0]].lines[cur[ord[0]]].length + 1)
	    {
	      while (savealloc < lines[ord[0]].lines[cur[ord[0]]].length + 1)
		savealloc *= 2;
	      saved.text = xrealloc(saved.text, savealloc);
	    }
	  saved.length = lines[ord[0]].lines[cur[ord[0]]].length;
	  memcpy(saved.text, lines[ord[0]].lines[cur[ord[0]]].text,
		 saved.length + 1);
	  savedflag = 1;
	}
      else
	{
	  xfwrite(lines[ord[0]].lines[cur[ord[0]]].text, 1,
		 lines[ord[0]].lines[cur[ord[0]]].length, ofp);
	  putc('\n', ofp);
	}

      /* Check if we need to read more lines into core. */
      if (++cur[ord[0]] == lines[ord[0]].used)
	if (fillbuf(&buffer[ord[0]], fps[ord[0]]))
	  {
	    findlines(&buffer[ord[0]], &lines[ord[0]]);
	    cur[ord[0]] = 0;
	  }
	else
	  {
	    /* We reached EOF on fps[ord[0]]. */
	    for (i = 1; i < nfps; ++i)
	      if (ord[i] > ord[0])
		--ord[i];
	    --nfps;
	    xfclose(fps[ord[0]]);
	    free(buffer[ord[0]].buf);
	    free((char *) lines[ord[0]].lines);
	    for (i = ord[0]; i < nfps; ++i)
	      {
		fps[i] = fps[i + 1];
		buffer[i] = buffer[i + 1];
		lines[i] = lines[i + 1];
		cur[i] = cur[i + 1];
	      }
	    for (i = 0; i < nfps; ++i)
	      ord[i] = ord[i + 1];
	    continue;
	  }

      /* The new line just read in may be larger than other lines
	 already in core; push it back in the queue until we encounter
	 a line larger than it. */
      for (i = 1; i < nfps; ++i)
	{
	  t = compare(&lines[ord[0]].lines[cur[ord[0]]],
		      &lines[ord[i]].lines[cur[ord[i]]]);
	  if (! t)
	    t = ord[0] - ord[i];
	  if (t < 0)
	    break;
	}
      t = ord[0];
      for (j = 1; j < i; ++j)
	ord[j - 1] = ord[j];
      ord[i - 1] = t;
    }

  if (unique && savedflag)
    {
      xfwrite(saved.text, 1, saved.length, ofp);
      putc('\n', ofp);
      free(saved.text);
    }
}

/* Sort the array LINES using TEMP for temporary space. */
static void
sortlines(lines, nlines, temp)
     struct line *lines, *temp;
     int nlines;
{
  register struct line *lo, *hi, *t;
  register int nlo, nhi;

  if (nlines == 2)
    {
      if (compare(&lines[0], &lines[1]) > 0)
	*temp = lines[0], lines[0] = lines[1], lines[1] = *temp;
      return;
    }

  nlo = nlines / 2;
  lo = lines;
  nhi = nlines - nlo;
  hi = lines + nlo;

  if (nlo > 1)
    sortlines(lo, nlo, temp);

  if (nhi > 1)
    sortlines(hi, nhi, temp);

  t = temp;

  while (nlo && nhi)
    if (compare(lo, hi) <= 0)
      *t++ = *lo++, --nlo;
    else
      *t++ = *hi++, --nhi;
  while (nlo--)
    *t++ = *lo++;

  for (lo = lines, nlo = nlines - nhi, t = temp; nlo; --nlo)
    *lo++ = *t++;
}

/* Check that each of the given FILES is ordered.
   Return a count of disordered files. */
static int
check(files, nfiles)
     char *files[];
     int nfiles;
{
  int i, disorders = 0;
  FILE *fp;

  for (i = 0; i < nfiles; ++i)
    {
      fp = xfopen(files[i], "r");
      if (! checkfp(fp))
	{
	  printf("sort: disorder on %s\n", files[i]);
	  ++disorders;
	}
    }
  return disorders;
}

/* Merge any number of FILES onto the given OFP. */
static void
merge(files, nfiles, ofp)
     char *files[];
     int nfiles;
     FILE *ofp;
{
  int i, j, t;
  char *temp;
  FILE *fps[NMERGE], *tfp;

  while (nfiles > NMERGE)
    {
      t = 0;
      for (i = 0; i < nfiles / NMERGE; ++i)
	{
	  for (j = 0; j < NMERGE; ++j)
	    fps[j] = xfopen(files[i * NMERGE + j], "r");
	  tfp = xfopen(temp = tempname(), "w");
	  mergefps(fps, NMERGE, tfp);
	  xfclose(tfp);
	  for (j = 0; j < NMERGE; ++j)
	    zaptemp(files[i * NMERGE + j]);
	  files[t++] = temp;
	}
      for (j = 0; j < nfiles % NMERGE; ++j)
	fps[j] = xfopen(files[i * NMERGE + j], "r");
      tfp = xfopen(temp = tempname(), "w");
      mergefps(fps, nfiles % NMERGE, tfp);
      xfclose(tfp);
      for (j = 0; j < nfiles % NMERGE; ++j)
	zaptemp(files[i * NMERGE + j]);
      files[t++] = temp;
      nfiles = t;
    }

  for (i = 0; i < nfiles; ++i)
    fps[i] = xfopen(files[i], "r");
  mergefps(fps, i, ofp);
  for (i = 0; i < nfiles; ++i)
    zaptemp(files[i]);
}

/* Sort any number of FILES onto the given OFP. */
static void
sort(files, nfiles, ofp)
     char **files;
     int nfiles;
     FILE *ofp;
{
  struct buffer buf;
  struct lines lines;
  struct line *tmp;
  int i, ntmp;
  FILE *fp, *tfp;
  struct tempnode *node;
  int ntemp = 0;
  char **tempfiles;

  initbuf(&buf, sortalloc);
  initlines(&lines, sortalloc / linelength + 1);
  ntmp = lines.alloc;
#ifdef _MSC_VER
  assert_lines (ntmp);
#endif /* _MSC_VER */
  tmp = (struct line *) xmalloc(ntmp * sizeof (struct line));

  while (nfiles--)
    {
      fp = xfopen(*files++, "r");
      while (fillbuf(&buf, fp))
	{
	  findlines(&buf, &lines);
	  if (lines.used > ntmp)
	    {
	      while (lines.used > ntmp)
		ntmp *= 2;
#ifdef _MSC_VER
	      assert_lines (ntmp);
#endif /* _MSC_VER */
	      tmp = (struct line *) xrealloc((char *) tmp,
					     ntmp * sizeof (struct line));
	    }
	  sortlines(lines.lines, lines.used, tmp);
	  if (feof(fp) && !nfiles && !ntemp)
	    tfp = ofp;
	  else
	    {
	      ++ntemp;
	      tfp = xfopen(tempname(), "w");
	    }
	  for (i = 0; i < lines.used; ++i)
	    if (!unique || i == lines.used - 1
		|| compare(&lines.lines[i], &lines.lines[i + 1]))
	      {
		xfwrite(lines.lines[i].text, 1, lines.lines[i].length, tfp);
		putc('\n', tfp);
	      }
	  if (tfp != ofp)
	    xfclose(tfp);
	}
      xfclose(fp);
    }

  free(buf.buf);
  free((char *) lines.lines);
  free((char *) tmp);

  if (ntemp)
    {
      tempfiles = (char **) xmalloc(ntemp * sizeof (char *));
      i = ntemp;
      for (node = temphead.next; node; node = node->next)
	tempfiles[--i] = node->name;
      merge(tempfiles, ntemp, ofp);
      free((char *) tempfiles);
    }
}

/* Insert a key at the end of the list. */
static void
insertkey(key)
     struct keyfield *key;
{
  struct keyfield *k = &keyhead;

  while (k->next)
    k = k->next;
  k->next = key;
  key->next = NULL;
}

static void
usage()
{
  fprintf(stderr,
	  "usage: sort [ -cmu ] [ -tc ] [ -o file ] [ -T dir ]\n");
  fprintf(stderr,
	  "            [ -bdfiMnr ] [ +n [ -m ] . . . ] [ files . . . ]\n");
  exit(-1);
}

static void
badfieldspec(s)
     char *s;
{
  fprintf(stderr, "sort: bad field specification %s\n", s);
  exit(-1);
}

/* Handle interrupts and hangups. */
static void
inthandler()
{
  signal(SIGINT, SIG_DFL);
  cleanup();
#ifdef _MSC_VER
  abort ();
#else /* not _MSC_VER */
  kill(getpid(), SIGINT);
#endif /* not _MSC_VER */
}

#ifndef _MSC_VER
static void
huphandler()
{
  signal(SIGHUP, SIG_DFL);
  cleanup();
  kill(getpid(), SIGHUP);
}
#endif /* not _MSC_VER */

int
sortfilesGnu (argc, argv)
     int argc;
     char *argv[];
{
  struct keyfield *key = NULL, gkey;
  char *s;
  int i, t, t2;
  int checkonly = 0, mergeonly = 0, nfiles;
  char *minus = "-", *outfile = minus, **files, *tmp;
  FILE *ofp;

#ifdef _MSC_VER
  prefix = getenv ("TMP");
  if (!prefix)
    prefix = ".";
#endif /* _MSC_VER */

  inittables();

//  if (signal(SIGINT, SIG_IGN) != SIG_IGN)
//    signal(SIGINT, inthandler);


  gkey.sword = gkey.eword = -1;
  gkey.ignore = NULL;
  gkey.translate = NULL;
  gkey.numeric = gkey.month = gkey.reverse = 0;
  gkey.skipsblanks = gkey.skipeblanks = 0;

  for (i = 1; i < argc; ++i)
    {
      if (argv[i][0] == '+')
	{
	  if (key)
	    insertkey(key);
	  key = (struct keyfield *) xmalloc(sizeof (struct keyfield));
	  key->eword = -1;
	  key->ignore = NULL;
	  key->translate = NULL;
	  key->skipsblanks = key->skipeblanks = 0;
	  key->numeric = key->month = key->reverse = 0;
	  s = argv[i] + 1;
	  if (!digits[UCHAR(*s)])
	    badfieldspec(argv[i]);
	  for (t = 0; digits[UCHAR(*s)]; ++s)
	    t = 10 * t + *s - '0';
	  t2 = 0;
	  if (*s == '.')
	    for (++s; digits[UCHAR(*s)]; ++s)
	      t2 = 10 * t2 + *s - '0';
	  if (t2 || t)
	    {
	      key->sword = t;
	      key->schar = t2;
	    }
	  else
	    key->sword = -1;
	  while (*s)
	    {
	      switch (*s)
		{
		case 'b':
		  key->skipsblanks = 1;
		  break;
		case 'd':
		  key->ignore = nondictionary;
		  break;
		case 'f':
		  key->translate = fold_tolower;
		  break;
		case 'i':
		  key->ignore = nonprinting;
		case 'M':
		  key->skipsblanks = key->skipeblanks = key->month = 1;
		  break;
		case 'n':
		  key->skipsblanks = key->skipeblanks = key->numeric = 1;
		  break;
		case 'r':
		  key->reverse = 1;
		  break;
		default:
		  badfieldspec(argv[i]);
		  break;
		}
	      ++s;
	    }
	}
      else if (argv[i][0] == '-')
	{
	  if (!strcmp("-", argv[i]))
	    break;
	  s = argv[i] + 1;
	  if (digits[UCHAR(*s)])
	    {
	      if (! key)
		usage();
	      for (t = 0; digits[UCHAR(*s)]; ++s)
		t = t * 10 + *s - '0';
	      t2 = 0;
	      if (*s == '.')
		for (++s; digits[UCHAR(*s)]; ++s)
		  t2 = t2 * 10 + *s - '0';
	      key->eword = t;
	      key->echar = t2;
	      while (*s)
		{
		  switch (*s)
		    {
		    case 'b':
		      key->skipeblanks = 1;
		      break;
		    case 'd':
		      key->ignore = nondictionary;
		      break;
		    case 'f':
		      key->translate = fold_tolower;
		      break;
		    case 'i':
		      key->ignore = nonprinting;
		    case 'M':
		      key->skipsblanks = key->skipeblanks = key->month = 1;
		      break;
		    case 'n':
		      key->skipsblanks = key->skipeblanks = key->numeric = 1;
		      break;
		    case 'r':
		      key->reverse = 1;
		      break;
		    default:
		      badfieldspec(argv[i]);
		      break;
		    }
		  ++s;
		}
	      insertkey(key);
	      key = NULL;
	    }
	  else
	    while (*s)
	      {
		switch (*s)
		  {
		  case 'b':
		    gkey.skipsblanks = gkey.skipeblanks = 1;
		    break;
		  case 'c':
		    checkonly = 1;
		    break;
		  case 'd':
		    gkey.ignore = nondictionary;
		    break;
		  case 'f':
		    gkey.translate = fold_tolower;
		    break;
		  case 'i':
		    gkey.ignore = nonprinting;
		    break;
		  case 'M':
		    gkey.skipsblanks = gkey.skipeblanks = gkey.month = 1;
		    break;
		  case 'm':
		    mergeonly = 1;
		    break;
		  case 'n':
		    gkey.skipsblanks = gkey.skipeblanks = gkey.numeric = 1;
		    break;
		  case 'o':
		    if (s[1])
		      outfile = s + 1;
		    else
		      {
			if (i == argc - 1)
			  {
			    fprintf(stderr, "sort: missing argument to -o\n");
			    exit(-1);
			  }
			else
			  outfile = argv[++i];
		      }
		    goto outer;
		  case 'r':
		    gkey.reverse = reverse = 1;
		    break;
		  case 'T':
		    if (s[1])
		      prefix = s + 1;
		    else
		      {
			if (i == argc - 1)
			  {
			    fprintf(stderr, "sort: missing argument to -T\n");
			    exit(-1);
			  }
			else
			  prefix = argv[++i];
		      }
		    goto outer;
		  case 't':
		    if (s[1])
		      tab = *++s;
		    else if (i < argc - 1)
		      {
			tab = *argv[++i];
			goto outer;
		      }
		    else
		      {
			fprintf(stderr, "sort: missing character for -tc\n");
			exit(-1);
		      }
		    break;
		  case 'u':
		    unique = 1;
		    break;
		  case 'V':
		    fprintf(stderr, "%s\n", version);
		    break;
#ifdef _MSC_VER
		  case 'S':
		    {
		      long num;
		      if (s[1])
			num = atol (s + 1);
		      else
			{
			  if (i == argc - 1)
			    {
			      fprintf(stderr, "sort: missing argument to -S\n");
			      exit(-1);
			    }
			  else
			    num = atol (argv[++i]);
			}
		      if (num > 32767 || num <= 0)
			fprintf(stderr, "sort: argument to -S must be < 32k\n");
		      else
			sortalloc = (int) num;
		    }
		    goto outer;
#endif /* _MSC_VER */
		  default:
		    usage();
		    exit(-1);
		  }
		++s;
	      }
	}
      else
	break;
    outer:;
    }

  if (key)
    insertkey(key);

  /* Inheritance of global options to individual keys. */
  for (key = keyhead.next; key; key = key->next)
    if (!key->ignore && !key->translate && !key->skipsblanks && !key->reverse
	&& !key->skipeblanks && !key->month && !key->numeric)
      {
	key->ignore = gkey.ignore;
	key->translate = gkey.translate;
	key->skipsblanks = gkey.skipsblanks;
	key->skipeblanks = gkey.skipeblanks;
	key->month = gkey.month;
	key->numeric = gkey.numeric;
	key->reverse = gkey.reverse;
      }

  if (! keyhead.next && (gkey.ignore || gkey.translate || gkey.skipsblanks
			 || gkey.reverse || gkey.skipeblanks
			 || gkey.month || gkey.numeric))
    insertkey(&gkey);

  if (i < argc)
    {
      nfiles = argc - i;
      files = &argv[i];
    }
  else
    {
      nfiles = 1;
      files = &minus;
    }

  if (checkonly)
    exit(check(files, nfiles));

  if (strcmp(outfile, "-"))
    {
      for (i = 0; i < nfiles; ++i)
	if (!strcmp(outfile, files[i]))
	  break;
      if (i == nfiles)
	ofp = xfopen(outfile, "w");
      else
	{
	  char buf[8192];
	  FILE *fp = xfopen(outfile, "r");
	  int cc;

	  tmp = tempname();
	  ofp = xfopen(tmp, "w");
	  while (cc = (int)fread(buf, 1, sizeof buf, fp))
	    if (cc < 0)
	      {
		fprintf(stderr, "sort: error in fread\n");
		perror("Error ");
		cleanup();
		exit(-1);
	      }
	    else
	      xfwrite(buf, 1, cc, ofp);
	  xfclose(ofp);
	  xfclose(fp);
	  files[i] = tmp;
	  ofp = xfopen(outfile, "w");
	}
    }
  else
    ofp = stdout;

  if (mergeonly)
    merge(files, nfiles, ofp);
  else
    sort(files, nfiles, ofp);
  if (ofp != stdout)
	  xfclose(ofp);   /*  Bill Charley, Oct 97   */
  cleanup();
  return 0;
}

int sortfiles(char *unsortedIn, char *sortedOut)
{
	char *filenames[3];
	char temp[_MAX_FNAME];

	stringCopy (temp, _MAX_FNAME, "-o" , _TRUNCATE);
	stringCat(temp, _MAX_FNAME, sortedOut, strlen(sortedOut));

	filenames[0] = (char *) 0;
	filenames[1] = (char *)&temp;
	filenames[2] = unsortedIn;

	return sortfilesGnu (3, filenames);
}

void sortfilesinterface_ (char *file1, char *file2, int *status, size_t len1, size_t len2)
{
	/* Interface for call to sortfiles */

	char *unsortedIn;
	char *sortedOut;

	unsortedIn = stringFortToC(file1, len1) ;
	sortedOut = stringFortToC(file2, len2) ;

	*status = sortfiles (unsortedIn, sortedOut);

	free(unsortedIn);
	free(sortedOut);

}

