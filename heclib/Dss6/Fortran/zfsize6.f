      SUBROUTINE zfsize6 (IFLTAB)
C
      implicit none
C     This subroutine sets the file size parameters based
C     on a zset6 call with the parameter 'SIZE', or uses
C     a default file size.
C
C     The parameters set include:
C        The Hash size (number of hashes possible)
C        The Bin size (how large a pathname bin is)
C        The number of bins per block (how many pathname
C        bins are grouped together)
C        The structure type:
C              Type 1:  With hash table for dynamic data bases
C              Type 2:  No hash table (hash directly to bins) for
C                       somewhat stable data bases
C
C
C   Size        Hash  N Bins  Target    Target         Default
C   Name        Size  Block   Number    Range           Range
C   ----        ----  -----   ------    -----           -----
C   Tiny           8    4       20       1-50            1-20
C   Extra Small   32    4       50       1-200          21-99
C   Small        128    8      200     100-1,000       100-499
C   Medimum      512   16    1,000     200-5,000       500-1,999
C   Large       1024   32    4,000   1,000-10,000    2,000-7,999
C   Extra Large 2048   32   10,000   2,000-20,000    8,000-14,999
C   Huge        4096   32   20,000   5,000-40,000   15,000-29,999
C   Extra Huge  8192   32   50,000  20,000-80,000   30,000-60,000
C   Super       8192   64  200,000     >50,000         >60,000
C
C
C     In the preceding table, the target number and target range are
C     the optimal number of records in the file, while the default
C     range is the size selected when a number of pathnames is
C     passed in a zset6 'SIZE' call.
C
C     Written by Bill Charley at HEC, 1988.
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsscz.h'
C
C
C     Set file size parameters
      INTEGER JHASHS(9), NBINBK(9), IRANGE(9)
      INTEGER IFLTAB(*),I,J
C
      DATA JHASHS(1), NBINBK(1), IRANGE(1)
     * /           8,         4,        1/
      DATA JHASHS(2), NBINBK(2), IRANGE(2)
     * /          32,         4,        2/
      DATA JHASHS(3), NBINBK(3), IRANGE(3)
     * /         128,         8,       20/
      DATA JHASHS(4), NBINBK(4), IRANGE(4)
     * /         512,        16,      100/
      DATA JHASHS(5), NBINBK(5), IRANGE(5)
     * /        1024,        32,      200/
      DATA JHASHS(6), NBINBK(6), IRANGE(6)
     * /        2048,        32,     4000/
      DATA JHASHS(7), NBINBK(7), IRANGE(7)
     * /        4096,        32,    10000/
      DATA JHASHS(8), NBINBK(8), IRANGE(8)
     * /        8192,        32,    30000/
      DATA JHASHS(9), NBINBK(9), IRANGE(9)
     * /        8192,        64,    60000/
C
C
C     DATA JHASHS(1), IBSIZE(1), NBINBK(1), IRANGE(1)
C    * /           8,        64,         4,        1/
C     DATA JHASHS(2), IBSIZE(2), NBINBK(2), IRANGE(2)
C    * /          32,        64,         4,       21/
C     DATA JHASHS(3), IBSIZE(3), NBINBK(3), IRANGE(3)
C    * /         128,       128,         8,      100/
C     DATA JHASHS(4), IBSIZE(4), NBINBK(4), IRANGE(4)
C    * /         512,       128,        16,      500/
C     DATA JHASHS(5), IBSIZE(5), NBINBK(5), IRANGE(5)
C    * /        1024,       128,        32,     2000/
C     DATA JHASHS(6), IBSIZE(6), NBINBK(6), IRANGE(6)
C    * /        2048,       256,        32,     8000/
C     DATA JHASHS(7), IBSIZE(7), NBINBK(7), IRANGE(7)
C    * /        4096,       256,        32,    15000/
C     DATA JHASHS(8), IBSIZE(8), NBINBK(8), IRANGE(8)
C    * /        8192,       256,        32,    30000/
C
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) LSZSET, NSIZE, IHSIZE, CSIZE
 20   FORMAT (T8,'-----DSS---Debug:  Enter zfsize6',/,T12,
     * 'LSZSET, NSIZE, IHSIZE, CSIZE: ',L2,2I5,2X,A)
C
C     If a zset6 'SIZE' call has been made, determine proper parameter
      IF ((IHSIZE.GT.0).AND.(IHSIZE.LT.9)) THEN
C
      ELSE
C
      IF (LSZSET) THEN
      IHSIZE = 0
C
      IF (CSIZE(1:2).NE.'  ') THEN
      CALL UPCASE (CSIZE)
C
      I = INDEX(CSIZE,'TINY')
      IF (I.GT.0) THEN
      IHSIZE = 1
      GO TO 100
      ENDIF
C
      I = INDEX(CSIZE,'SMAL')
      IF (I.GT.0) THEN
      J = INDEX(CSIZE,'EX')
      IF (J.GT.0) THEN
      IHSIZE = 2
      ELSE
      IHSIZE = 3
      ENDIF
      GO TO 100
      ENDIF
C
      I = INDEX(CSIZE,'MED')
      IF (I.GT.0) THEN
      IHSIZE = 4
      GO TO 100
      ENDIF
C
      I = INDEX(CSIZE,'LARG')
      IF (I.GT.0) THEN
      J = INDEX(CSIZE,'EX')
      IF (J.EQ.0) THEN
      IHSIZE = 5
      ELSE
      IHSIZE = 6
      ENDIF
      GO TO 100
      ENDIF
C
      I = INDEX(CSIZE,'HUGE')
      IF (I.GT.0) THEN
      J = INDEX(CSIZE,'EX')
      IF (J.EQ.0) THEN
      IHSIZE = 7
      ELSE
      IHSIZE = 8
      ENDIF
      ENDIF
C
      I = INDEX(CSIZE,'SUPER')
      IF (I.GT.0) THEN
         IHSIZE = 9
      ENDIF
C
 100  CONTINUE
C
      ELSE
      DO 120 I=1,9
      IF (NSIZE.GE.IRANGE(I)) IHSIZE = I
 120  CONTINUE
      ENDIF
C
      IF (IHSIZE.EQ.0) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 140) NSIZE, CSIZE
 140  FORMAT (/,' -----DSS***  zopen6 - Error:  Illegal File Size Set',/
     * ' Number of Records Set:',I7,'  Size Name Set: ',A,/,
     * ' --- The Default File Size Will Be Used ---',/)
      IHSIZE = 6
      ENDIF
C
      ELSE
      IHSIZE = 6
      ENDIF
      ENDIF
C
C
      IFLTAB(KHSIZE) = IHSIZE
      IFLTAB(KHASH)  = INT(JHASHS(IHSIZE))
      IFLTAB(KBNBLK) = INT(NBINBK(IHSIZE))
C     As of Version 6-M, all bin sizes are 127 words
      IFLTAB(KBNSIZ) = INT(127)
C
      IF (LSTABL) THEN
      IFLTAB(KTABLE) = 2
      ELSE
      IFLTAB(KTABLE) = 1
      ENDIF
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,200) IHSIZE, IFLTAB(KTABLE)
 200  FORMAT (T8,'-----DSS---Debug:  Exit  zfsize6;  Size:',I5,
     * ',  Ktable:',I3)
C
      RETURN
      END

