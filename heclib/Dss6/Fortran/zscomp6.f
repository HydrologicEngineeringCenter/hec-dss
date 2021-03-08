      SUBROUTINE zscomp6 (ICOMP, BASE, LBASE, LHIGH, NPRE)
C
C
C     This routine sets compression values for storing time-
C     series data for routines zsrts6 and zsits6.  If compression
C     is to be used (and not the default compression), this
C     routine must be called each time before one of these
C     routines (i.e., compression settings are not remembered).
C
C     Written by Bill Charley at HEC, 1989
C
C
      INCLUDE 'zdssmz.h'
C
      COMMON /ZDSSCP/ JCOMP, BASEV, LBASEV, LDHIGH, NPREC
      LOGICAL LBASEV, LDHIGH, LBASE, LHIGH
C
C
      IF (MLEVEL.GE.9) WRITE (MUNIT, 20) ICOMP, BASE, LBASE,
     * LHIGH, NPRE
 20   FORMAT (T11,'-----DSS---DEBUG:  Enter zscomp6',/,
     * T11,'COMP:',I5,',  BASEV:',F8.2,',  LBASEV:',L2,/
     * T11,'LDHIGH:',L2,',  NPREC:',I4)
C
      JCOMP = ICOMP
      BASEV = BASE
      LBASEV = LBASE
      LDHIGH = LHIGH
      NPREC = NPRE
C
      RETURN
      END

