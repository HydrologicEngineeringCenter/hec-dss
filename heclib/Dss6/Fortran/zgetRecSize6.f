      SUBROUTINE zgetRecSize6(IFLTAB, CPATH, NIHEAD, NCHEAD,
     *                         NUHEAD, NDATA, ISTATUS)
C
C
C     Get internal record sizes
C     Purpose is to know how large to allocate arrays
C
C     ISTATUS is returned as:
C        0  Record found, ok to read/write
C       -1  Record not found
C        1  Record found, user does not have delete access
C           (but does have read/write access)
C        2  Record found, user has only read access
C        3  Record found, user does not have access
C
C     Written by Bill Charley, HEC, 2014.
C
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*)
      INTEGER NIHEAD, NCHEAD, NUHEAD, NDATA, ISTATUS
C
      INTEGER JHEAD, JDATA, NPATH
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      IF (MLEVEL.GE.11) THEN
         CALL CHRLNB(CPATH, NPATH)
         WRITE (MUNIT, 20) IFLTAB(KUNIT), CPATH(1:NPATH)
 20      FORMAT (T6,'-----DSS---Debug: Enter zgetRecSize6',/,
     *   T5,'Unit:',I5,'  Path: ',A)
      ENDIF
C
C
C     Read in the main information block
      CALL zrdinf6(IFLTAB, CPATH, JHEAD, JDATA, ISTATUS)
      IF (ISTATUS.NE.0) GO TO 900
C
C     Get the sizes of the headers and data blocks
      NIHEAD = INFO(NPPWRD+KINIHE)
      NCHEAD = INFO(NPPWRD+KINCHE)
      NUHEAD = INFO(NPPWRD+KINUHE)
      NDATA  = INFO(NPPWRD+KINDAT)
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) ISTATUS
 820  FORMAT (T6,'-----DSS---Debug: Exit zgetRecSize6, Status:',I4)
      RETURN
C
 900  CONTINUE
      NIHEAD = 0
      NCHEAD = 0
      NUHEAD = 0
      NDATA  = 0
      GO TO 800
C
C
      END

