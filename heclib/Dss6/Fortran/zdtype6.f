      SUBROUTINE zdtype6 (IFLTAB, CPATH, NDATA, LFOUND, CDTYPE, IDTYPE)
C
C
C     Determine the data type of a record and whether it exists
C
C     Written by Bill Charley at HEC, 1990.
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*), CDTYPE*(*)
      LOGICAL LFOUND
      INTEGER IBPART(6), IEPART(6), ILPART(6)
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
       INCLUDE 'zdssShared.h'
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH
 20   FORMAT (T6,'-----DSS---Debug: Enter zdtype6',/,T10,
     * 'UNIT =',I5,'  PATH: ',A)
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zdtype6', 0,
     * IFLTAB, ' ', 0, ' ',0)
C
C
C     Check if this record exists
C
      CALL CHRLNB (CPATH, NPATH)
      CALL zcheck6 (IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
      IF (LFOUND) THEN
C     Get data type
      IDTYPE = IFLTAB(KDTYPE)
C
C     Fix for incorrect storage of gridded data type. B Charley May 1999
      IF (IDTYPE.EQ.0) THEN
         CALL zrdinf6 (IFLTAB, CPATH, NUHEAD, NDATA, ISTAT)
         IDTYPE = INFO(NPPWRD+KITYPE)
      ENDIF
C
      ELSE
C
      NDATA = 0
      IDTYPE = 0
C     See if we can determine if this is a time series record
      CALL zupath (CPATH, IBPART, IEPART, ILPART, ISTAT)
      IF ((ISTAT.EQ.0).AND.(ILPART(5).GE.4)) THEN
      IST = 1
      CALL zgintl6 (INTL, CPATH(IBPART(5):IEPART(5)), N, IST)
      IF (IST.EQ.0) THEN
      IDTYPE = 100
      ELSE IF (IST.EQ.1) THEN
      IDTYPE = 110
      ENDIF
C     CHECK FOR TIME SERIES PATTERN TYPE
      IF (CPATH(IBPART(4):IBPART(4)+2).EQ.'TS-') THEN
         IDTYPE = IDTYPE + 1
      ENDIF
      ENDIF
C
      ENDIF
C
C
C     Get the character record data type
      DO 40 I=1,NRTYPE
         IF (IDTYPE.EQ.IRTYPE(I)) THEN
            call strcpy(CDTYPE, CRTYPE(I))
            GO TO 50
         ENDIF
 40   CONTINUE
      CDTYPE = 'UND'
C
 50   CONTINUE
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) LFOUND, IDTYPE, CDTYPE
 820  FORMAT (T6,'-----DSS--Debug: EXIT zdtype6;  Found: ',L1,
     * ';  Type:',I4,2X,A)
C
      RETURN
C
      END

