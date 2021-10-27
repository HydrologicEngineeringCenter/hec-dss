      SUBROUTINE zrtxts6 (IFLTAB, CPATH, CSTRING, KSTRING,
     * NSTRING, IUHEAD, KUHEAD, NUHEAD, ISTAT)
C
C
C     Retrieve text data and place into the character string CSTRING.
C     CSTRING should be large enough to hold the text data
C     (Its dimension is KSTRING)
C     For writing text data to a unit (e.g., file) use zrtext6
C
C     Written by Bill Charley at HEC, 2000
C
C
      INTEGER IFLTAB(*), IUHEAD(*)
      INTEGER KSTRING, NSTRING, KUHEAD, NUHEAD, ISTAT
C
      CHARACTER CSTRING*(*)
      CHARACTER CPATH*(*)
C
C
      CHARACTER CLINE*10
      LOGICAL LFOUND , LEND
      INTEGER IPOS, JPOS
C
      COMMON /WORDS/ IWORD(10)
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssts.h'
C
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zrtxts6',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C
      ISTAT = 0
      CALL CHRLNB (CPATH, NPATH)
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) WRITE (MUNIT, 20) KSTRING, KUHEAD,
     * CPATH(1:NPATH)
 20   FORMAT (T5,'----- Enter zrtxts6  -----',/,
     * T11,'String Dimension:',I4,'  User Header Dimension:',
     * I5,/,T11,'Pathname: ',A)
C
C
C
      CALL zreadx6 (IFLTAB, CPATH, NBYTES, 1, N, ICHEAD, 0, N,
     * IUHEAD, KUHEAD, NUHEAD, ILBUFF, KLBUFF, NBUFF, 0, LFOUND)
C
      IF (.NOT.LFOUND) THEN
      ISTAT = -1
      GO TO 800
      ENDIF
C
      CALL zinqir6 (IFLTAB, 'TYPE', CLINE, JTYPE)
      IF (JTYPE.NE.300) GO TO 900
C
C
C     Do we need to call zrdbuf6 (not enough array space)?
      IF (NBUFF.GT.KLBUFF) THEN
         LEND = .FALSE.
      ELSE
         LEND = .TRUE.
      ENDIF
C
      IPOS = 1
      NSTRING = 0
C
C
 80   CONTINUE
      IF (.NOT.LEND) THEN
         CALL zrdbuf6 (IFLTAB, CPATH, I, 0, N, ILBUFF, KLBUFF, NBUFF,
     *                LEND, 0, LFOUND)
      ENDIF
C
      NREAD = NBUFF * IWORD(2)
      JPOS = IPOS + NREAD - 1
      IF (JPOS.GE.KSTRING) THEN
          NREAD = KSTRING - IPOS + 1
          LEND = .TRUE.
      ENDIF
C
C
C
 100  CONTINUE
      NSTRING = NSTRING + NREAD
      IF (NSTRING.GT.NBYTES) THEN
         NREAD = NREAD - (NSTRING - NBYTES)
         NSTRING = NBYTES
      ENDIF
      CALL HOLCHR (ILBUFF, 1, NREAD, CSTRING, IPOS)
      IPOS = IPOS + NREAD
C
      IF (IPOS.LT.NBYTES) THEN
         IF (LEND) THEN
            GO TO 800
         ELSE
            GO TO 80
         ENDIF
      ENDIF
C
C
 800  CONTINUE
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NSTRING, ISTAT
 820  FORMAT(T5,'----- Exit zrtxts6, Number of bytes read:',I6,
     * ',  Status:',I5)
C
      RETURN
C
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,901) JTYPE, CPATH(1:NPATH)
 901  FORMAT (/,' *****DSS*** zrtxts6:  ERROR - Record Not Identified',
     * ' as TEXT data.',/,' Data Type:',I5,/,' Pathname: ',A)
      ISTAT = -2
      GO TO 800
C
C
      END

