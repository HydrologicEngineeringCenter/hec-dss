      SUBROUTINE zquery6 (CFLG, cval, INUMB)
C
C
C
C     Inquire about library settings
C
C     Written by Bill Charley at HEC, 2008.
C
      CHARACTER CFLG*(*), cval*(*)
      CHARACTER CTEMP*128, CFLAG*4, calpha*128
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssts.h'
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
      INTEGER NERROR, MAXERROR
C
      COMMON /WORDS/ IWORD(10)
C
      COMMON /ZDSSFZ/ LFIRST
      LOGICAL LFIRST
      INTEGER IFPOS, ISTAT
      LOGICAL LCHAR
C
C
C
      IF (LFIRST) THEN
      CALL zinit6
      LFIRST = .FALSE.
      ENDIF
C
      LCHAR = .FALSE.
      CFLAG = CFLG
      CALL UPCASE(CFLAG)
C
      IF (MLEVEL.GE.15) WRITE (MUNIT,20) CFLAG
 20   FORMAT (T10,'-----DSS---Debug:  Enter zquery6;  Flag: -',A,'-')
C
C
C     MLEVEL: MESSAGE LEVEL
      IF ((CFLAG.EQ.'MLEV').OR.(CFLAG.EQ.'MLVL')) THEN
      INUMB = MLEVEL
C
      ELSE IF (CFLAG.EQ.'TOTA') THEN
      INUMB = TOTAL_NUMB
C
      ELSE IF (CFLAG.EQ.'CURR') THEN
      INUMB = CURRENT_NUMB
C
      ELSE IF (CFLAG.EQ.'NERR') THEN
      INUMB = NERROR
C
      ELSE IF (CFLAG.EQ.'MAXE') THEN
      INUMB = MAXERROR
C
C
C     MUNIT: MESSAGE UNIT
      ELSE IF ((CFLAG.EQ.'MUNI').OR.(CFLAG.EQ.'MLFN')) THEN
      INUMB = MUNIT
C
C     VERSION: DSS SOFTWARE VERSION
      ELSE IF (CFLAG.EQ.'VERS') THEN
      CALPHA = CVERS
      INUMB = 6
      LCHAR = .TRUE.
C
C     PROG: PROGRAM
      ELSE IF (CFLAG.EQ.'PROG') THEN
      CTEMP = ' '
      CALL HOLCHR (INFO(NPPWRD+KIPROG), 1, NPROGC, CTEMP, 1)
      CALPHA = CTEMP
      LCHAR = .TRUE.
C
C
C     80COL:  80 COLUMN OUTPUT ON OR OFF (VS. 132 COL)
      ELSE IF (CFLAG.EQ.'80CO') THEN
      IF (L80COL) THEN
      CALPHA = 'ON'
      INUMB = 1
      ELSE
      CALPHA = 'OFF'
      INUMB = 0
      ENDIF
C
C     TAG:  TAG OF LAST RECORD ACCESSED
      ELSE IF (CFLAG(1:3).EQ.'TAG') THEN
      CTEMP = ' '
      IF ((JPNBIN.GE.1).AND.(NPPWRD.GT.1)) THEN
      CALL HOLCHR (IPNBIN(JPNBIN+NPPWRD+KBTAG), 1, NTAGC, CTEMP, 1)
      ENDIF
      CALPHA = CTEMP
      LCHAR = .TRUE.
C
C
C     Abort on fatal error (should be yes!)
      ELSE IF (CFLAG.EQ.'ABOR') THEN
      IF (.NOT.LNOABT) THEN
      CALPHA = 'ON'
      INUMB = 1
      ELSE
      CALPHA = 'OFF'
      INUMB = 0
      ENDIF
      LCHAR = .TRUE.
C
C
C     Copy Empty (all missing) records on a squeeze or file copy?
      ELSE IF (CFLAG.EQ.'EMPT') THEN
      IF (LCPEMPTY) THEN
      CALPHA = 'ON'
      INUMB = 1
      ELSE
      CALPHA = 'OFF'
      INUMB = 0
      ENDIF
      LCHAR = .TRUE.
C
C     Force squeeze, even though might not be needed
      ELSE IF (CFLAG.EQ.'SQUE') THEN
      IF (LFSQUEEZE) THEN
      CALPHA = 'ON'
      INUMB = 1
      ELSE
      CALPHA = 'OFF'
      INUMB = 0
      ENDIF
      LCHAR = .TRUE.
C
C     If a system error occurred, the system error message
C     and number associated with that error
      ELSE IF (CFLAG.EQ.'ERRO') THEN
         INUMB = IERRMS
         CALPHA = CERRMS
         LCHAR = .TRUE.
C
C
C
      ELSE
      IF (MLEVEL.GE.4) WRITE (MUNIT,80) CFLG
 80   FORMAT(' ----zquery6 - Unrecognized Inquire: ',A)
      INUMB = -1
      CALPHA = ' '
      ENDIF
C
C
C
 800  CONTINUE
      call strcpy(cval, calpha)
      IF (MLEVEL.GE.15) THEN
      CALL CHRLNB (CALPHA, N)
      IF (N.EQ.0) N = 1
      WRITE (MUNIT,820) INUMB
 820  FORMAT (T10,'-----DSS---Debug:  Exit zquery6',/,
     * T10,'Number:',I9)
      IF (LCHAR) THEN
      WRITE (MUNIT,830) CALPHA(1:N)
 830  FORMAT (T10,'Character: ',A)
      ENDIF
      ENDIF
      RETURN
C
      END

