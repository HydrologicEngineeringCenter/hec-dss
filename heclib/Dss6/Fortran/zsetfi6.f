      SUBROUTINE zsetfi6 (IFLTAB, CPARAM, CALPHA, INUMB, ISTATUS)
C
C
C     Set file parrameters
C
C     Written by Bill Charley at HEC, 1998.
C
      INTEGER IFLTAB(*)
      CHARACTER CPARAM*(*), CALPHA*(*)
      CHARACTER CFLAG*4
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
      COMMON /WORDS/ IWORD(10)
C
      COMMON /ZDSSFZ/ LFIRST
      LOGICAL LFIRST
C
      LOGICAL LFOUND
      INTEGER N, NH, ND
C
C
      IF (LFIRST) THEN
      CALL zinit6
      LFIRST = .FALSE.
      ENDIF
C
      CFLAG = CPARAM
      CALL UPCASE (CFLAG)
      ISTATUS = 0
C
      IF (MLEVEL.GE.15) WRITE (MUNIT,20) CFLAG
 20   FORMAT (T10,'-----DSS---Debug:  Enter zsetfi6;  Flag: -',A,'-')
C
      IF (IFLTAB(1).NE.6) THEN
C        File not opened
         ISTAT = -1
         IF (MLEVEL.GE.4) WRITE (MUNIT,40)
 40      FORMAT (' ----zsetfi6 - Invalid call: HEC-DSS File not opened')
         GO TO 800
      ENDIF
C
C
C     MULT: Set Multi-user access level
      IF (CFLAG.EQ.'MULT') THEN
C         We are limited to only certain levels
         IF (INUMB.EQ.1) THEN
C           Read only level - OK
            CALL zbdump6 (IFLTAB, 1)
            IFLTAB(KMULT) = 1
            IFLTAB(KREADO) = 1
         ELSE IF ((INUMB.EQ.2).AND.(IFLTAB(KMULT).EQ.3)) THEN
C           We can only go to level 2 from 3
            IFLTAB(KMULT) = 2
            IF (MLEVEL.GE.1) WRITE (MUNIT, 80) IFLTAB(KUNIT)
 80         FORMAT(' -----DSS--- File set to multi-user access, unit:',
     *             I6)
         ELSE
            IF (MLEVEL.GE.6) WRITE (MUNIT,100) INUMB
 100         FORMAT(' ----zsetfi6 - Invalid multi-user access: ',I4)
            ISTATUS = -1
         ENDIF

C
C     Force mult-user access for PC
      ELSE IF (CFLAG.EQ.'FMUL') THEN
         IF (IFLTAB(KMULT).EQ.3) IFLTAB(KMULT) = 2
         CALL zmultu6 (IFLTAB, .TRUE., .TRUE.)
         CALL UPCASE (CALPHA)
         IF (CALPHA(1:2).EQ.'ON') THEN
            IFLTAB(KMULT2) = 1
         ELSE IF (CALPHA(1:2).EQ.'OFF') THEN
            IFLTAB(KMULT2) = 0
         ENDIF
C        Re-write permanent section to the file
         NADD = 1
         CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
         CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
C
C
       ELSE IF (CFLAG.EQ.'PSEU') THEN
          ! LOCK FILE
          CALL ZMULTU6 (IFLTAB, .TRUE., .FALSE.)
          CALL CHRLNB(CALPHA, N)
          CALL ZCHECK6(IFLTAB, CALPHA, N, NH, ND, LFOUND)
          IF (LFOUND) THEN
             NBWPAT = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCPW) + 1
             IF (IPNBIN(JPNBIN+KBPINTL+NBWPAT).NE.INUMB) THEN
                IPNBIN(JPNBIN+KBPINTL+NBWPAT) = INUMB
                IPBADD = IFLTAB(KPADD)
                ISIZE = IFLTAB(KBNSIZ)
                CALL ZPTREC6 (IFLTAB, IPNBIN, ISIZE, IPBADD, .FALSE.)
             ENDIF
          ELSE
             ISTATUS = -1
          ENDIF
          !UNLOCK FILE
          CALL ZMULTU6(IFLTAB, .FALSE., .TRUE.)
C
C
      ELSE
         IF (MLEVEL.GE.6) WRITE (MUNIT,700) CPARAM
 700     FORMAT(' ----zsetfi6 - Unrecognized Setting: ',A)
         ISTATUS = -20
      ENDIF
C
C
C
 800  CONTINUE
      IF (MLEVEL.GE.15) THEN
      CALL CHRLNB (CALPHA, N)
      IF (N.EQ.0) N = 1
      IF (MLEVEL.GE.15) WRITE (MUNIT,820) INUMB, CALPHA(1:N), ISTATUS
 820  FORMAT (T10,'-----DSS---Debug:  Exit zsetfi6',/,
     * T10,'Number:',I8,',  Character: ',A,'  Status: ',I3)
      ENDIF
      RETURN
C
      END

