      SUBROUTINE zrenam6(IFLTAB, CPATHO, NPATHO, CPATHN, NPATHN, LFOUND)
C
C
C     This subroutine renames record in a DSS file by flagging the
C     infromation block of the old name with a rename delete flag,
C     then writing a new information block (and pathname bin) with
C     the new pathname, but pointing to the existing data and header
C     areas.  This subroutine works in conjunction with zdelet6 and
C     zwrite6.
C
C     IRENAM is flag indicating rename status:
C        0:  No rename
C        1:  Deleting old record for rename
C        2:  Renaming this record
C
C     Written by Bill Charley at HEC, 1988.
C
C
      INTEGER IFLTAB(*), INFORN(50)
      CHARACTER CPATHO*(*), CPATHN*(*)
      LOGICAL LFOUND
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssmz.h'
C
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zrenam6;  Unit:',I5)
C
C     Make sure the guy is not trying to rename the pathname to itself
      IF (NPATHN.EQ.NPATHO) THEN
      IF (CPATHN(1:NPATHN).EQ.CPATHO(1:NPATHO)) GO TO 910
      ENDIF
C
C     Get multiple user access
      CALL zmultu6 ( IFLTAB, .TRUE., .TRUE.)
C
      LWRITE = .TRUE.
      IRENAM = 2
C
C     Set a delete flag for the old pathname, and at the
C     same time get information from the INFO block
      CALL zdelet6 (IFLTAB, CPATHO, NPATHO, LFOUND)
      IF (.NOT.LFOUND) GO TO 900
C
      NLEN = KIUNUS - KIPASS + 1
      DO 40 I=1,NLEN
      J = I + NPPWRD + KIPASS - 1
      INFORN(I) = INFO(J)
 40   CONTINUE
C
      INTL_PSEUDO = IPNBIN(JPNBIN+NPPWRD+KBPINTL)
      IRNTYP = INFO(NPPWRD+KITYPE)
      NDATA = INFO(NPPWRD+KINDAT)
      NIHEAD = INFO(NPPWRD+KINIHE)
      NCHEAD = INFO(NPPWRD+KINCHE)
      NUHEAD = INFO(NPPWRD+KINUHE)
      NLDATA = INFO(NPPWRD+KILNDA)
      CALL HOLCHR (INFO(NPPWRD+KITAG), 1, NTAGC, CRNTAG, 1)
C
C     Make sure that the new pathname does not already exist
      CALL zcheck6 (IFLTAB, CPATHN, NPATHN, JHEAD, JDATA, LFOUND)
C     If the new record already exists, delete it.
      IF (LFOUND) THEN
      IRENAM = 1
      CALL zdelet6 (IFLTAB, CPATHN, NPATHN, LFOUND)
      IRENAM = 2
      ENDIF
C
      CALL znwrit6 (IFLTAB, CPATHN, NPATHN, NIHEAD, NCHEAD, NUHEAD,
     * NDATA)
C
C     Store the information block
C
      NPPWRD = (NPATHN-1)/NCPW + 1
      DO 60 I=1,NLEN
      J = I + NPPWRD + KIPASS - 1
      INFO(J) = INFORN(I)
 60   CONTINUE
C
      ISIZE = NPPWRD + NINFO
      CALL zptrec6 (IFLTAB, INFO, ISIZE, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
C
C
      LFOUND = .TRUE.
      LWRITE = .FALSE.
C
      IF (MLEVEL.GE.3) WRITE(MUNIT,140) IFLTAB(KUNIT), CPATHO(1:NPATHO),
     * CPATHN(1:NPATHN)
 140  FORMAT (' -----DSS---zrenam6;  Unit',I5,';',/,' Old Pathname: ',A,
     * /,' New Pathname: ',A)
C
C
 800  CONTINUE
      IRENAM = 0
      INTL_PSEUDO = 0
      IFLTAB(KLPATL) = -1
C     Release multiple user access
      CALL zmultu6 ( IFLTAB, .FALSE., .TRUE.)
      IF (MLEVEL.GE.11) WRITE (MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug:  Exit  zrenam6')
      RETURN
C
C
 900  CONTINUE
C     The original (old) record was not found!
      IF (MLEVEL.GE.2) WRITE (MUNIT,901) IFLTAB(KUNIT), CPATHO(1:NPATHO)
 901  FORMAT (' -----DSS---zrenam6:  Cannot Find Record Specified;',
     * '  Unit:',I5,/,' Pathname: ',A)
      LFOUND = .FALSE.
      GO TO 800
C
C
 910  CONTINUE
C     The new pathname is the same as the old!
      IF (MLEVEL.GE.2) WRITE (MUNIT,911) IFLTAB(KUNIT), CPATHO(1:NPATHO)
 911  FORMAT (' -----DSS---zrenam6:  New Pathname Same as Old;',
     * '  Unit:',I5,/,' Pathname: ',A)
      LFOUND = .FALSE.
      GO TO 800
C
      END

