      SUBROUTINE zrdinf6 (IFLTAB, CPATH, NHEAD, NDATA, ISTAT)
C
C
C     Read the info block for a record
C     ISTAT is returned as:
C        0  Record found, ok to read/write
C       -1  Record not found
C        1  Record found, user does not have delete access
C           (but does have read/write access)
C        2  Record found, user has only read access
C        3  Record found, user does not have access
C
C     Written by Bill Charley at HEC, 1989.
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*), CTPATH*392, CV*4
      LOGICAL LFOUND
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH
 20   FORMAT (T6,'-----DSS---Debug: Enter zrdinf6',/,T10,
     * 'UNIT =',I5,'  PATH: ',A)
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zrdinf6',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
      CALL CHRLNB (CPATH, NPATH)
C
C     Check if this record exists
C
      CALL zcheck6 ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
C     If the record does not exist, write message then return
      IF (.NOT.LFOUND) THEN
      IF (MLEVEL.GE.5) WRITE ( MUNIT, 30) CPATH(1:NPATH)
 30   FORMAT ( '-----DSS---zrdinf6, Record Not Found: ',A)
      NHEAD = 0
      NDATA = 0
      ISTAT = -1
      ELSE
C
C     Get Information Block
      NADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL zgtrec6 (IFLTAB, INFO, NINFO+NPPWRD, NADD, .FALSE.)
C
C     Double Check that this is the correct pathname
      CALL HOL2CH ( INFO(KIPATH), CTPATH, NPMWRD)
      IF (NPATH.NE.INFO(KINPAT)) GO TO 900
      IF (CPATH(1:NPATH).NE.CTPATH(1:NPATH)) GO TO 900
C
C     Check the record access
      IF (INFO(NPPWRD+KIPASS).NE.0) THEN
C     Check access for this user
      ISTAT = 3   !!!! TEMPORARY !!!!!!
      CALL zinqir6 (IFLTAB, 'FVER', CV, I)
      I = ICHAR(CV(3:3))
      IF (I.LT.69) ISTAT = 0
      ELSE
C     Every one has access
      ISTAT = 0
      ENDIF
C
      ENDIF
C
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) ISTAT
 820  FORMAT (T6,'-----DSS--Debug: EXIT zrdinf6;  Status:',I3)
C
      RETURN
C
C
 900  CONTINUE
      IF (.NOT.LNOABT) CALL ZDEBUG (MUNIT, INFO, 1, 30)
      NP = INFO(KINPAT)
      IF (MLEVEL.GE.3) WRITE ( MUNIT, 930) IFLTAB(KUNIT), CPATH
 930  FORMAT ( '-----DSS---zrdinf6, Error, Pathname: ',A)
      CALL zerror6 (IFLTAB, 11,'zrdinf6', 0, NADD, CPATH, NPATH, CTPATH,
     * NP)
      ISTAT = -2
      RETURN
C
      END

