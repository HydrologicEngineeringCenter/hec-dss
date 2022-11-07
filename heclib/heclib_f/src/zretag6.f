      SUBROUTINE zretag (IFLTAB, CPATH, NPATH, CTAG, LFOUND)
C
C
C     Retags a record in a DSS file with tag CTAG.
C     (similar to a rename)
C
C     Written by Bill Charley at HEC, 1989.
C
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*), CTAG*(*), CT*8
      LOGICAL LFOUND
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
C
C
      IF (MLEVEL.GE.12) WRITE ( MUNIT, 20) IFLTAB(KUNIT)
 20   FORMAT (T8,'-----DSS---Debug:  Enter zretag6;  Unit:',I5)
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zretag6',
     * 0, 0, ' ', 0, ' ',0)
C
C
C     Lock file for multiple User Access
      CALL zmultu6 ( IFLTAB, .TRUE., .TRUE.)
      LWRITE = .TRUE.
C
      CT = CTAG
C
C     Fin the pahtname bin block location
      CALL zcheck6 ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
      IF (LFOUND) THEN
C
C     Change the tag in the pathname bin
      NPPWRD = (NPATH-1)/NCPW + 1
      CALL CHRHOL (CT, 1, NTAGC, IPNBIN(JPNBIN+NPPWRD+KBTAG), 1)
      I = IFLTAB(KBNSIZ)
      CALL zptrec6 (IFLTAB, IPNBIN, I, IPBADD, .FALSE.)
C
C     Get the record information area
      IADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL zgtrec6 (IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
C
C     Double Check that this is the correct pathname
      IF (NPATH.NE.INFO(KINPAT)) GO TO 900
C
C     Change the tag in the record information block
      CALL CHRHOL (CT, 1, NTAGC, INFO(NPPWRD+KITAG), 1)
      CALL zptrec6 (IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
C
C     Write informative message
C
      IF (MLEVEL.GE.4) WRITE (MUNIT,100) CT, CPATH(1:NPATH)
 100  FORMAT (1X,'----DSS--zretag6:  ',A,2X,A)
C
      ELSE
C
C     That record was not found!
      IF (MLEVEL.GE.2) WRITE (MUNIT,160) CPATH(1:NPATH)
 160  FORMAT (' ----DSS--zretag6; Record Not Found: ',A)
C
      ENDIF
C
C     Release Multiple User Acess and dump buffer
      CALL zmultu6 ( IFLTAB, .FALSE., .TRUE.)
      LWRITE = .FALSE.
C
      IF (MLEVEL.GE.12) WRITE ( MUNIT,220)
 220  FORMAT (T8,'-----DSS---Debug:  Exit  zretag6')
C
      RETURN
C
C
 900  CONTINUE
      NP = INFO(KINPAT)
      CALL zerror6(IFLTAB, 11,'zretag6',NP, IADD, CPATH, NPATH, ' ',NP)
C
      END

