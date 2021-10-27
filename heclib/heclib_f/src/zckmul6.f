      SUBROUTINE zckmul6 (IFLTAB)
C
C
C     Check Multi-user access.
C     If the file is in an "advisory" multi-user access level,
C     check to see if any other programs have requested write
C     access to the file.  If so, go into a full multi-user mode.
C     Written by Bill Charley at HEC, 1998.
C
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      COMMON /WORDS/ IWORD(10)
C
      INTEGER IFLTAB(*)
C
C
C     If on a PC, on a read, do we need to check for an advisory lock?
C     (On a write, zmultu6 will check)
      IF (MLEVEL.GE.14) WRITE (MUNIT, 20) IFLTAB(KUNIT), IFLTAB(KMULT),
     * IFLTAB(KLOCK)
 20   FORMAT (T5,'----DSS--- DEBUG:  Enter zckmul6;  Unit:',I5,/,
     * T12,'KMULT:',I3,',  KLOCK:',I3)
C
C     IFLTAB(KFILEW).EQ.1:  Have we written to the file yet?
C        (if we are just reading we don't need multi-user access,
C         and we should not try to lock the file)
C     IFLTAB(KFILEW).EQ.1:  Are we in a "advisory mode"?
      IF ((IFLTAB(KFILEW).EQ.1).AND.(IFLTAB(KMULT).EQ.3)) THEN
         IBYTE = IFLTAB(KLOCKB) + IWORD(2)
         CALL lockdss (IFLTAB(KHANDL), 3, IBYTE, IWORD(2), IERR)
         IF (IERR.NE.0) THEN
C           Some one else has requested access, go to mode 2.
            IFLTAB(KMULT) = 2
            IF (MLEVEL.GE.1) WRITE (MUNIT, 80) IFLTAB(KUNIT)
 80         FORMAT(' -----DSS--- zckmul6, File set to multi-user access',
     *              ', unit:',I6)
C           Let go of it, if we are not in the middle of a write
            IF ((IFLTAB(KLOCK).EQ.2).AND.(.NOT.LWRITE)) THEN
               CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
            ENDIF
         ENDIF
      ENDIF
C
C
      RETURN
C
      END

