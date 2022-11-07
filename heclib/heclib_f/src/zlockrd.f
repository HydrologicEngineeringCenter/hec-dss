      SUBROUTINE zlockrd (IFLTAB, lockFlag, istat)
C
      implicit none
C
C     Sets, releases or tests for a read lock
C     A read lock indicates that the file is in use
C     and another process cannot have exclusive access
C     Used by squeeze.
C
C     The lock flag follows the regular locking area
C
C     The lockFlag (second) argument  is as follows:
C        0 - Unlock
C        1 - Lock for read.
C        2 - Test to see if another process has this file locked.  (Do not lock.)
C        3 - Test for number processes have this file lock.  (Do not lock.)
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
      INTEGER IWORD
C
      INTEGER IFLTAB(*), lockFlag, istat
      INTEGER  i, IERR
      integer lockPos
C
C
      if ((lockFlag.eq.0).and.(ifltab(KLOCKRD).eq.0)) then
C       Shouldn't get here
        istat = 0
        return
      endif
C
C
      if ((lockFlag.eq.0).and.(ifltab(KLOCKRD).gt.0)) then
C       Unlock the lock read area
        CALL lockdss(IFLTAB(KHANDL), 0,ifltab(KLOCKRD), IWORD(2), IERR)
        ifltab(KLOCKRD) = 0
C       ignore an error on unlocking
        istat = 0
        return
      endif
C
C
      if (lockFlag.eq.1) then
        lockPos = IFLTAB(KLOCKB) + (4 * IWORD(2))
        do 40 i=1,100
          CALL lockdss (IFLTAB(KHANDL), 2, lockPos, IWORD(2), IERR)
          if (IERR.eq.0) then
             IFLTAB(KLOCKRD) = lockPos
             ISTAT = 0
             return
          endif
          lockPos = lockPos + IWORD(2)
 40     continue
C       Hmm....  cannot lock file
        ISTAT = -1
        return
      endif
C
C
C     Test lock read
      if (lockFlag.ge.2) then
        lockPos = IFLTAB(KLOCKB) + (4 * IWORD(2))
        ISTAT = 0
        do 60 i=1,100
C         Don't check my own read lock
          if (lockPos.ne.ifltab(KLOCKRD)) then
            CALL lockdss(IFLTAB(KHANDL), 3, lockPos,IWORD(2),IERR)
            if (IERR.ne.0) then
               ISTAT = ISTAT + 1
               if (lockFlag.eq.2) then
C                 Just a test to see if read locked
                  return
               endif
            endif
          endif
          lockPos = lockPos + IWORD(2)
 60     continue
        return
      endif
C
C     Shouldn't get here
      ISTAT = -1
C
      RETURN
C
      END

