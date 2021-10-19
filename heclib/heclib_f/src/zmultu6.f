      SUBROUTINE zmultu6 ( IFLTAB, LOCK, LFLUSH)
      implicit none
C
C     zmultu6 takes care of all multiple user access
C     and loads in the permanent section of the file,
C     when called to lock the file for a write request.
C     It also dumps changed buffers to disk when an
C     unlock request is made.
C
C     This routine is always called at the beginning
C     and end of every change to the DSS file.
C
C     **** THIS ROUTINE IS SYSTEM DEPENDENT *****
C
C     LOCK IS A FLAG TO INDICATE IF THE OPERATION IS
C     TO BEGIN LOCK, OR HAS BEEN COMPLETED
C
C         LOCK = TRUE, BEGIN WRITE REQUEST
C         LOCK = FALSE, WRITE COMPLETE
C
C        LFLUSH = TRUE, Force read of permanent section on Lock,
C                       and flush buffers on un-lock
C        LFLUSH = FALSE, Only lock and un-lock file
C
C
C     IF IFLTAB(KMULT) = 0,  Exclusive open / Single user only mode
C        (The file was opened so that no one else can access the file)
C     IF IFLTAB(KMULT) = 1,  Read only mode, but locks are supported
C        (someone else can lock the file)
C     IF IFLTAB(KMULT) = 2,  Standard multi-user mode with locks
C     IF IFLTAB(KMULT) = 3,  Multi-user "advisory".  This locks the
C        file on the first write and only un-locks on close, or if
C        someone else requests access (where the mode goes to 2).
C        This is much faster if we might be the only one using the file.
C     IF IFLTAB(KMULT) = 4, Exclusive access with locks for WRITING only
C     IF IFLTAB(KMULT) = 5, Exclusive access with locks for both reading
C        and writing.  NOT ACTIVE!!!
C     IF IFLTAB(KMULT) = 6,  Release exclusive access (internal flag)
C
C     IF IFLTAB(KLOCK) = 1, The file is currently not locked
C     IF IFLTAB(KLOCK) = 2, The file is currently locked
C
C     IFLTAB(KLOCKB) contains the first byte position of the
C        record used in the file for locking
C     The first word indicates the file is locked
C     The second word is a request to lock the file (another
C        program already has the file locked, and need to check
C        this occasionally)
C     The third word, when combined with the first two, indicate
C        that the file is exclusively locked and is unaccessible
C        to other programs until it is closed.  This is used, for
C        example, when a squeeze is going on.  Other programs
C        should generally gracefully exit.
C
C     The MODE (second) argument for LOCKDSS is as follows:
C        0 - Unlock
C        1 - Lock.  Wait if unavailable.
C        2 - Lock.  Do not wait if unavailable.
C        3 - Test for lock.  (Do not lock.)
C
C
C
      LOGICAL LOCK, LFLUSH
      INTEGER IFLTAB(*)
      integer I,J,IADD,IERR,ISIZE,IUNIT,JERR,KERR,IWORD
      integer*8 IBYTE
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdsslz.h'
C
      COMMON /WORDS/ IWORD(10)
C
C
C
C
      IUNIT = IFLTAB(KUNIT)
      IF (MLEVEL.GE.12) WRITE (MUNIT, 20) IUNIT, IFLTAB(KMULT),
     * IFLTAB(KLOCK), LOCK, LFLUSH
 20   FORMAT (T5,'----DSS--- DEBUG:  Enter zmultu6;  Unit:',I5,/,
     * T12,'KMULT:',I3,',  KLOCK:',I3,'  LOCK: ',L1,'  FLUSH:',L1)
C
C
C     Check that this is a multiple user access file
      IF (IFLTAB(KMULT).GT.1) THEN
C
      IF (LOCK) THEN
C
C     If the file has already been locked, ignore this request
C     (this allows a smart program to lock and save buffers
C     before zwrite6 does)
      IF (IFLTAB(KLOCK).NE.2) THEN
C
C     Only lock a small part of the file for DOS and unix
      IF (IFLTAB(KMULT).EQ.4) THEN
C        Exclusive lock - lock the lock record
         ISIZE = IWORD(2) * 3
         IBYTE =  IFLTAB(KLOCKB)
         CALL lockdss (IFLTAB(KHANDL), 2, IBYTE, ISIZE, IERR)
         IF (IERR.NE.0) THEN
            WRITE ( MUNIT, 30) IUNIT
 30         FORMAT (/,' ----- DSS ERROR ----*',/
     *       ' Unable to lock file for Exclusive access.   Unit: ',I5)
            CALL zabort6 (IFLTAB, 210, 'zmultu6', IUNIT, IFLTAB(KLOCK),
     *       'Can not EXCLUSIVELY lock file')
            RETURN
         ENDIF
      ELSE
C
      IBYTE =  IFLTAB(KLOCKB)
      CALL lockdss (IFLTAB(KHANDL), 2, IBYTE, IWORD(2), IERR)
      ENDIF
C
C
C     If we have an "advisory lock" (IFLTAB(KMULT) = 3), and cannot
C     access the file because it is in use, place a lock request on
C     the word following the locked word, and see if the other user
C     will unlock the file (go to mode 2)
      IF (IERR.NE.0) THEN
C
C        Do a quick test to see if we have access to the file
C        (Does someone else have the file in exclusive access mode?)
         IBYTE = IFLTAB(KLOCKB) + (IWORD(2) * 2)
         CALL lockdss (IFLTAB(KHANDL), 3, IBYTE, IWORD(2), JERR)
         IF (JERR.NE.0) THEN
C           Can not lock the exclusive access word.  Be sure this
C           is not just a fluke (someone else could be testing lock
C           at the same time!
C           DO 32 I=1,10                                                u
C              CALL WAITS (1.0)                                         u
C              CALL lockdss (IFLTAB(KHANDL), 3, IBYTE, IWORD(2), JERR)  u
C              IF (JERR.EQ.0) GO TO 35                                  u
C32         CONTINUE                                                    u
            CALL lockdss (IFLTAB(KHANDL), 1, IBYTE, IWORD(2), JERR)     Md
            CALL lockdss (IFLTAB(KHANDL), 0, IBYTE, IWORD(2), KERR)     Md
            IF (JERR.EQ.0) GO TO 35                                     Md
C           We do not have access to lock this file!
            WRITE ( MUNIT, 60) IUNIT
            CALL zabort6 (IFLTAB, 210, 'zmultu6', IUNIT, IFLTAB(KLOCK),
     *       'Can not lock file')
            RETURN
         ENDIF
C
 35      CONTINUE
         IF (IFLTAB(KMULT).EQ.3) THEN
             IFLTAB(KMULT) = 2
             IF (MLEVEL.GE.1) WRITE (MUNIT, 36) IFLTAB(KUNIT)
 36          FORMAT(' -----DSS--- File set to multi-user access, unit:',
     *              I6)
         ENDIF
         IBYTE = IFLTAB(KLOCKB) + IWORD(2)
         CALL lockdss (IFLTAB(KHANDL), 2, IBYTE, IWORD(2), JERR)
         IF (JERR.EQ.0) IFLTAB(KLOCKR) = 1
C
C        Now try to re-lock the original area until the other program
C        releases it, or we time out (5 minutes!)
         DO 50 I=1,30
C           On MS Windows, this lock will wait up to 10 seconds
            IBYTE = IFLTAB(KLOCKB)
            CALL lockdss (IFLTAB(KHANDL), 1,IBYTE,
     *                     IWORD(2), JERR)
            IF (JERR.EQ.0) THEN
C              Have a release and the file is locked!
C              Leave the request area locked and move on (if locked).
               GO TO 70
            ELSE
C              Write a message every 30 seconds
               J =  MOD (I, 3)
               IF ((J.EQ.1).AND.(MLEVEL.GE.1)) WRITE (MUNIT, 40)
 40   FORMAT (' -----DSS--- Waiting for file access...')
            ENDIF
 50      CONTINUE
C        Hmmm.  Failed lock.  Error out
         WRITE ( MUNIT, 60) IUNIT
 60      FORMAT (//,' ***** DSS ERROR *****',/
     *    ' UNABLE TO LOCK FILE FOR MULTIPLE USER ACCESS; UNIT: ',I5)
         CALL zabort6 (IFLTAB, 210, 'zmultu6', IUNIT, IFLTAB(KLOCK),
     *    'Can not lock file')
         RETURN
C
 70   CONTINUE
C
C
      ELSE
C        Successful lock.  Lock the request area just in case
C        another program begins to access this file
         IF (IFLTAB(KMULT).EQ.2) THEN
             IBYTE = IFLTAB(KLOCKB) + IWORD(2)
             CALL lockdss (IFLTAB(KHANDL), 2, IBYTE, IWORD(2), JERR)
             IF (JERR.EQ.0) IFLTAB(KLOCKR) = 1
         ENDIF
      ENDIF
C
C     On unix, we need to be compatiable with DSS versions prior to 6-K
C     call oldLockDSS (IFLTAB(KHANDL), 1,IFLTAB(KLOCKB),IWORD(2), JERR) u
      IFLTAB(KLOCK) = 2
      IF (MLEVEL.GE.12) WRITE (MUNIT,*)'---DSS:  File Locked'
C
C
C     Read the permanent section of the file
C     On Multiple users systems, be sure this is a
C     phyical read (not just a buffer in memory)!!
      IF (LFLUSH) THEN
         IF (MLEVEL.GE.10) WRITE (MUNIT,*)'---DSS:  zmultu6:  Read Perm'
         CALL zrdprm6 (IFLTAB, .TRUE.)
      ENDIF
C
      ENDIF
C
      ELSE
C
C     Unlock the file.
C
C
C     If we are in an exclusive write lock mode, don't unlock file
      IF ((IFLTAB(KWLOCK).NE.1).AND.(IFLTAB(KMULT).NE.4)) THEN
C
C     First dump all buffers to disk
      IF (LFLUSH) THEN
         IF (MLEVEL.GE.10)WRITE(MUNIT,*)'---DSS:  zmultu6: Dump Buffs'
         CALL zbdump6 (IFLTAB, 1)
      ENDIF
C
C     If we have the request area locked, unlock it
      IF (IFLTAB(KLOCKR).NE.0) THEN
         IBYTE = IFLTAB(KLOCKB) + IWORD(2)
         CALL lockdss (IFLTAB(KHANDL), 0, IBYTE, IWORD(2), JERR)
         IFLTAB(KLOCKR) = 0
      ENDIF
C
C     For DOS, check that if un-lock request has been issued
      IF (IFLTAB(KMULT).EQ.3) THEN
         IBYTE = IFLTAB(KLOCKB) + IWORD(2)
         CALL lockdss (IFLTAB(KHANDL), 3, IBYTE, IWORD(2), IERR)
         IF (IERR.NE.0) IFLTAB(KMULT) = 2
      ENDIF
C
C
      IF ((IFLTAB(KLOCK).EQ.2).AND.(IFLTAB(KMULT).LE.2)) THEN           Md
C     Compatability for unix DSS versions prior to 6-K
C     IF ((IFLTAB(KLOCK).EQ.2).AND.(IFLTAB(KMULT).LE.3)) THEN           u
C
C        Be sure buffers are written to disk!!
         IF (LFLUSH) THEN
           IF(MLEVEL.GE.11)WRITE(MUNIT,*)'---DSS:  File Buffers Flushed'
            CALL flushf (IFLTAB(KHANDL), IERR)
            IF (IERR.EQ.-1) THEN
C               If the network goes down, we will come here.  Wait for
C               a few seconds then try again.
               CALL WAITS (5.0)
                CALL flushf (IFLTAB(KHANDL), IERR)
             ENDIF
             IF ((IERR.NE.0).AND.(MLEVEL.GT.0)) WRITE (MUNIT,75) IUNIT,
     *           IERR
 75          FORMAT (' ---DSS - Error:  Unable to flush DSS file',
     *       ' to disk.',/,'  Unit:',I5,'  Code:',I5)
         ENDIF
C
C     On unix, we need to be compatiable with DSS versions prior to 6-K
C     call oldLockDSS (IFLTAB(KHANDL), 0,IFLTAB(KLOCKB),IWORD(2), JERR) u
C
         CALL lockdss (IFLTAB(KHANDL), 0,IFLTAB(KLOCKB),IWORD(2),IERR)
C
         IFLTAB(KLOCK) = 1
         IF (MLEVEL.GE.10) WRITE (MUNIT,*)'---DSS:  File Unlocked'
C        If we have an error unlocking, I don't know what to do!
         IF ((IERR.NE.0).AND.(MLEVEL.GE.3)) THEN
            WRITE ( MUNIT, 80) IUNIT, IERR
 80         FORMAT (' ---DSS - Caution:  Unable to unlock DSS file',
     *       ' for multiple user access.',/,'  Unit:',I5,'  Code:',I5)
         ENDIF
      ELSE IF (IFLTAB(KMULT).EQ.6) THEN
C        Release exclusive access
         IF (LFLUSH) THEN
            IF(MLEVEL.GE.10)WRITE(MUNIT,*)'---DSS:  zmultu6: Dump Buffs'
            CALL zbdump6 (IFLTAB, 1)
            CALL flushf (IFLTAB(KHANDL), IERR)
         ENDIF
         ISIZE = IWORD(2) * 3
         CALL lockdss (IFLTAB(KHANDL), 0, IFLTAB(KLOCKB), ISIZE, IERR)
         IFLTAB(KLOCK) = 1
      ENDIF
      ENDIF
      ENDIF
C
C
C
      ELSE
C
      IF (LOCK) THEN
      IF (IFLTAB(KLOCK).NE.2) THEN
      IFLTAB(KLOCK) = 2
C
C     Read the permanent section of the file (Always!)
C     Don't need to force (not multi-user)
      IF (LFLUSH) THEN
         IF (MLEVEL.GE.10) WRITE (MUNIT,*)'---DSS:  zmultu6:  Read Perm'
         IADD = 1
         CALL zgtrec6 (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .TRUE.)
      ENDIF
      ENDIF
C
      ELSE
      IF (LFLUSH) THEN
         IF (MLEVEL.GE.10) WRITE(MUNIT,*)'---DSS:  zmultu6:  Dump Buffs'
         CALL zbdump6 (IFLTAB, 1)
      ENDIF
      IFLTAB(KLOCK) = 1
      ENDIF
C
      ENDIF
C
      IF (MLEVEL.GE.12) WRITE (MUNIT, 120) IUNIT, IFLTAB(KMULT),
     * IFLTAB(KLOCK), LOCK
 120  FORMAT (T5,'----DSS--- DEBUG:  Exit zmultu6;  Unit:',I5,/,
     * T12,'KMULT:',I3,',  KLOCK:',I3,'  LOCK: ',L1)
C
      RETURN
      END
C     subroutine oldLockDSS (ihandle, mode, position, size, istat)      u
c
C     integer ihandle, mode, position, size, istat                      u
C     integer oldPosition                                               u
c
c     Compatability for versions of dss prior to 6-K
c
C     irec = position / 512                                             u
C     ibyte = position - (irec * 512)                                   u
C     oldPosition = (irec * 127) + 1 + ibyte                            u
c
C     call lockdss (ihandle, mode, oldPosition, size, istat)            u
c
C     return                                                            u
C     end                                                               u

