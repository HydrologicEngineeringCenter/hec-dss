      SUBROUTINE zwlock6 (IFLTAB, CACT, ISTAT)
C
      integer i, j, ifsize, nadd, jrec, istat, nlock
C
C     Set the (opened) file in an exclusive lock or
C     write lock state.  In this state, no one else may
C     access the file.  The write lock is the exclusive lock
C     plus the permenant record and the hash table (if it fits)
C     remains in memory, and no records are specifically
C     flushed to the disk until the write lock is set to "OFF",
C     or the file is closed.  In the event of a crash or abort,
C     the file MUST be squeezed, because the pointer tables may
C     not have been updated.
C
C     CACT is a character string that describes the action to take.
C     The three possible actions are:
C        "ON"  - Locks the file and keeps pointers in memory.
C        "EXCLUSIVE" - Just locks the file (disk is updated after
C                      every write).
C        "OFF"  - Flushes pointers and buffers to disk, and unlock
C                 the file (applies to both "ON" and "EXCLUSIVE").
C
C
      INTEGER IFLTAB(*)
C
      CHARACTER CACT*(*)
C
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) IFLTAB(KUNIT), CACT
 20   FORMAT (/,T10,'-----DSS---Debug: Enter zwlock6,  Unit:',I5,
     * ',  Lock action: ',A)
C
C
      IF (IFLTAB(KMULT).LT.2) THEN
         ISTAT = -2
         GO TO 800
      ENDIF
C
      ISTAT = 0
C
      IF (CACT(1:2).EQ.'ON') THEN
C     Be sure that we have no more than two records locked at this time
C     (from other file(s))
      NLOCK = 0
      DO 40 I=1,MXBUFF
      IF (LOCKBF(I)) NLOCK = NLOCK + 1
 40   CONTINUE
C
      IF (J.LE.2) THEN
C     Write to the file a flag indicating Write Lock on,
C     in case there is an abort, we will know the file size is unknown!!
      IFSIZE = IFLTAB(KFSIZE)
      IFLTAB(KFSIZE) = -1357
      NADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
      CALL zbdump6 (IFLTAB, 1)
C
      CALL zmultu6 ( IFLTAB, .TRUE., .TRUE.)
      IFLTAB(KWLOCK) = 1
c
      IFLTAB(KFSIZE) = IFSIZE
C     Now lock records 1 and 2
      NADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
      LOCKBF(JBUFF) = .TRUE.
      JREC = 2
      CALL zgetad6 (NADD, JREC, 1)
      CALL zgtrec6 (IFLTAB, INFO, 1, NADD, .TRUE.)
      LOCKBF(JBUFF) = .TRUE.
C
      ELSE
      ISTAT = -1
      ENDIF
C
      ELSE IF (CACT(1:2).EQ.'OF') THEN
C
      DO 100 I=1,MXBUFF
      IF (JBUNIT(I).EQ.IFLTAB(KHANDL)) LOCKBF(I) = .FALSE.
 100  CONTINUE
C
C     Be sure the buffer area is cleared and
C     release any multiple user access
      IFLTAB(KWLOCK) = 0
      IF (IFLTAB(KMULT).GT.2) IFLTAB(KMULT) = 2
      CALL zmultu6 ( IFLTAB, .FALSE., .TRUE.)
C
      ENDIF
C
C
 800  CONTINUE
      IF (MLEVEL.GE.12) WRITE (MUNIT,820) ISTAT
 820  FORMAT (T8,'-----DSS---Debug: EXIT zwlock6,  Status:',I3)
      RETURN
      END

