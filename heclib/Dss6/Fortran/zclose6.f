      SUBROUTINE zclose6 ( IFLTAB)
C
C
C     Close file, then print file status
C
C     Written by Bill Charley at HEC, 1988.
C
      INTEGER IFLTAB(*), JSTAT
      CHARACTER CSCRAT*64  !SHOULD PUT THIS IN COMMON
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      COMMON /WORDS/ IWORD(10)
      COMMON /TST /NUMBREADS
C
C

C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zclose6;  Unit:',I5)
C
      IUNIT = IFLTAB(KUNIT)
      IHANDL = IFLTAB(KHANDL)
C
C     If this file was set in a lock write mode, release the locks,
C     then dump the buffers
      DO 40 I=1,MXBUFF
      IF (JBUNIT(I).EQ.IHANDL) LOCKBF(I) = .FALSE.
 40   CONTINUE
C
C     Be sure the buffer area is cleared and
C     release any multiple user access
      IF (IFLTAB(KMULT).GE.4) THEN
         IFLTAB(KMULT) = 6
         CALL zmultu6 ( IFLTAB, .FALSE., .TRUE.)
      ELSE
         IF (IFLTAB(KMULT).GT.2) IFLTAB(KMULT) = 2
         IFLTAB(KWLOCK) = 0
         CALL zmultu6 ( IFLTAB, .FALSE., .TRUE.)
      ENDIF
      CALL ZLOCKRD(IFLTAB, 0, JSTAT)
C
C
C     Informative File Status Message:
C
C     IF (IFLTAB(KMULT).GT.0) CALL zrdprm6 (IFLTAB, .FALSE.)
C     Compute the file size
      FILSIZ = REAL(IFLTAB(KFSIZE)) -1.
      FILSIZ = FILSIZ * (512./508.)
C     Compute the size in kilobytes
      BYTESK = FILSIZ * FLOAT(IWORD(2)) / 1000.0
C     Compute the amount of dead space
      DEAD = FLOAT(IFLTAB(KDEAD))
      DEADS = (DEAD/FILSIZ) * 100.
C
C     Compute a pointer effiency
      CSCRAT = ' '
      CALL zinqir6(IFLTAB, 'POINT', CSCRAT, IPOINT)
      POINTU = FLOAT(IPOINT) / 100.0
C
C     Get the file name
      CSCRAT = ' '
      CALL zinqir6 (IFLTAB, 'NAME', CSCRAT, ILARGE)
      CALL CHRLNB (CSCRAT, NNAME)
C
C
      IF (MLEVEL.GT.1) WRITE (MUNIT,120) IUNIT, CSCRAT(1:NNAME), POINTU,
     * IFLTAB(KNRECS), BYTESK, DEADS
 120  FORMAT (T5,'-----DSS---zclose6 Unit:',I5,',   File: ',A,/,
     * T16,'Pointer Utilization:',F6.2,/,
     * T16,'Number of Records:',I7,/,T16,'File Size:',F9.1,2X,
     * 'Kbytes',/,T16,'Percent Inactive: ',F5.1)
      IF (MLEVEL.GT.1) WRITE (MUNIT,121) NUMBREADS
 121  FORMAT (  T16,'Number of Reads:', I9)
C
C
C
C     Close the file
C     CLOSE (UNIT=IFLTAB(KUNIT))
      CALL closf (IHANDL, ISTAT)
C
C     Clear the main array
      DO 140 I=1,KEY3
      IFLTAB(I) = 0
 140  CONTINUE
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,800)
 800  FORMAT (T6,'-----DSS---Debug:  Exit  zclose6')
      CALL FLUSH(MUNIT)                                                 Mu
C
      RETURN
      END

