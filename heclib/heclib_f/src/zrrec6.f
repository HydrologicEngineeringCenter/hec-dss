      SUBROUTINE zrrec6 (IUNIT, IREC, IARRAY, NWORDS, ISWAP,
     *                  ISTAT, JSTAT)
#ifdef _MSC_VER
      USE IFCORE
#endif    
C
C
C     Reads physical record from disk
C
C     Written by Bill Charley at HEC, 1984.
C
         
C
C
      INTEGER IARRAY(NWORDS)
C
      CHARACTER C1*4, C2*4
      EQUIVALENCE (C1,I1), (C2,I2)
      COMMON /ZDSSSZ/ IARY2(128)
      integer IARY2
      COMMON /TST /NUMBREADS
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      INTEGER(8) IOFSET, IREC8, I8,IPOS8
      INTEGER IWARN
      SAVE IWARN
      DATA IWARN /0/
C
      NUMBREADS = NUMBREADS + 1
C
      IF (MLEVEL.GE.15) WRITE (MUNIT, 20) IUNIT, IREC
C     WRITE (MUNIT, 20) IUNIT, IREC
 20   FORMAT (T3,'+++++DSS+++Debug: Physical  Read;  Unit',I5,
     * '  Record',I8)
C
C
      ISTAT = 0
      JSTAT = 0
C
C
C     MS-DOS and Unix Assembly I-O
*      IOFSET = (IREC-1) * 512
*      CALL seekf (IUNIT, 0, IOFSET, IPOS, ISTAT)
*
*     Check for reaching near max file size
*     Max file size = Max int * 4 (bytes per word) = 2,147,483,647 * 4 =  8,589,934,588 bytes 
*     = 8.192 GB limit
*     Max size in recs = 8,589,934,588 / 512 (bytes per rec) = 16,777,215 recs
*     However, if we get to this point, any write will crash or corrupt the file,
*     so we will subtract a little....  16,777,000
C
C    
C                 16,777,000
      IF (IREC.GT.16777000) THEN
         IF (IWARN.EQ.0) THEN
         WRITE(MUNIT, 40)
 40      FORMAT(/' ***** DSS: WARNING! Nearing maximum file size limit',
     *           ' *****'/)
         WRITE (MUNIT, 20) IUNIT, IREC             
         ENDIF
         IF (IREC.GT.16777200) THEN
         WRITE(MUNIT, 45)
 45      FORMAT(/' ***** DSS: Maximum file size limit reached *****'/)
         WRITE (MUNIT, 20) IUNIT, IREC
         ISTAT = -1
         GO TO 800
         ENDIF
      ENDIF
C
C     IOFSET = (IREC-1) * 512      
      IREC8 = INT(IREC, 8) -1
      I8 = 512
      IOFSET = IREC8 * I8
      CALL seekf64 (IUNIT, 0, IOFSET, IPOS8, ISTAT)
C
      IF (ISTAT.NE.0) THEN
         IF ((IREC.GT.0).AND.(IOFSET.LT.0)) WRITE(MUNIT, 50)
 50      FORMAT(/' ***** DSS: Error reading file *****'/)
         GO TO 800
      ENDIF
      IF (ISWAP.EQ.0) THEN
C        READ ( UNIT=IUNIT, REC=IREC, IOSTAT=ISTAT) IARRAY              F
         CALL readf (IUNIT, IARRAY, 508, ISTAT, NTRANS)
C
C        On Windows computers, if the network fails on a remote drive,
C        the status will be returned as 22.  Try a few more times
C        before erroring out
C        IF (ISTAT.EQ.22) THEN                                          d
C           DO 30 I=1,10                                                d
C              CALL WAITS (1.5)                                         d
C              CALL readf (IUNIT, IARRAY, 508, ISTAT, NTRANS)           d
C              IF (ISTAT.NE.22) GO TO 40                                d
C30         CONTINUE                                                    d
C           IF (MLEVEL.GE.1) WRITE (MUNIT, 35) IUNIT                    d
C35         FORMAT (' ERROR:  Network Failure for unit ', I5)           d
C40         CONTINUE                                                    d
C        ENDIF                                                          d
      ELSE
C        READ ( UNIT=IUNIT, REC=IREC, IOSTAT=ISTAT) IARY2               F
         CALL readf (IUNIT, IARY2,  508, ISTAT, NTRANS)
      ENDIF
      IF (ISTAT.NE.0) THEN
          WRITE(*,*)'***** readf fail, status = ',ISTAT
      ENDIF
      IF (NTRANS.NE.508) THEN
          WRITE(*,*)'***** readf fail, NTRANS != 508, = ',NTRANS
      ENDIF
      IF ((ISTAT.EQ.0).AND.(NTRANS.NE.508)) ISTAT = -1
C     CALL READF (IUNIT, IARRAY, NRECL, ISTAT, NTRANS)                  c
C     IF ((ISTAT.EQ.0).AND.(NTRANS.NE.NRECL)) ISTAT = -1                c
C
C     Switch bytes for big endian computers
      IF (ISWAP.NE.0) THEN
      DO 60 I=1,NWORDS
      I1 = IARY2(I)
      C2(1:1) = C1(4:4)
      C2(2:2) = C1(3:3)
      C2(3:3) = C1(2:2)
      C2(4:4) = C1(1:1)
      IARRAY(I) = I2
 60   CONTINUE
      ENDIF
C
C
C
 800  CONTINUE
C     If an error occured, find out what it was, and save it
C     in the common error message area
      IF (ISTAT.NE.0) THEN
          CALL GERROR (CERRMS)
          !IERRMS = IERRNO ()
      ENDIF
C
      RETURN
      END

