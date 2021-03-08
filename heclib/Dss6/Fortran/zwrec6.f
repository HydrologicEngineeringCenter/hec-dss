      SUBROUTINE zwrec6 (IUNIT, IREC, IARRAY, NWORDS, ISWAP,
     *                  ISTAT, JSTAT)
#ifdef _MSC_VER
      USE IFCORE
#endif
      implicit none
C
C     Write physical record to disk
C
C     Written by Bill Charley at HEC, 1984.
C

C
      INTEGER IARRAY(NWORDS)
C
      CHARACTER C1*4, C2*4
      EQUIVALENCE (C1,I1), (C2,I2)
      COMMON /ZDSSSZ/ IARY2(128)
      integer IARY2
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      INTEGER IOFSET
      INTEGER(8) IOFSET8, IPOS8
      integer I1,I2,IUNIT,IREC,ISWAP,ISTAT,JSTAT
      integer I,ISIZE,NTRANS,NWORDS
C
      INTEGER IWARN
      SAVE IWARN
      DATA IWARN /0/
C
      IF (MLEVEL.GE.15) WRITE (MUNIT, 20) IUNIT, IREC, ISWAP
 20   FORMAT (T3,'=====DSS===Debug: Physical Write;  Unit',I5,
     * '  Record',I8,',  Swap:',I3)
      
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
         WRITE(MUNIT, 21)
 21      FORMAT(/' ***** DSS: WARNING! Nearing maximum file size limit',
     *           ' *****'/)
         WRITE (MUNIT, 20) IUNIT, IREC             
         ENDIF
         IF (IREC.GT.16777200) THEN
         WRITE(MUNIT, 22)
 22      FORMAT(/' ***** DSS: Maximum file size limit reached *****'/)
         WRITE (MUNIT, 20) IUNIT, IREC
         ISTAT = -1
         GO TO 800
         ENDIF
      ENDIF      
C
C
      ISTAT = 0
      JSTAT = 0
C
C     Switch bytes for big endian computers
      IF (ISWAP.NE.0) THEN
      DO 40 I=1,NWORDS
      I1 = IARRAY(I)
      C2(1:1) = C1(4:4)
      C2(2:2) = C1(3:3)
      C2(3:3) = C1(2:2)
      C2(4:4) = C1(1:1)
      IARY2(I) = I2
 40   CONTINUE
      ENDIF
C
C
C     MS-DOS and Unix Assembly I-O
***************  This code for small file (2 GB) size *****
      IF (.NOT.L8GB) THEN
         IOFSET = (IREC-1) * 512
         IF (IOFSET.LT.0) THEN
            WRITE(MUNIT, 50)
            GO TO 800
         ENDIF
      ENDIF
C
***************  This code for large file (8 GB) size *****
      IOFSET8 = (INT(IREC, 8)-1) * 512
      IF (IOFSET8.LT.0) THEN
         IOFSET = -1
         WRITE(MUNIT, 50)
         GO TO 800
      ENDIF
C
      CALL seekf64 (IUNIT, 0, IOFSET8, IPOS8, ISTAT)
C
      IF (ISTAT.NE.0) THEN
         IF ((IREC.GT.0).AND.(IOFSET8.LT.0)) WRITE(MUNIT, 50)
 50      FORMAT(/' ***** DSS: Maximum file size limit reached *****'/)
         GO TO 800
      ENDIF
C     IF (IOFSET.LT.30000000) THEN
C     ISIZE = 508
C     ELSE
      ISIZE = 512
C     ENDIF
      IF (ISWAP.EQ.0) THEN
C     WRITE ( UNIT=IUNIT, REC=IREC, IOSTAT=ISTAT) IARRAY                F
      CALL writf (IUNIT, IARRAY, ISIZE, ISTAT, NTRANS)
      ELSE
C     WRITE ( UNIT=IUNIT, REC=IREC, IOSTAT=ISTAT) IARY2                 F
      CALL writf (IUNIT, IARY2,  ISIZE, ISTAT, NTRANS)
      ENDIF
      IF ((ISTAT.EQ.0).AND.(NTRANS.NE.ISIZE)) ISTAT = -1
C     CALL WRITF (IUNIT, IARRAY, NRECL, ISTAT, NTRANS)
C     IF ((ISTAT.EQ.0).AND.(NTRANS.NE.NRECL)) ISTAT = -1
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
C
      END

