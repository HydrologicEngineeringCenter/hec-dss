      SUBROUTINE zfilst6 (IFLTAB)
C
C
C     Get file status and efficiency of pointers
C
C     Written by Bill Charley at HEC, 1980.
C
      INTEGER IFLTAB(*)
      CHARACTER CSCRAT*70   !SHOULD PUT THIS IN COMMON
      CHARACTER CV*4, CD*7
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssmz.h'
C
      COMMON /WORDS/ IWORD(10)
C
C
C
      IF (MLEVEL.LT.1) RETURN
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zfilst6;  Unit:',I5)
C
C
      CALL CHRFIL (CSCRAT(1:66), '-')
      WRITE (MUNIT, 40) CSCRAT(1:66)
 40   FORMAT (T5,A)
C
C     Force a read of the permenent section of the file
      CALL zrdprm6 (IFLTAB, .FALSE.)
C
C     Get the file name
      CSCRAT = ' '
      CALL zinqir6 (IFLTAB, 'NAME', CSCRAT, ILARGE)
      CALL CHRLNB (CSCRAT, NNAME)
C
C     Informative File Status Message:
C
C     Compute the file size
      FILSIZ = REAL(IFLTAB(KFSIZE)) -1.
C     Compute the size in kilobytes
      BYTESK = FILSIZ * FLOAT(IWORD(2)) / 1000.0
      BYTESK = BYTESK* (512./508.)
C     Compute the amount of dead space
      DEAD = FLOAT(IFLTAB(KDEAD))
      DEADS = (DEAD/FILSIZ) * 100.
C
C     Compute a pointer effiency
      IF (IFLTAB(KHUSED).GT.0) THEN
      X = 3.5 * (FLOAT(IFLTAB(KBNSIZ))/112.)
      POINTU =  FLOAT(IFLTAB(KNRECS)) / (FLOAT(IFLTAB(KHUSED)) * X)
      IF (POINTU.LT.0.01) POINTU = 0.01
      ELSE
      POINTU = 0.0
      ENDIF
C
      CALL zinqir6 (IFLTAB, 'FVERS', CV, ILARGE)
      CALL zinqir6 (IFLTAB, 'FDATE', CD, ILARGE)
C
      WRITE (MUNIT,60) CSCRAT(1:NNAME), CD, CV, IFLTAB(KNRECS), POINTU
 60   FORMAT (T5,'DSS File ',A,';   Created on ',A,';   DSS Version ',A,
     * /,T5,'Number of Records:',I6,';',T37,'Pointer Utilization:',F6.2)
C
      WRITE (MUNIT, 80) BYTESK, DEADS
 80   FORMAT (T5,'File Size:',F9.1,' Kilobytes;',T37,
     * 'Percent Inactive Space: ',F5.2)
C
      IF (IFLTAB(KTABLE).EQ.1) THEN
      CSCRAT = 'Dynamic Hash Table'
      ELSE
      CSCRAT = 'Stable Hash Table '
      ENDIF
C
      WRITE (MUNIT,100) IFLTAB(KHASH), CSCRAT(1:18),
     * IFLTAB(KBNBLK), IFLTAB(KBNSIZ)
 100  FORMAT (T5,'Max Hash Code:',I5,';',T37,A,/,
     * T5,'Numb Bins per Block:',I4,';',T37,'Size of Bin:',I5,' words')
C
      IF (IFLTAB(KHUSED).GT.0) THEN
      ASRCH = REAL(IFLTAB(KNRECS))/REAL(IFLTAB(KHUSED))
      ELSE
      ASRCH = 0.0
      ENDIF
      WRITE (MUNIT,120) IFLTAB(KBINS), IFLTAB(KBOVER), IFLTAB(KHUSED),
     * IFLTAB(KMAXPH), ASRCH, IFLTAB(KMAXHC)
 120  FORMAT (T5,'Bins Used:',I7,';',T37,'Overflow Bins:',I7,/,
     * T5,'Hash Codes Used:',I6,';',T37,'Max Paths for one Hash Code:',
     * I5,/,T5,'Average Number of Paths to search:',F6.1,
     * ';    (Max Hash Code:',I5,')')
C
      CALL CHRFIL (CSCRAT(1:66), '-')
      WRITE (MUNIT, 40) CSCRAT(1:66)
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,800)
 800  FORMAT (T6,'-----DSS---Debug:  Exit  zfilst6')
C
      RETURN
      END
      SUBROUTINE zfilst(IFLTAB)
      INTEGER IFLTAB(*)
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
      call zfilst6 (IFLTAB)
      else
      call zprintFileInfo(ifltab)
      endif
      return
      end

