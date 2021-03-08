      SUBROUTINE zrdprm6 (IFLTAB, LSAVE)
      implicit none
C     Force a physical read of the permanent section from the DSS file.
C     (Need to force this read because of multi-user access).
C
C     Written by Bill Charley at HEC, 1993.
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      INTEGER IFLTAB(*)
      LOGICAL LSAVE
      integer IUNIT,IHANDL,ISTAT,JADD,IREC,NWORD,I
C
C
C
      IUNIT = IFLTAB(KUNIT)
      IHANDL = IFLTAB(KHANDL)
      ISTAT = 0
C
      IF (MLEVEL.GE.15) WRITE (MUNIT,20) IHANDL
 20   FORMAT (T12,'----DSS---Debug:  Enter zrdprm6;  Unit:',I5)
C
C
      JADD = 1
      CALL zgetrw6 (JADD, IREC, NWORD)
C
C     Is the record already in memory (need to clear if it is)
      JBUFF = 0
      DO 40 I=1,MXBUFF
          IF ((IREC.EQ.JCREC(I)).AND.(IHANDL.EQ.JBUNIT(I))) JBUFF = I
 40   CONTINUE
C
      IF (JBUFF.NE.0) THEN
C         Don't clear if that record is dirty
          IF (JWRITE(JBUFF).EQ.0) THEN
              JCREC(JBUFF) = -1
              LSBUFF(JBUFF) = .FALSE.
          ENDIF
      ENDIF
C
C
C     Now read the first record
      CALL zgtrec6 (IFLTAB, IFLTAB(KPERM), NPERM, JADD, LSAVE)
C
C
 800  CONTINUE
C
      RETURN
C
C
      END

