      SUBROUTINE TZSHFT ( IFIRST,JDATE,MTIME,NXTDAT,MDTZ )
C
C    ADJUST OBSERVATION TIMES BASED ON A TIME ZONE ADJUSTMENT
C    <MDTZ> IN MINUTES.
C
      DIMENSION JDATE(*),MTIME(*),NXTDAT(*)
      INTEGER*4 JDATE                                                   M
C
c     write(*,*)' tzshft called '
      IDAT = IFIRST
   10 CONTINUE
      MTIME(IDAT) = MTIME(IDAT) + MDTZ
c     write(*,*)'mtime(idat)',mtime(idat)
      IF ( MTIME(IDAT).GT.1440 ) THEN
         JDATE(IDAT) = JDATE(IDAT) + MTIME(IDAT) / 1440
         MTIME(IDAT) = MOD ( MTIME(IDAT),1440 )
      ELSE
      ENDIF
c     IF ( MTIME(IDAT).LE.0 ) THEN
      IF ( MTIME(IDAT).Lt.0 ) THEN
c     write(*,*)'jdate(idat)',jdate(idat),mtime(idat)
         JDATE(IDAT) = JDATE(IDAT) + ( MTIME(IDAT) / 1440 ) - 1
         MTIME(IDAT) = 1440 + MOD( MTIME(IDAT),1440 )
c     write(*,*)'jdate(idat)',jdate(idat),mtime(idat)
      ELSE
      ENDIF
      IF ( NXTDAT(IDAT).GT.0 ) THEN
         IDAT = NXTDAT(IDAT)
         GO TO 10
      ELSE
      ENDIF
      RETURN
      END
