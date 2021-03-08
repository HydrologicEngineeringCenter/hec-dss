      SUBROUTINE SHFDI ( INTL,DI )
C
      CHARACTER DI*5,TUNITS(5)*1
      DIMENSION IZINTL(5)
      INTEGER*4 IZINTL, INTL                                            M
C
      DATA IZINTL /1,60,1440,43200,525600 /
      DATA TUNITS /'N','H','D','M','Y' /
C
      DI='DI'
      N = 1
   5  CONTINUE
      IF ( N.LE.4.AND.INTL.GE.IZINTL(N+1) ) THEN
          N=N+1
          GO TO 5
      ENDIF
      DI(3:3) = TUNITS(N)
      NINT = INTL/IZINTL(N)
      IF ( NINT.LT.10 ) THEN
          WRITE ( DI(4:4),'(I1)') NINT
      ELSE
          WRITE ( DI(4:5),'(I2)') NINT
      ENDIF
      RETURN
      END
