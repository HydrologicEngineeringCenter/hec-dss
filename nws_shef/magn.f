      INTEGER FUNCTION MAGN(X)
      CHARACTER*10 CBUFF
      WRITE (CBUFF,'(E10.3)') X
      READ(CBUFF(8:10),'(I3)') MAGN
      MAGN = MAGN - 1
      IP = INDEX(CBUFF,'.')
      I = INTGR(CBUFF,1,IP-1,IERR)
    5 CONTINUE
      IF ( I.GT.0 ) THEN
         MAGN = MAGN - 1
         I=I/10
         GO TO 5
      ENDIF
      RETURN
      END
