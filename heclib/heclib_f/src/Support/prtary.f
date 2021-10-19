      SUBROUTINE PRTARY ( ARRAY, NARRAY )
C
      INTEGER NARRAY, ISTART, IEND, ILINES
C
      REAL ARRAY(*)
C
      IF (MOD(NARRAY,8) .EQ. 0) THEN
         ILINES = NARRAY/8
      ELSE
         ILINES = NARRAY/8 + 1
      ENDIF
C
      DO 100 I=1,ILINES
         ISTART = 8*I - 7
         IEND   = 8*I
         IF (IEND .GE. NARRAY) IEND = NARRAY
         WRITE (*,'(1X,8F9.2)') (ARRAY(J),J=ISTART,IEND)
  100 CONTINUE
C
      WRITE (*,*) ' '
C
      RETURN
      END

