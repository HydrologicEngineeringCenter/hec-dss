      SUBROUTINE CHRHOL(C,IS,NC,IC,JS)
C
C     CONVERT FROM CHARACTER VARIABLE "C" TO AN ALPHANUMERIC
C     VARIABLE IC (A3) AND COUNT "NC"
C     BEGIN AT "IS" AND "JS" RESPECTIVELY.
C
      CHARACTER C*(*)
      INTEGER IC(*)
      integer initialize
      data initialize /-1/
C
C
C     MAKE SURE IWORDS IS INITIALIZED
      IF (initialize.LT.0) then
        CALL BKDATW
        initialize = 0
      endif
C     MOVE EACH CHARACTER FROM C TO IC
C
      I=IS-1
      DO 10 J=JS,JS+NC-1
      I=I+1
      if (I.LE.Len(C)) then
C      write(*,*)'I,J,C(I:I),ICHAR(C(I:))=',I,J,C(I:I),ICHAR(C(I:I)))
      CALL PUTHOL(IC,J,ICHAR(C(I:I)))
      endif
10    CONTINUE
      RETURN
      END

