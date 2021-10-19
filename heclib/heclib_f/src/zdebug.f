      SUBROUTINE zdebug ( MUNIT, IARRAY, IADD, ILEN)
C
C
C     Prints coded information in IARRAY (usually IFLTAB)
C     to unit MUNIT.
C     IADD is the beginning address
C     ILEN is the length of the array to print.
C
C     Written by Bill Charley at HEC, 1989
C
      COMMON /WORDS/ IWORD(10)
C
      INTEGER IARRAY(*)
      INTEGER IBYTES(10)
      CHARACTER C1*24, CREAL*12
      EQUIVALENCE (I6,R)
      INTEGER ISHORT(2)
      EQUIVALENCE (ISHORT,I6)
C
C
      WRITE (MUNIT, 20)
 20   FORMAT (T5, 'Address', T16, 'Rec Word  Offset', T38, 'Large Int',
     * T48, 'Char', T61, 'Real', T69, 'Small Ints', T85, 'Bytes')
C
      C1 = '  '
      NWPW = IWORD(2) / IWORD(1)
C
      DO 100 I=1,ILEN
      CALL HOL2CH ( IARRAY(I), C1, NWPW)
      DO 30 J=1,IWORD(2)
      N = ICHAR(C1(J:J))
      IF ((N.LT.32).OR.(N.GT.126)) C1(J:J) = '~'
 30   CONTINUE
C
      JADD = IADD + I - 1
      CALL zgetrw6 (JADD, JREC, JWRD)
C
      I6 = IARRAY(I)
C
      DO 40 N=1,IWORD(2)
      CALL GETHOL (I6, N, IBYTES(N))
 40   CONTINUE
      IF (IBYTES(4).LT.254) THEN
      CALL XREALC (R, CREAL, 1, 12, 2)
      ELSE
      CREAL = '============'
      ENDIF
      WRITE (MUNIT,50,ERR=100)JADD, JREC, JWRD, I, IARRAY(I),
     * C1(1:IWORD(2)), CREAL, ISHORT, (IBYTES(K),K=1,IWORD(2))
 50   FORMAT (1X,I10,I8,I4,'  (',I3,')',I16,1X,A,1X,A,1X,2I8,1X,10I4)
 100  CONTINUE
      RETURN
C
      END

