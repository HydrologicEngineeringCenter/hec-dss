      SUBROUTINE zdebug1 (IVAL, IVAL8, CRVAL, CSTRNG, IBYTES)
C
C
C     Return an IFLTAB word in different possible formats.
C     CRVAL is a character string with the REAL representation
C     CHARACTER CRVAL*12
C     CHARACTER CSTRNG*4
C     INTEGER IBYTES(4)
C
C     Written by Bill Charley at HEC
C
      INTEGER(4) IVAL
      INTEGER(8) IVAL8
      CHARACTER CRVAL*(*)
      CHARACTER CSTRNG*(*)
      INTEGER IBYTES(*)
C
      EXTERNAL CONVI4TOI8
      INTEGER(8) CONVI4TOI8
C
      REAL RV
      INTEGER IV
      EQUIVALENCE (IV, RV)
C
      COMMON /WORDS/ IWORD(10)
C
C
      IVAL8 = CONVI4TOI8(IVAL)

      call strcpy(CSTRNG, ' ')
      NWPW = IWORD(2) / IWORD(1)
C
      CALL HOL2CH (IVAL, CSTRNG, NWPW)
      DO 30 J=1,IWORD(2)
         N = ICHAR(CSTRNG(J:J))
         IF ((N.LT.32).OR.(N.GT.126)) CSTRNG(J:J) = '~'
 30   CONTINUE
C
      DO 40 N=1,IWORD(2)
         CALL GETHOL (IVAL, N, IBYTES(N))
 40   CONTINUE
      IF (IBYTES(4).LT.254) THEN
         IV = IVAL
         CALL XREALC (RV, CRVAL, 1, 12, 2)
      ELSE
         call strcpy(CRVAL, '============')
      ENDIF
C
      RETURN
C
      END

