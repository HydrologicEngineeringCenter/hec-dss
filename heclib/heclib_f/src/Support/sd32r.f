      SUBROUTINE SD32R ( I2, R )
C ------
C ------ Form real number R from a compacted 3 significant digit
C ------ 2 byte representation.  Number will always be rounded to
C ------ 3 decimal digits.
C ------
C ------    Range of R : -d.dd E+16 to -d.dd E-14
C ------                  0.00                         (1001)
C ------                 +d.dd E-14 to +d.dd E+16
C ------
C ------    Negative overflow   = -1.00 E+38           (1005)
C ------    Negative underflow  = -1.00 E-38           (1003)
C ------    Positive underflow  = +1.00 E-38           (1002)
C ------    Positive overflow   = +1.00 E+38           (1004)
C ------
      REAL R, RA
C     INTEGER*3 I4, I2, I1024                                           H
      INTEGER*4 I4, I2, I1024                                           MLu
      INTEGER IP, IEXPON, IMANT
      LOGICAL LNEG
C ------
      I4 = 0
C
      CALL GETHOL ( I2, 1, ITMP )
C     CALL PUTHOL ( I4, 2, ITMP )                                       H
      CALL PUTHOL ( I4, 1, ITMP )                                       MLu
C
      CALL GETHOL ( I2, 2, ITMP )
C     CALL PUTHOL ( I4, 3, ITMP )                                       H
      CALL PUTHOL ( I4, 2, ITMP )                                       MLu
C
      IF ( I4 .EQ. 1001 ) THEN
      R = 0.0
      ELSE IF ( I4 .GT. 1023 ) THEN
      IEXPON = I4 / 1024
      LNEG = .FALSE.
      IF ( IEXPON .GT. 32 ) THEN
      LNEG = .TRUE.
      IEXPON = IEXPON - 32
      ENDIF
      I1024 = 1024
      IMANT = MOD ( I4, I1024 )
      IP = IEXPON - 15
      RA = IMANT
      R = RA * 10.0**IP
      IF (LNEG) R = -R
      ELSE IF ( I4 .EQ. 1002 ) THEN
C     R = +1.0 E-38                                                     Hu
      R = +1.0 E-37                                                     ML
      ELSE IF ( I4 .EQ. 1003 ) THEN
C     R = -1.0 E-38                                                     Hu
      R = -1.0 E-37                                                     ML
      ELSE IF ( I4 .EQ. 1004 ) THEN
C     R = +1.0 E+38                                                     Hu
      R = +1.0 E+37                                                     ML
      ELSE IF ( I4 .EQ. 1005 ) THEN
C     R = -1.0 E+38                                                     Hu
      R = -1.0 E+37                                                     ML
      ENDIF
      RETURN
      END

