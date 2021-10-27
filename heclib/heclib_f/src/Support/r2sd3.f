      SUBROUTINE R2SD3 ( R, I2 )
C ------
C ------ Form real number R into a compacted 3 significant digit
C ------ 2 byte representation.  Number will always be rounded to
C ------ 3 decimal digits.
C ------
C ------    S E E E E E M M M M M M M M M M
C ------
C ------    S - Sign of mantissa (0,1)
C ------    E - Biased exponent (1-31) (0 Special case)
C ------    M - Binary coded decimal digits (100-999) (1001-1005 Res)
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
      REAL R, RA, RL
C     INTEGER*3 I4, I2                                                  H
      INTEGER*4 I4, I2                                                  MLu
      INTEGER IP, IEXPON, IMANT, ISGN
C ------
      IF ( R .EQ. 0.0 ) THEN
      I4 = 1001
      ELSE
      ISGN = 0
      IF ( R .LT. 0.0 ) ISGN = 32
      RA = ABS ( R )
      RL = LOG10 ( RA )
      IP = INT ( RL ) - 2
      IEXPON = IP + 15
      IF ( IEXPON .LT. 1 ) THEN
C ------ Underflow
      I4 = 1002
      IF ( ISGN .GT. 0 ) I4 = I4 + 1
      ELSE IF ( IEXPON .GT. 31 ) THEN
C ------ Overflow
      I4 = 1004
      IF ( ISGN .GT. 0 ) I4 = I4 + 1
      ELSE
C ------ OK Number
C     WRITE (*,*) ' RA = ',RA,'  IP = ',IP
      RA = RA / 10.0**IP
      IMANT = NINT ( RA )
      IF (IMANT .EQ. 1000) THEN
      IMANT = 100
      IEXPON = IEXPON + 1
      ENDIF
      IEXPON = IEXPON + ISGN
      I4 = IMANT + IEXPON * 1024
      ENDIF
      ENDIF
C ------
C
C     CALL GETHOL ( I4, 2, ITMP )                                       H
      CALL GETHOL ( I4, 1, ITMP )                                       MLu
      CALL PUTHOL ( I2, 1, ITMP )
C
C     CALL GETHOL ( I4, 3, ITMP )                                       H
      CALL GETHOL ( I4, 2, ITMP )                                       MLu
      CALL PUTHOL ( I2, 2, ITMP )
C
      RETURN
      END

