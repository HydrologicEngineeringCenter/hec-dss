      SUBROUTINE zgetag6 (IFLTAB, CPATH, CTAG)
C
C
C     Generate a tag for zwrite6
C     If a tag scheme is set use that.
C     If not, use a capital T followed by the sequence number.
C
C     Written by Bill Charley at HEC, 1989
C
C
      INTEGER IFLTAB(*)
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      CHARACTER CPATH*(*), CTAG*(*), C*8
C
      INCLUDE 'zdsskz.h'
C
C
      CTAG = ' '
C
      IF (IFLTAB(KTAGS).GT.0) THEN
C     Tag scheme used
      C = ' '
      CALL zupath (CPATH, IBPART, IEPART, ILPART, ISTAT)
C
      DO 20 I=1,8
      ILOC = ((I-1) * 2) + KTAGS
      JVAL = IFLTAB(ILOC)
C
      IF (JVAL.GT.6) THEN
      C(I:I) = CHAR(JVAL)
C
      ELSE IF (JVAL.GT.0) THEN
      IF (IFLTAB(ILOC+1).LE.ILPART(JVAL)) THEN
      J = IBPART(JVAL) + IFLTAB(ILOC+1) - 1
      C(I:I) = CPATH(J:J)
      ENDIF
C
      ELSE IF ((JVAL.LT.0).AND.(JVAL.GT.-7)) THEN
      JVAL = IABS(JVAL)
      K = ISCAN(CPATH, IBPART(JVAL), ILPART(JVAL), ' -@&_+.:;', 1, 9, J)
      IF (K.GT.0) THEN
      K = K + IFLTAB(ILOC+1) - 1
      IF (K.LT.IEPART(JVAL)) THEN
      C(I:I) = CPATH(K+1:K+1)
      ENDIF
      ENDIF
C
      ELSE
      ENDIF
C
 20   CONTINUE
C
      ELSE
C
C     Use the sequence number
      IFLTAB(KSEQNO) = IFLTAB(KSEQNO) + 1
      C = 'T'
      WRITE (C(2:), '(I7)') IFLTAB(KSEQNO)
      ENDIF
C
      CALL REMBLK (C, CTAG, N)
C
      RETURN
      END

