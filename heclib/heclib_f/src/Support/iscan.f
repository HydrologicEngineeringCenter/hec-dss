      INTEGER FUNCTION ISCAN ( CSTRB, NBEG1, NLEN1, CSTR2, NBEG2,
     * NLEN2, IPOS2)
C
C     SCAN CHARACTER STRING CSTRB FOR ANY CHARACTERS IN CSTR2
C     WHERE CSTRB BEGINS AT NBEG1 WITH LENGTH NLEN1
C     AND CSTR2 BEGINS AT NBEG2 WITH LENGTH NLEN2
C     RETURN THE CHARACTER FOUND IN CSTR2 IN VARIABLE IPOS2
C
C     IF NLEN1 IS POSITIVE, SEARCH IS MADE FROM LEFT TO RIGHT
C     IF NLEN1 IS NEGATIVE, SEARCH IS MADE FROM RIGHT TO LEFT
C
C     ISCAN IS ZERO IF NO MATCHES ARE FOUND
C
C     YOU SHOULD USE INDEX OR INDEXR IF NLEN IS 1
C
C
      CHARACTER CSTRB*(*), CSTR2*(*)
C
C
      ISCAN = 0
      IPOS2 = 0
      IF (NLEN1.GE.0) THEN
      NEND1 = NBEG1 + NLEN1 - 1
      ELSE
      NEND1 = NBEG1 + NLEN1 + 1
      ENDIF
      NEND2 = NBEG2 + NLEN2 - 1
C
      IF (NLEN2.EQ.1) THEN
      IF (NLEN1.GT.0) THEN
      ILOC = INDEX (CSTRB(NBEG1:NEND1),CSTR2(NBEG2:NEND2))
      ELSE
      ILOC = INDEXR (CSTRB(NEND1:NBEG1),CSTR2(NBEG2:NEND2))
      ENDIF
      IF (ILOC.NE.0) THEN
      IPOS2 = NBEG2
      ISCAN = ILOC + MIN0(NBEG1,NEND1) - 1
      ELSE
      ISCAN = 0
      IPOS2 = 0
      ENDIF
      RETURN
      ENDIF
C
      IF (NLEN1.GT.0) THEN
      DO 10 I=NBEG1,NEND1,1
      IPOS2 = INDEX (CSTR2(NBEG2:NEND2),CSTRB(I:I))
      IF (IPOS2.GT.0) THEN
      ISCAN = I
      RETURN
      ENDIF
 10   CONTINUE
C
      ELSE
C
      DO 20 I=NBEG1,NEND1,-1
      IPOS2 = INDEX (CSTR2(NBEG2:NEND2),CSTRB(I:I))
      IF (IPOS2.GT.0) THEN
      ISCAN = I
      RETURN
      ENDIF
 20   CONTINUE
C
      ENDIF
C
      RETURN
      END
