      SUBROUTINE zdswap6 (IARRAY, NWORDS)
C
C
C     Swaps words for converting doubles on big-endian machines
C     to little endian style.
C     This is so DSS files will be compatitable across platforms
C
C
      INTEGER IARRAY(NWORDS), NWORDS, ITEMP
C
      DO 20 I=1,NWORDS,2
         ITEMP = IARRAY(I)
         IARRAY(I) = IARRAY(I+1)
         IARRAY(I+1) = ITEMP
 20   CONTINUE
C
      RETURN
      END

