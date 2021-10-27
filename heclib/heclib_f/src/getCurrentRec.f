      SUBROUTINE getCurrentRec (icurrent)
C
C
C
      INTEGER icurrent
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
C
      SAVE ALL
C
      icurrent = CURRENT_NUMB;
C
      RETURN
C
      END

