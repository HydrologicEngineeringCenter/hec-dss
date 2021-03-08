      SUBROUTINE HOL2CH ( IHOL, CHR, NWORDS)
C
C     TRANSFER HOLLERITH IHOL TO CHARACTER CHR ON WORD BOUNDARIES
C     NUMBER OF WORDS TO TRANSFER = NWORDS
C
C     ***** A MACHINE DEPENDENT SUBROUTINE ******
C
C
      INTEGER IHOL(*), INT
      CHARACTER CHR*(*)
      CHARACTER STR*(8)                                                 HMu
C
      EQUIVALENCE (INT,STR)                                             HMu
C
      EXTERNAL BKDATW
      COMMON /WORDS/ IWORD(10)
C
C
C     This code for machines where you may
C     equivalence characters to integers.
C
      IBEG = 1                                                          HMu
      NCMW = IWORD(7)
      IF ((NCMW.LT.2).OR.(NCMW.GT.10)) THEN
C     WRITE ( 3, *) ' ERROR - BLOCK DATA BKDATW NOT LOADED'             H
      WRITE ( 6, *) ' ERROR - BLOCK DATA BKDATW NOT LOADED'             LMu
      !CALL ABORT
      CALL BKDATW                                                       HMu
      ENDIF
C
      DO 10 I=1,NWORDS                                                  HMu
      INT = IHOL(I)                                                     HMu
      IEND = IBEG + NCMW - 1                                            HMu
C     IF (IWORD(4).EQ.0) THEN                                           Hu
      CHR(IBEG:IEND) = STR(1:NCMW)                                      HMu
C     ELSE                                                              Hu
C     CHR(IBEG:IBEG) = STR(4:4)                                         Hu
C     CHR(IBEG+1:IBEG+1) = STR(3:3)                                     Hu
C     CHR(IBEG+2:IBEG+2) = STR(2:2)                                     Hu
C     CHR(IBEG+3:IBEG+3) = STR(1:1)                                     Hu
C     ENDIF                                                             Hu
      IBEG = IEND + 1                                                   HMu
 10   CONTINUE                                                          HMu
C
C
C     For Lahey, use holchr always
C     For Microsoft, use holchr if we need to swap bytes!
      IF (IWORD(4).NE.0) THEN                                           M
      KWORDS = NWORDS * IWORD(7)                                        ML
      CALL HOLCHR (IHOL, 1, KWORDS, CHR, 1)                             ML
      ENDIF                                                             M
C
C
C     THIS CODE FOR NO ALTERNATIVES
C     IHOL IS DECLARED *2
C     KWORDS = NWORDS * IWORD(7)/2
C     DO 10 I=1,KWORDS
C     IPOS = (I*2) - 1
C     CALL GETHOL (IHOL(I), 1, ICH)
C     CHR(IPOS:IPOS) = CHAR(ICH)
C     IPOS = IPOS + 1
C     CALL GETHOL (IHOL(I), 2, ICH)
C     CHR(IPOS:IPOS) = CHAR(ICH)
C10   CONTINUE
C
      RETURN
      END

