      SUBROUTINE CH2HOL ( CHR, IHOL, NWORDS)
C
C     TRANSFER CHARACTER CHR TO HOLLERITH ON WORD BOUNDARIES
C     NUMBER OF WORDS TO TRANSFER = NWORDS
C
C     ***** A MACHINE DEPENDENT SUBROUTINE ******
C     NOTE: CHANGE THIS PARAMETER STATEMENT FOR THE CORRECT
C           NUMBER OF CHARACTERS PER MACHINE WORD
C
C
      INTEGER IHOL(*), INT
      CHARACTER CHR*(*)
      CHARACTER STR*8
      INTEGER LENGTH
C
C
      EQUIVALENCE (INT,STR)                                             HMu
C
      EXTERNAL BKDATW
      COMMON /WORDS/ IWORD(10)
C
C
C     THIS CODE FOR MACHINES WHERE YOU MAY
C     EQUIVALENCE CHARACTERS AND INTEGERS
C
      NCMW = IWORD(7)
      IF ((NCMW.LT.2).OR.(NCMW.GT.10)) THEN
      WRITE ( 6, *) ' ERROR - BLOCK DATA BKDATW NOT LOADED'
 !     CALL ABORT
      CALL BKDATW                                                       HMu
      ENDIF
C
      LENGTH = LEN(CHR)
      IBEG = 1                                                          HMu
      DO 10 I=1,NWORDS                                                  HMu
      IEND = IBEG + NCMW - 1                                            HMu
      IF (IEND.GT.LENGTH) THEN
          IEND = LENGTH
          STR = ' '
      ENDIF
      IF (IBEG.GT.LENGTH) GO TO 20
C     IF (IWORD(4).EQ.0) THEN                                           Hu
      STR(1:NCMW) = CHR(IBEG:IEND)                                      HMu
C     ELSE                                                              Hu
C     STR(4:4) = CHR(IBEG:IBEG)                                         Hu
C     STR(3:3) = CHR(IBEG+1:IBEG+1)                                     Hu
C     STR(2:2) = CHR(IBEG+2:IBEG+2)                                     Hu
C     STR(1:1) = CHR(IBEG+3:IBEG+3)                                     Hu
C     ENDIF                                                             Hu
      IHOL(I) = INT                                                     HMu
      IBEG = IEND + 1                                                   HMu
 10   CONTINUE                                                          HMu
C
C
C     For Lahey, use chrhol always
C     For Microsoft, use chrhol if we need to swap bytes!
 20   CONTINUE
      IF (IWORD(4).NE.0) THEN                                           M
      KWORDS = NWORDS * IWORD(7)                                        ML
      CALL CHRHOL (CHR, 1, KWORDS, IHOL, 1)                             ML
      ENDIF                                                             M
C
C
C     THIS CODE FOR NO ALTERNATIVES
C     IHOL IS DECLARED *2
C     KWORDS = NWORDS * IWORD(7) / 2
C     DO 10 I=1,KWORDS
C     GET EACH BYTE
C     IPOS = (I*2) - 1
C     CALL PUTHOL (IHOL(I), 1, ICHAR(CHR(IPOS:IPOS)))
C     IPOS = IPOS + 1
C     CALL PUTHOL (IHOL(I), 2, ICHAR(CHR(IPOS:IPOS)))
C10   CONTINUE
C
      RETURN
      END

