      SUBROUTINE GETHOL ( ISTR, IPOS, ICH)
C
C     Gets a Hollerith from ISTR, position IPOS and
C     places it in the rightmost byte of zero filled ICH
C     May cross work boundaries
C
C     ***** THIS IS A MACHINE DEPENDENT ROUTINE ******
C
C     INTEGER   ISTR(*)                                                 Hu
      INTEGER*4 ISTR(*)                                                 M
C     For Lahey INT*4 code, we must use an INT*2 word!!
C     INTEGER*2 ISTR(*)                                                 L
C
C     CHARACTER CVAR*3                                                  H
      CHARACTER CVAR*8                                                  Mu
C     INTEGER   IVAR                                                    Hu
      INTEGER*4 IVAR                                                    M
C     'CHEAT' BY EQUIVALENCING A INTEGER AND CHARACTER
      EQUIVALENCE (IVAR,CVAR)                                           HMu
C
      EXTERNAL BKDATW
      COMMON /WORDS/ IWORD(10)
C
C
C     CHECK THAT WORDS HAS BEEN SET
      NCMW = IWORD(1)
      NCMW = IWORD(2)                                                   M
      IF ((NCMW.LT.2).OR.(NCMW.GT.10)) THEN
C     WRITE ( 3, *) ' ERROR - BLOCK DATA BKDATW NOT LOADED'             H
      WRITE ( 6, *) ' ERROR - BLOCK DATA BKDATW NOT LOADED'             LMu
 !     CALL ABORT
      ENDIF
C
C
C     THIS CODE IS FOR MACHINES WHERE EQUIVALENCING CHARACTER
C     AND INTEGER WORDS IS LEGAL
C
C     CALCULATE THE WORD AND BYTE POSTION
      IWD = (IPOS-1)/NCMW + 1                                           HMu
      IBYTE = IPOS - ((IWD-1)*NCMW)                                     HMu
      IF (IWORD(4).NE.0) IBYTE = NCMW + 1 - IBYTE                       Mu
C
C     SUCK OUT THAT CHARACTER
      IVAR = ISTR(IWD)                                                  HMu
      ICH = ICHAR ( CVAR(IBYTE:IBYTE))                                  HMu
C
C
C     THIS CODE IS FOR MACHINES WHERE INTEGER AND CHARACTER
C     CAN BE PASSED AS SAME ARGUMENT
C     ICH = ICHAR(ISTR(IPOS))
C
C
C
C     This code is where Boolean functions are available.
C     An INTEGER*2 word must be legal
C
C     KWORD = (IPOS-1)/2 + 1                                            L
C     K = MOD(IPOS,2)                                                   L
C
C     IF (K.NE.0) THEN                                                  L
C     Move from the lowest byte
C     ICH: 11111111 11111111 to 00000000 11111111
C     ICH = IAND (ISTR(KWORD), 255)                                     L
C
C     ELSE                                                              L
C     Move from the upper most byte
C     ICH: 11111111 11111111 to 00000000 11111111
C     ICH = ISHFT (ISTR(KWORD), -8)                                     L
C
C     ENDIF                                                             L
C
C     Use math when no other method is available.
C     This procedure will not work when the high order (8th) bit
C     is set.  However, no ANSI characters use this bit, so it is ok
C     as long as the hollerith have been preset to zeros!!
C
C     KWORD = (IPOS-1)/2 + 1
C     K = MOD(IPOS,2)
C
C     IF (K.NE.0) THEN
C     MOVE FROM LOWEST MOST BYTE
C     JCH = ISTR(KWORD)/256
C     JCH = JCH*256
C     ICH = ISTR(KWORD) - JCH
C
C     ELSE
C     MOVE FROM THE UPPER MOST BYTE
C     ICH = ISTR(KWORD)/256
C
C     ENDIF
C
      RETURN
      END

