      SUBROUTINE zread6 ( IFLTAB, CPATH, NPATH, IHEAD, NHEAD,
     * IDATA, NDATA, IPLAN, LFOUND)
C
C
C     Replaced by zreadx6
C     zread6 is used where only small integers are stored
C     (zreadx6 and zwritex6 stores large integers)
C
C
C     IPLAN is a code:
C     IPLAN = 0:  Use limits NHEAD and NDATA, but do not update NHEAD,
C                 and NDATA
C     IPLAN = 1:  Use limits, but update NHEAD and NDATA
C     IPLAN = 2:  Same as 1, but do not write a warning message if data
C                 is not found
C
C     **** NHEAD AND NDATA MUST BE VARIABLES!!! *****
C
C     Written by Bill Charley at HEC, 1989.
C
      INTEGER IFLTAB(*), IIHEAD(2)
      INTEGER IHEAD(*), IDATA(*), IARRAY(2)
      CHARACTER CPATH*(*)
      LOGICAL LEND, LFOUND
C
      COMMON /WORDS/ IWORD(10)
C
      INCLUDE 'zdsskz.h'
C
C
      CALL CHRLNB (CPATH(1:NPATH), NP)
      CALL zreadx6 (IFLTAB, CPATH(1:NP), IIHEAD, 2, NIHEAD, IHEAD, 0, I,
     * IHEAD, 0, JH, IDATA, 0, JD, 0, LFOUND)
      IF (IFLTAB(KSTAT).NE.0) RETURN
C
      IF (LFOUND) THEN
C
C     Was this data stored by the actual zwrite6?
      IF (NIHEAD.EQ.2) THEN
      NH = IIHEAD(1)
      ND = IIHEAD(2)
      MH = ((NH - 1) / IWORD(8)) + 1
      MD = ((ND - 1) / IWORD(8)) + 1
      IF ((MH.NE.JH).OR.(MD.NE.JD)) THEN
C     No, report number rounded up (if necessary)
      NH = JH * IWORD(8)
      ND = JD * IWORD(8)
      ENDIF
C
      ELSE
      NH = JH * IWORD(8)
      ND = JD * IWORD(8)
      ENDIF
C
C     Can we do a straight forward retrieve?
      MH = MIN0(NH,NHEAD) / IWORD(8)
      MD = MIN0(ND,NDATA) / IWORD(8)
C
      IF ((MH.EQ.JH).AND.(MD.EQ.JD)) THEN
      CALL zrdbuf6 (IFLTAB, CPATH(1:NP), IHEAD, JH, MH,
     * IDATA, JD, MD, LEND, IPLAN, LFOUND)
C
      ELSE
C     Need to read data and header as portions.  This will not
C     be done too often, so we can make a few calls to zrdbuf6.
      NHA = MIN0(NH,NHEAD)
      NDA = MIN0(ND,NDATA)
      JH = ((NHA - 1) / IWORD(8)) + 1
      JD = ((NDA - 1) / IWORD(8)) + 1
C
      IF (NHA.GT.0) THEN
      CALL zrdbuf6 (IFLTAB, CPATH(1:NP), IHEAD, MH, I,
     * IDATA, 0, I, LEND, IPLAN, LFOUND)
      IF (MH.NE.JH) THEN
C     Retrieve the last small integer word of the header
      CALL zrdbuf6 (IFLTAB, CPATH(1:NP), IARRAY, 1, I,
     * IDATA, 0, I, LEND, IPLAN, LFOUND)
      IHEAD(NHA) = IARRAY(1)
      ENDIF
      ENDIF
C
      IF (NDA.GT.0) THEN
      CALL zrdbuf6 (IFLTAB, CPATH(1:NP), IHEAD, 0, I,
     * IDATA, MD, I, LEND, IPLAN, LFOUND)
      IF (MD.NE.JD) THEN
C     Retrieve the last small integer word of the data
      CALL zrdbuf6 (IFLTAB, CPATH(1:NP), IHEAD, 0, I,
     * IARRAY, 1, I, LEND, IPLAN, LFOUND)
      IDATA(NDA) = IARRAY(1)
      ENDIF
      ENDIF
C
      ENDIF
C
      IFLTAB(KRBNPA) = 0
C
      ELSE
C
      NH = 0
      ND = 0
C
      ENDIF
C
C
      IF (IPLAN.NE.0) THEN
      NHEAD = MIN0(NH,NHEAD)
      NDATA = MIN0(ND,NDATA)
      ENDIF
C
      IFLTAB(KRBNPA) = -1
      RETURN
C
      END

