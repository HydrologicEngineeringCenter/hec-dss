      SUBROUTINE ATTDCD (CONTRL,CSTAT,NBIT,CACCS ,NLEN,CTYPE,CFORM,
     +                   CMODE ,CPRMP,CNOP,ISTDOT)
C
C-----------------------------------------------------------------------
C  DECODES THE CONTRL FIELD BEING PASSED FROM ATTACH
C-----------------------------------------------------------------------
C
      CHARACTER CONTRL*(*), CSTAT*1, CACCS*1, CTYPE*1, CFORM*1,
     + CMODE*1, CPRMP*1, CNOP*3
      CHARACTER CSTRNG*256, CPARM*256, CDLM*1, CI*1, CJ*1, CL*2, CR*1
      CHARACTER FMT*4
C
      DIMENSION IBF(10), ILF(10), IDT(10), IDP(10), ITBL(128),
     +          JBF(10), JLF(10)
C
      DATA FMT/'(IX)'/
C
C-----------------------------------------------------------------------
C  SET DEFAULTS
C-----------------------------------------------------------------------
C
      CSTAT  = 'U'
      CACCS  = 'S'
      CFORM  = 'F'
      CMODE  = 'N'
      CPRMP  = 'N'
      CNOP   = ' '
      CSTRNG = ' '
      NLEN   = 512
C
C-----------------------------------------------------------------------
C  SET HARRIS DEFAULTS - NBIT OF 116 IS ACCESS CODE: PR PW PD
C-----------------------------------------------------------------------
C
C     NBIT = 116                                                        H
C     CTYPE  = 'U'                                                      H
C
      NCNTRL = LEN(CONTRL)
      IF (NCNTRL .EQ. 0) GO TO 999
      JBG = NINDX(CONTRL(1:NCNTRL),' ')
C
      IF (JBG .EQ. 0) GO TO 999
C
      IF (CONTRL(JBG:) .EQ. 'NOP') THEN
        CNOP = 'NOP'
        GO TO 999
      ENDIF
C
C-----------------------------------------------------------------------
C  CHANGE COMMAS TO SPACE, REMOVE UNNECESSARY SPACES AND MOVE
C  INTO CSTRNG
C-----------------------------------------------------------------------
C
      J = 1
C
      CSTRNG(1:1) = CONTRL(JBG:JBG)
C
      DO 100 I=JBG+1,NCNTRL-1
        CI = CONTRL(I:I)
C
        IF (CI .EQ. ',') CI = ' '
C
        IF (CI .EQ. ' ') THEN
          CJ = CONTRL(I+1:I+1)
C
C  IS THE SPACE BEFORE ANOTHER SPACE OR AN EQUAL OR SLASH?
C
          IF (CJ .EQ. ' ' .OR.
     +        CJ .EQ. '=' .OR.
     +        CJ .EQ. '/') GO TO 100
C
          CJ = CONTRL(I-1:I-1)
C
C  IS THE SPACE AFTER ANOTHER SPACE OR AN EQUAL OR SLASH?
C
          IF (CJ .EQ. ' ' .OR.
     +        CJ .EQ. '=' .OR.
     +        CJ .EQ. '/') GO TO 100
        ENDIF
C
        J           = J + 1
        CSTRNG(J:J) = CI
 100  CONTINUE
C
      JLN             = J + 1
      CSTRNG(JLN:JLN) = CONTRL(NCNTRL:NCNTRL)
C
      IF (CSTRNG(JLN:JLN) .NE. ' ') THEN
        JLN             = JLN + 1
        CSTRNG(JLN:JLN) = ' '
      ENDIF
C
C-----------------------------------------------------------------------
C  NOW SPLIT THE FIELDS FROM WITHIN CSTRNG
C-----------------------------------------------------------------------
C
      CDLM = ' '
C
      CALL SETDLM (1,CDLM,1,1,ITBL)
      CALL SETDLM (2,CDLM,1,0,ITBL)
      CALL SETDLM (3,CDLM,1,0,ITBL)
      CALL FINDLM (CSTRNG,1,JLN,NF,IBF,ILF,IDT,IDP,ITBL)
C
C-----------------------------------------------------------------------
C  NOW START DECIPHERING THE FIELDS
C-----------------------------------------------------------------------
C
      DO 150 I=1,NF
        IST = IBF(I)
        IEN = IST + ILF(I) - 1
C
        CL = CSTRNG(IST:IST+1)
        CR = CSTRNG(IST+2:IST+2)
C
C-----------------------------------------------------------------------
C  LOOK FOR FILE STATUS - NEW, OLD, OR UNKNOWN
C-----------------------------------------------------------------------
C
        IF (CL .EQ. 'S=') THEN
          CSTAT = CR
C
C  LOOK FOR HARRIS SPECIFIC FILE ACCESS LEVEL. IE: PR/PW/PD
C
C         IF (CR .NE. 'N'.AND. CR .NE. 'U') GO TO 150                   H
C
C         NSLSH = INDEX(CSTRNG(IST:IEN),'/')                            H
C         IF (NSLSH .EQ. 0) GO TO 150                                   H
C
C         NSLSH = NSLSH + IST                                           H
C         CPARM = CSTRNG(NSLSH:IEN)                                     H
C
C  FUNCTION IACBIT RETURNS BIT PATTERN FOR CALL TO RETYPE
C
C         NBIT = IACBIT(CPARM)                                          H
C
C-----------------------------------------------------------------------
C  LOOK FOR FILE ACCESS TYPE - SEQUENTIAL OR DIRECT
C-----------------------------------------------------------------------
C
        ELSE IF (CL .EQ. 'A=') THEN
          CACCS = CR
C
          IF (CR .EQ. 'S') GO TO 150
          IF (CR .NE. 'D') GO TO 130
C
          NSLSH = INDEX(CSTRNG(IST:IEN),'/')
C
          IF (NSLSH .EQ. 0) GO TO 150
C
          NSLSH = NSLSH + IST
          JLN   = IEN - NSLSH + 1
          CPARM = CSTRNG(NSLSH:IEN)
          CDLM  = '/'
C
          CALL SETDLM (1,CDLM,1,1,ITBL)
          CALL FINDLM (CPARM,1,JLN,JF,JBF,JLF,IDT,IDP,ITBL)
C
          IST = JBF(1)
          IEN = IST + JLF(1) - 1
          JLN = IEN - IST + 1
C
          WRITE (FMT(3:3),'(I1)') JLN
          READ (CPARM(IST:),FMT) NLEN
C
          IF (JF .EQ. 2) THEN
            IST   = JBF(2)
            CTYPE = CPARM(IST:IST)
          ENDIF
C
C-----------------------------------------------------------------------
C  FORM OF FILE (FORMATTED OR UNFORMATTED)
C-----------------------------------------------------------------------
C
        ELSE IF (CL .EQ. 'F=') THEN
C
          IF (CR .NE. 'U' .AND. CR .NE. 'F') GO TO 130
          CFORM = CR
C
C-----------------------------------------------------------------------
C  HARRIS SPECIFIC - MODE OF FILE (SHARED, EXCLUSIVE, OR NORMAL)
C-----------------------------------------------------------------------
C
C       ELSE IF (CL .EQ. 'M=') THEN                                     H
C         IF (CR .NE. 'S' .AND.                                         H
C    +        CR .NE. 'E' .AND.                                         H
C    +        CR .NE. 'N') GO TO 130                                    H
C
C         CMODE = CR                                                    H
C
C-----------------------------------------------------------------------
C  FLAG FOR PROMPTING OF FILENAME (YES OR NO)
C-----------------------------------------------------------------------
C
        ELSE IF (CL.EQ.'P=') THEN
          IF (CR .NE. 'Y' .AND. CR .NE. 'N') GO TO 130
          CPRMP = CR
        ENDIF
C
C-----------------------------------------------------------------------
C  KEEP LOOPING
C-----------------------------------------------------------------------
C
        GO TO 150
C
C-----------------------------------------------------------------------
C  ERROR -- UNRECOGNIZED ARGUMENT
C-----------------------------------------------------------------------
C
 130    WRITE (ISTDOT,135) CL,CR
 135    FORMAT (' UNRECOGNIZED ARGUMENT: [',A2,A1,'] IN CALL TO ATTACH.
     +DEFAULT USED.)')
C
C-----------------------------------------------------------------------
C  CONTINUE LOOP
C-----------------------------------------------------------------------
C
 150  CONTINUE
C
 999  RETURN
      END

