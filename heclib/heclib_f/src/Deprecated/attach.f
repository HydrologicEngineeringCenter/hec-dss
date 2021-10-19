      SUBROUTINE ATTACH (IUNIT,CKYWRD,CDFALT,CONTRL,CNAME,ISTAT)
C
C-----------------------------------------------------------------------
C
C     GETS EXECUTION LINE PARAMETERS
C     WHERE EXECUTION LINE IS LIKE:
C     MYPROG INPUT=MYIN,OUTPUT=MYOUT
C
C     CKYWRD IS THE 'KEY WORD' CHARACTER STRING
C     CDFALT IS THE DEFAULT FILE (OR PARAMETER) NAME, CHARACTER
C     CONTRL IS USED TO DEFINE FILE ATTRIBUTES.
C     CNAME IS A VARIABLE CHARACTER STRING, RETURNED WITH
C     THE PARAMETER ENTERED ON THE EXECUTION LINE, OR THE
C     STRING CDFALT IF NO EXECUTION PARAMETER WITH KEY.
C
C     ATTACH SHOULD BE CALLED AT THE BEGINNING OF YOUR PROGRAM
C     END THE CALLS TO ATTACH WITH A CALL TO ATTEND.
C
C     DEFAULT PARAMETERS ARE DISPLAYED BY USING A '?' AS THE
C     EXECUTION PARAMETER, I.E.:
C     MYPROG,?
C
C-----------------------------------------------------------------------
C
      CHARACTER CKYWRD*(*), CDFALT*(*), CONTRL*(*), CNAME*(*), CLIN*(*)
      CHARACTER*256 CKYARY, CDFSUB, CKYSUB
      CHARACTER CSVWRD(40)*15
      DIMENSION CKYARY(40), CKYSUB(40)
      CHARACTER CSTAT*1, CACCS*1, CTYPE*1, CFORM*1, CMODE*1, CPRMP*1,
     +          CNOP*3, CATVER*30, CLINE*132
C
      LOGICAL LFIRST, LINAC, LDFALT, LQUES
C
      SAVE LFIRST,LINAC,LQUES,CKYARY,CKYSUB,CSVWRD,JKS,KS, KSV
C
      DATA LFIRST /.TRUE./, LINAC /.TRUE./, LQUES /.FALSE./
      DATA CKYARY /40*'  '/
      DATA NLINE /0/, JSTAT /0/
C-----------------------------------------------------------------------
C
      DATA CATVER /' ATTACH  VER 2.7 (Jan 1992)'/
C
C-----------------------------------------------------------------------
C  DEFINE MACHINE DEPENDENT STANDARD INPUT/OUTPUT UNIT
C  UNIT 0 & 0 FOR THE PC AND 0 & 3 FOR THE HARRIS.
C
C         ISTDIN = 0                                                    L
C         ISTDOT = 0                                                    L
C
C     ISTDIN = 0                                                        H
C     ISTDOT = 3                                                        H
C
      ISTDIN = 5                                                        Mu
      ISTDOT = 6                                                        Mu
C-----------------------------------------------------------------------
C
      JKS    = 40
      NKYWRD = LEN (CKYWRD)
      NDFALT = LEN (CDFALT)
      NCNTRL = LEN (CONTRL)
      NNAME  = LEN (CNAME)
      LDFALT = .TRUE.
      ISTAT = 0
C
      CALL ATTSUB (CDFALT,CDFSUB,NDFALT)
C
      CNAME(1:NNAME) = ' '
C
      IF (JSTAT .EQ. -3) ISTAT = -3
      IEN = 1
C
      IF (LFIRST) THEN
        LFIRST = .FALSE.
        KSV = 0
        ITYPE = 0
C
C-----------------------------------------------------------------------
C  HARRIS ROUTINE TO DETERMINE IF JOB IS INTERACTIVE OR BATCH
C
C       CALL PTYPE (ITYPE)                                              H
C
C  WHEN ITYPE = 0 THEN THE JOB IS INTERACTIVE, OTHERWISE IT'S
C                       A BATCH JOB.
        IF (ITYPE.NE.0) LINAC = .FALSE.
C-----------------------------------------------------------------------
C
        ISTAT  = -3
C
C-----------------------------------------------------------------------
C  ATTCRK GETS THE EXECUTE LINE AND CRACKS IT FOR FILE
C  SUBSTITUTION.  COMMENT OUT THE CALL FOR A GENERIC VERSION
C  OF ATTACH. (ALL FILES WILL BE PROMPTED).
C-----------------------------------------------------------------------
C
        CALL ATTCRK (CKYARY,CKYSUB,JKS,KS,ISTAT)
C
        JSTAT = ISTAT
C
C  WRITE OUT TITLE TO TERMINAL
C
        IF (ISTAT .LE. -10) THEN
C-----------------------------------------------------------------------
C  HARRIS ROUTINE TO TURNOFF TRACKING
C         CALL TRKOFF                                                   H
C-----------------------------------------------------------------------
          LQUES = .TRUE.
          IF (NLINE.GT.0) WRITE (ISTDOT,100) CLINE(1:NLINE)
          IF (ISTAT.EQ.-11) WRITE (ISTDOT,100) CATVER
 100      FORMAT (1X,A)
          WRITE (ISTDOT,105)
 105      FORMAT (T5,'UNIT',T15,'KEYWORD',T32,'*ABREV',T42,
     +    '**MAX',T50,'DEFAULT')
        ELSE
C-----------------------------------------------------------------------
C  HARRIS ROUTINE TO INITIATE TRACKING
C       CALL TRKINT                                                     H
C-----------------------------------------------------------------------
        ENDIF
C
      ELSE
      DO 107 I=1,NKYWRD
      DO 106 K=1,KSV
      JLN = INDEX(CSVWRD(K),' ') -1
      IF (INDEX(CSVWRD(K)(1:JLN),CKYWRD(1:I)).EQ.1) GO TO 107
  106 CONTINUE
      IEN = I
      GO TO 109
  107 CONTINUE
      WRITE (ISTDOT,108)
  108 FORMAT (' ** WARNING TO PROGRAMMER **'/
     +' THIS VERSION OF ATTACH ALLOWS ABBREVIATION OF KEYWORDS. THERE'/
     +' COULD BE A COLLISION OF KEYWORDS AND ABBREVIATION OF KEYWORDS'/
     +' IN YOUR CALLS TO ATTACH.  EITHER CHANGE THE KEYWORD OR'/
     +' REARRANGE THE CALLS SO THAT THE SHORTER OF THE CONFLICTING'/
     +' KEYWORDS APPEAR EARLIER.')
  109 CONTINUE
      ENDIF
C
      KSV = KSV +1
      CSVWRD(KSV) = CKYWRD
C
      CALL ATTDCD (CONTRL,CSTAT,NBIT,CACCS,NLEN,CTYPE,CFORM,CMODE,
     +             CPRMP,CNOP,ISTDOT)
C
      IF (LQUES) THEN
        CALL CHRLNB (CDFSUB, N)
        IF (N.EQ.0) N = 1
        IF (CNOP .NE. 'NOP') WRITE (CNOP,'(I3)') IUNIT
        WRITE (ISTDOT,110) CNOP,CKYWRD,CKYWRD(1:IEN),NNAME,CDFSUB(1:N)
 110    FORMAT (T6,A,T15,A,T32,A,T42,I3,T50,A)
        GO TO 999
      ENDIF
C
      IF (ISTAT .LE. -2) GO TO 125
C
C-----------------------------------------------------------------------
C  LOOK FOR MATCHING KEYWORDS
C-----------------------------------------------------------------------
C
      CALL ATTMAT (CKYARY,CKYWRD,IEN,JKS,KS,JSUB)
      IF (JSUB.NE.0) GO TO 130
C
C-----------------------------------------------------------------------
C  NO MATCH - MOVE DEFAULT INTO CNAME
C-----------------------------------------------------------------------
C
      ISTAT = 0
C
 125  IF (.NOT. LINAC) GO TO 129
C
      IF (CPRMP .EQ. 'Y' .OR. ISTAT .EQ. -3) THEN
        IF (CNOP.EQ.'NOP') THEN
          WRITE (ISTDOT,127) CKYWRD(1:NKYWRD),CDFSUB(1:NDFALT)
 127      FORMAT (' ENTER THE CHARACTER STRING FOR ',A,' [',A,'] ')
        ELSE
          WRITE (ISTDOT,128) CKYWRD(1:NKYWRD),CDFSUB(1:NDFALT)
 128      FORMAT (' ENTER THE FILENAME FOR ',A,' [',A,'] ')
        ENDIF
        KS = KS + 1
        READ (ISTDIN,'(A)') CATVER
        IF (CATVER .EQ. ' ') GO TO 129
        JLN = INDEX(CATVER,' ') -1
        IF (CATVER(1:JLN) .EQ. CDFSUB(1:NDFALT)) GO TO 129
        CKYSUB(KS) = CATVER
        JSUB = KS
        GO TO 130
      ENDIF
C
 129  CNAME = CDFSUB
      GO TO 135
C
C-----------------------------------------------------------------------
C  MOVE SUBSTITUTE INTO CNAME
C-----------------------------------------------------------------------
C
 130  CNAME     = CKYSUB(JSUB)
      CKYARY(JSUB) = ' '
      ISTAT     = -1
      LDFALT    = .FALSE.
C
 135  IF (CNOP .EQ. 'NOP') GO TO 999
C
      CALL ATTOPN(IUNIT,CKYWRD,CNAME,ISTDIN,ISTDOT,CSTAT,NBIT,CACCS,
     + NLEN,CTYPE,CFORM,CMODE,LINAC,LDFALT,ISTAT)
C
      GO TO 999
C
C-----------------------------------------------------------------------
C-- ENTRY POINT ATTEND -------------------------------------------------
C-----------------------------------------------------------------------
C
      ENTRY ATTEND
      KSV = 0
C
      IF (LQUES) THEN
        WRITE (ISTDOT,200)
 200  FORMAT (/T5,'* ABREV - SHORTEST ABBREVIATION ALLOWED FOR KEYWORD'/
     + T5,'** MAX - MAXIMUM # OF CHARACTERS FOR FILENAME (OR STRING)')
        STOP
      ENDIF
C
      DO 220 K=1,KS
        IF (CKYARY(K) .EQ. ' ') GO TO 220
        LQUES = .TRUE.
        WRITE (ISTDOT,210) CKYARY(K)
 210    FORMAT(' UNRECOGNIZED KEYWORD - ',A)
 220  CONTINUE
C
      IF (LQUES) STOP
C
      RETURN
C
C-----------------------------------------------------------------------
C-- ENTRY POINT ATTSET -------------------------------------------------
C-----------------------------------------------------------------------
C
      ENTRY ATTSET (CLIN)
C
      CLINE = CLIN
      CALL CHRLNB (CLINE, NLINE)
      RETURN
C
 999  CONTINUE
      IF (LFIRST) KSTAT = ISTAT
      LFIRST = .FALSE.
      IF ((KSTAT.LE.-2).AND.(ISTAT.EQ.0)) ISTAT = KSTAT
      RETURN
      END

