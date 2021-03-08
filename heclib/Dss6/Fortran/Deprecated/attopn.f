      SUBROUTINE ATTOPN (IUNIT,CKYWRD,CNAME,ISTDIN,ISTDOT,CSTAT,NBIT  ,
     +                   CACCS,NLEN  ,CTYPE,CFORM ,CMODE ,LINAC,LDFALT,
     +                   ISTAT)
C
C-----------------------------------------------------------------------
C        ROUTINE TO OPEN/ASSIGN FILES TO UNITS
C-----------------------------------------------------------------------
C
      CHARACTER     CKYWRD*(*), CNAME*(*)
      CHARACTER*1   CSTAT,      CACCS,     CTYPE,  CFORM, CMODE
      CHARACTER*3   CANS
      CHARACTER*7   CSTT
      CHARACTER*11  CACC,       CFRM
      LOGICAL       LAPND,      LINAC,     LEXIST, LDFALT
C
      NNAME = LEN(CNAME)
C
C        SET PARAMETERS ACCORDING TO CONTROL FIELD
C
      CACC = 'SEQUENTIAL'
      IF (CACCS .EQ. 'D') CACC = 'DIRECT'
C
      CFRM = 'FORMATTED'
      IF (CFORM .EQ. 'U') CFRM = 'UNFORMATTED'
C
      CSTT = 'UNKNOWN'
      IF (CSTAT .EQ. 'N') CSTT = 'NEW'
      IF (CSTAT .EQ. 'O') CSTT = 'OLD'
      IF (CSTAT .EQ. 'S') CSTT = 'SCRATCH'
      IF (CNAME(1:8).EQ.'SCRATCH.') THEN                                MLu
      CSTT = 'SCRATCH'                                                  MLu
      CNAME = ' '                                                       MLu
C     Green Hills on Intergraph does not make a unique name for
C     scratch files.  Thus make a unique name!
C     CALL TEMPNAME (CNAME, IUNIT)                                      u
      ENDIF                                                             MLu
C
      LAPND = .FALSE.
      IF (CNAME(1:1) .EQ. '+') THEN
        LAPND = .TRUE.
        CNAME(1:) = CNAME(2:)
        IF (CACC .EQ. 'DIRECT') THEN
          WRITE (ISTDOT,100) CNAME
 100      FORMAT (' ERROR ** CANNOT APPEND TO A DIRECT ACCESS FILE: ',A)
          STOP
        ENDIF
        CACC = 'APPEND'
      ENDIF
C
C     ITYP = 2                                                          H
C     KLN  = INDEX(CNAME,' ') -1                                        H
C     IF (CNAME(1:1) .EQ. ':') ITYP= -1                                 H
C     IF (CNAME(1:1) .EQ. '@') ITYP= -2                                 H
C     IF (CNAME(1:1) .EQ. '*') ITYP= 1                                  H
C
C     IF (ITYP .NE. 2) THEN                                             H
C       JLN = INDEX(CNAME,' ') -2                                       H
C       IUN = INTGR(CNAME,2,JLN,ISTT)                                   H
C       IF (ISTT .EQ. 0) GO TO 350                                      H
C       IF (ITYP .LE. (-1) ) GO TO 330                                  H
C
C        FALL THROUGH HERE FOR QUALIFIER SYST
C
C     ELSE                                                              H
C
C        MAKE(CREATE) WORK FILES H0-H9 UNBLOCKED FILES
C
C       IF (INDEX(CNAME,' ') .NE. 3) GO TO 110                          H
C       IF (CNAME(1:1) .NE. 'H') GO TO 110                              H
C       READ (CNAME(2:2),'(I1)',ERR=110) JLN                            H
C
C       CALL CDELET (CNAME,IERR)                                        H
C       IF (IERR .NE. 0 .AND. IERR .LT. 21) GO TO 390                   H
C
C       CALL CCREAT (CNAME,200,0,-1,IERR)                               H
C       IF (IERR .NE. 0) GO TO 390                                      H
C
C       CFRM = 'UNFORMATTED'                                            H
C       GO TO 200                                                       H
C     ENDIF                                                             H
C
 110  CONTINUE
      IF (CSTT(1:3).EQ.'SCR') GO TO 200                                 MLu
      INQUIRE (FILE=CNAME,EXIST=LEXIST)
C
      IF (CSTT .EQ. 'NEW') THEN
        CALL CHRLNB (CNAME,KLN)
 120    IF (LAPND .OR. LDFALT) GO TO 200
        IF (CNAME(1:KLN) .EQ. 'CON') GO TO 200                          MLu
        IF (LINAC .AND. LEXIST) THEN
 130      WRITE (ISTDOT,140) CNAME(1:KLN)
 140      FORMAT (1X,'[',A,']',
     +            ' ALREADY EXISTS...DO YOU WISH TO OVERWRITE IT? ')
          READ (ISTDIN,'(A)') CANS
          IF (INDEX(CANS,'Y') .EQ. 1) GO TO 200
          IF (INDEX(CANS,'N') .EQ. 1) THEN
            WRITE (ISTDOT,150)
 150        FORMAT (' ENTER NEW FILE NAME: ')
            READ (ISTDIN,'(A)') CNAME
            CALL CHRLNB (CNAME,KLN)
            INQUIRE (FILE=CNAME,EXIST=LEXIST)
            GO TO 120
          ELSE
            WRITE (ISTDOT,'('' UNRECOGNIZED RESPONSE'')')
            GO TO 130
          ENDIF
        ENDIF
      ENDIF
C
      IF (CSTT .NE. 'OLD') GO TO 200
C
 160  CONTINUE
      IF (CNAME .EQ. 'CON') LEXIST = .TRUE.                             MLu
      IF (LEXIST .OR. .NOT.LINAC) GO TO 200
C
      KLN = NINDXR(CNAME(1:NNAME),' ')
      WRITE (ISTDOT,170) CNAME(1:KLN)
 170  FORMAT (' OPEN STATUS IS OLD BUT FILE [',A,'] DOES NOT EXIST'//
     + ' ENTER ANOTHER FILE NAME: ')
      CNAME = ' '
      READ (ISTDIN,'(A)') CNAME
      INQUIRE (FILE=CNAME,EXIST=LEXIST)
      GO TO 160
C
C-----------------------------------------------------------------------
C
C        Apparently Microsoft Fortran V4.00 has difficulty in
C        handling STATUS = 'UNKNOWN' when 'UNKNOWN' is stored
C        in a variable name. Same is true with 'SCRATCH'
C
C-----------------------------------------------------------------------
 200  CONTINUE
      IF (CACC .EQ. 'DIRECT') THEN                                      MLu
        IF (CSTT .NE. 'SCRATCH') THEN                                   MLu
        OPEN (UNIT   = IUNIT,                                           MLu
     +        FILE   = CNAME,                                           MLu
     +        ACCESS = CACC,                                            MLu
     +        FORM   = CFRM,                                            MLu
     +        RECL   = NLEN,                                            MLu
     +        STATUS = 'UNKNOWN',                                       MLu
     +        IOSTAT = JSTAT)                                           MLu
        ELSE                                                            MLu
        OPEN (UNIT   = IUNIT,                                           MLu
C    +        FILE   = CNAME,                                           gmu
     +        ACCESS = CACC,                                            MLu
     +        FORM   = CFRM,                                            MLu
     +        RECL   = NLEN,                                            MLu
     +        STATUS = 'SCRATCH',                                       MLlg
     +        IOSTAT = JSTAT)                                           MLu
        ENDIF                                                           MLu
C
      ELSE IF (CSTT .NE. 'SCRATCH') THEN                                MLu
C
C     On UNIX, don't try to connect unit 6
C     to stdin or stdout (already connected!)
C     IF ((IUNIT.EQ.5).OR.(IUNIT.EQ.6)) THEN                            u
C        JSTAT = 0                                                      u
C        IF (CNAME(1:8).EQ.'/dev/tty') GO TO 999                        u
C     ENDIF                                                             u
C
         OPEN (UNIT   = IUNIT,                                          MLu
     +        FILE   = CNAME,                                           MLu
     +        ACCESS = CACC,                                            MLu
     +        FORM   = CFRM,                                            MLu
     +        IOSTAT = JSTAT)                                           MLu
      ELSE                                                              MLu
        OPEN (UNIT   = IUNIT,                                           MLu
C    +        FILE   = CNAME,                                           u
     +        ACCESS = CACC,                                            MLu
     +        FORM   = CFRM,                                            MLu
     +        STATUS = 'SCRATCH',                                       MLlg
     +        IOSTAT = JSTAT)                                           MLu
C        IF (JSTAT.NE.0) THEN                                           u
C           OPEN (UNIT   = IUNIT,                                       u
C    +            FILE   = CNAME,                                       u
C    +            ACCESS = CACC,                                        u
C    +            FORM   = CFRM,                                        u
C    +            IOSTAT = JSTAT)                                       u
C        ENDIF                                                          u
      ENDIF                                                             MLu
C
      IF (JSTAT .GT. 0) THEN                                            MLu
        WRITE (ISTDOT,210) JSTAT,IUNIT                                  MLu
 210    FORMAT (' : ERROR: NUMBER ',I5,' WHILE TRYING TO OPEN UNIT',I4) MLu
        ISTAT = JSTAT                                                   MLu
      ENDIF                                                             MLu
C
      IF (.NOT. LEXIST .AND. .NOT. LDFALT) WRITE (ISTDOT,220) CNAME     MLu
 220  FORMAT (' : WARNING: FILE GENERATED -- ',A)                       MLu
C
C     IF (CMODE .EQ. 'S') CALL ASSIGS (IUNIT,CNAME,IERR)                H
C     IF (CMODE .EQ. 'E') CALL ASSIGX (IUNIT,CNAME,IERR)                H
C     IF (CMODE .EQ. 'N') CALL CASSIG (IUNIT,CNAME,IERR)                H
C
C     IF (IERR .EQ. 0 .AND. LAPND) THEN                                 H
C       CALL WIND (IUNIT)                                               H
C       IF (LDFALT) WRITE (ISTDOT,310) CNAME                            H
C310    FORMAT (' DATA WILL BE APPENDED TO THE DEFAULT FILE: ',A)       H
C     ENDIF                                                             H
C
C     IF (IERR .EQ. 0) THEN                                             H
C       IF (CACC .EQ. 'DIRECT')                                         H
C    +  OPEN (UNIT=IUNIT,ACCESS=CACC,FORM=CFRM,RECL=NLEN,STATUS=CSTT)   H
C       IF (CACC .EQ. 'SEQUENTIAL')                                     H
C    +  OPEN (UNIT=IUNIT,ACCESS=CACC,FORM=CFRM,STATUS=CSTT)             H
C       GO TO 999                                                       H
C     ENDIF                                                             H
C     IF (IERR .NE. 1) GO TO 370                                        H
C
C        FILE NOT FOUND, SHOULD IT HAVE BEEN?
C
C     IF (CSTT .EQ. 'OLD') GO TO 370                                    H
C
C        FILE TO ASSIGN NOT FOUND, MUST CREATE ONE
C
C     IBL = 0                                                           H
C
C       DETERMINE TYPE OF FILE TO CREATE
C
C     IF (CACCS .EQ. 'D') THEN                                          H
C       IF (CTYPE .EQ. 'U') IBL=-1                                      H
C       IF (CTYPE .EQ. 'R') IBL=-2                                      H
C     ENDIF                                                             H
C
C-----------------------------------------------------------------------
C     IGRAN = 0                                                         H
C     IF (IBL .LT. 0) IGRAN = 200                                       H
C
C     LAPND = .FALSE.                                                   H
C     CALL CCREAT (CNAME,IGRAN,0,IBL,IERR)                              H
C     IF (IERR .NE. 0) GO TO 390                                        H
C     CALL CRETYP (CNAME,NBIT,0,IERR)                                   H
C     IF (IERR .NE. 0) GO TO 390                                        H
C
C     IF (CSTAT .EQ. 'S') GO TO 200                                     H
C     WRITE (ISTDOT,320) CNAME                                          H
C320  FORMAT(' : WARNING: FILE GENERATED -- ',A)                        H
C     GO TO 200                                                         H
C330  WRITE (ISTDOT,340) CNAME                                          H
C340  FORMAT(' UNRECOGNIZED PDN ',A)                                    H
C     GO TO 410                                                         H
C
C350  IF (ITYP .EQ. (-2)) THEN                                          H
C        SPOOL UNIT TO A PDN
C     CALL SPLPDN (IUNIT,IUN,1,IERR)                                    H
C     IF (IERR .EQ. 0) GO TO 999                                        H
C     GO TO 370                                                         H
C     ENDIF                                                             H
C
C-----------------------------------------------------------------------
C       SITE DEPENDENT CODE..BYPASS ASSIGNMENT TO PDN 2 (HEC'S OPCOM)
C-----------------------------------------------------------------------
C
C     IF (ITYP .NE. (-1) .OR. IUN .NE. 2) GO TO 360                     H
C     IERR = 6                                                          H
C     GO TO 370                                                         H
C
C-----------------------------------------------------------------------
C360  IF (IUNIT .EQ. IUN .AND. ITYP .EQ. 1) GO TO 999                   H
C     CALL ASIGNI(IUNIT,IUN,ITYP,IERR)                                  H
C     IF (IERR .EQ. 0) THEN                                             H
C     OPEN (UNIT=IUNIT)                                                 H
C     GO TO 999                                                         H
C     ENDIF                                                             H
C
C        RESOURCE PDN IF NOT ASSIGNABLE
C
C     CALL PTYPE (ITYPE)                                                H
C     IF (ITYPE.EQ.0) THEN                                              H
C     CALL RSCPDN (IUNIT,IUN,1,IERR)                                    H
C     ELSE                                                              H
C     CALL RSCPDN (IUNIT,IUN,6,IERR)                                    H
C     ENDIF                                                             H
C     CALL RSCPDN (IUNIT,IUN,2,IERR)                                    H
C     IF (IERR .EQ. 0) THEN                                             H
C     OPEN (UNIT=IUNIT)                                                 H
C     GO TO 999                                                         H
C     ENDIF                                                             H
C        WAIT 2 SECONDS AND TAKE STATUS AGAIN
C     CALL WAITS (2.0)                                                  H
C     CALL RSCPDN (IUNIT,IUN,2,IERR)                                    H
C     IF (IERR .EQ. 0) GO TO 999                                        H
C
C370  WRITE (ISTDOT,380) CKYWRD,CNAME                                   H
C380  FORMAT(' : ERROR:   TRYING TO ASSIGN ',A,' TO ',A)                H
C
C     CALL ASNERR (ISTDOT,IUN,ITYP,IERR)                                H
C
C     GO TO 410                                                         H
C390  WRITE (ISTDOT,400) CNAME                                          H
C400  FORMAT(' : ERROR:   TRYING TO CREATE ',A)                         H
C
C     CALL GENERR (ISTDOT,IERR)                                         H
C
C410  WRITE (ISTDOT,420)                                                H
C420  FORMAT(' DETECTED IN ROUTINE ATTACH')                             H
C     STOP                                                              H
C
 999  RETURN
      END

