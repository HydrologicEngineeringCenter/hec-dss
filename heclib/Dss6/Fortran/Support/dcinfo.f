      SUBROUTINE DCINFO (IH, NH, IBGELM, NELMS, ISTWRD, NWORDS, IBBYTE)
C
      PARAMETER ( NCHARS = 5 )
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     COMMON / DCDBUG / LDEBUG, MUNIT                                   D
C     LOGICAL LDEBUG                                                    D
C
      LOGICAL LRPEAT, LDELTA, LSIGDT
C
      INTEGER IH(*), ISTBYT(NCHARS)
      INTEGER NH, IBGELM, NWORDS, IBBYTE
C
C     INTEGER*3 NELMS                                                   H
      INTEGER*4 NELMS                                                   MLu
C
      CHARACTER*1 CVAR, CHEAD(5)
C
C --- TURN ON THE DEBUG OPTION IF ISTAT IS SET TO -999,
C --- THEN RESET ISTAT TO 0
C
C
      ISTAT = 0
C
C --- CHECK TO SEE IF NUMBER OF INPUT ELEMENTS IS LESS THAN OR EQUAL
C --- TO ZERO. IF SO, PRINT ERROR MESSAGE AND RETURN.
C
      IF ( NELMS .LE. 0 ) THEN
         ISTAT = -1201
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'NUMBER OF INPUT ELEMENTS LESS THAN 1 '     D
C           WRITE (MUNIT,*) 'NELMS:    ', NELMS, ' ISTAT: ', ISTAT      D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- CHECK TO SEE IF THE HEADER HAS BEEN PASSED. IF NH IS LESS THAN
C --- OR EQUAL TO ZERO, PRINT ERROR MESSAGE AND RETURN.
C
      IF ( NH .LE. 0 ) THEN
         ISTAT = -1202
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'NUMBER OF ELEMENTS IN HEADER IS LESS '     D
C           WRITE (MUNIT,*) 'THAN OR EQUAL TO ZERO.'                    D
C           WRITE (MUNIT,*) 'NH: ', NH, ' ISTAT: ', ISTAT               D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- IF SPECIFIED BEGINNING ELEMENT IS LESS THAN OR EQUAL TO ZERO,
C --- PRINT ERROR MESSAGE AND RETURN TO CALLING PROGRAM.
C
      IF ( IBGELM .LE. 0 ) THEN
         ISTAT = -1203
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'NUMBER OF INPUT ELEMENTS LESS THAN 1 '     D
C           WRITE (MUNIT,*) 'NELMS:    ', NELMS, ' ISTAT: ', ISTAT      D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- GET THE SECOND BYTE OF THE COMPRESSION HEADER AND VERIFY
C --- THAT THE COMPRESSION SCHEME VERSION NUMBER IS VALID.
C
      CALL GETHOL ( IH, 2, IVERS )
C
      IF ( IVERS .NE. 16 ) THEN
         ISTAT = -1204
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'INVALID DATA COMPRESSION VERSION'          D
C           WRITE (MUNIT,*) 'IVERS:  ', IVERS, ' ISTAT: ', ISTAT        D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- INITIALIZE THE CHEAD ARRAY WHICH REPRESENTS ASCII CHARACTERS
C --- FOUND IN THE DATA COMPRESSION HEADER.
C
      CHEAD(1) = 'C'
      CHEAD(2) = 'D'
      CHEAD(3) = 'S'
      CHEAD(4) = 'R'
      CHEAD(5) = 'E'
C
C --- CHECK THE FIRST BYTE OF EACH SECTION OF THE COMPRESSION HEADER
C --- FOR THE CORRECT ASCII CHARACTER. IF WRONG CHARACTER IS FOUND,
C --- PRINT ERROR MESSAGE AND RETURN TO CALLING PROGRAM.
C
      ISTBYT(1) = 1
C
      DO 100 I=1,NCHARS
C
      CALL GETHOL ( IH, ISTBYT(I), NBYTES )
      CVAR(1:1) = CHAR( NBYTES )
C
      IF ( CVAR(1:1) .NE. CHEAD(I) ) THEN
         ISTAT = -1205
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'INVALID DATA COMPRESSION HEADER'           D
C           WRITE (MUNIT,*) 'CVAR: ', CVAR, ' ISTAT: ', ISTAT           D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
      IBYT = ISTBYT(I) + 2
      CALL GETHOL ( IH, IBYT, NBYTES )
      ISTBYT(I+1) = ISTBYT(I) + NBYTES
C
  100 CONTINUE
C
C --- HEADER IS INTACT, SO DETERMINE WHICH COMPRESSION SCHEMES ARE USED
C
      LRPEAT = .FALSE.
      LDELTA = .FALSE.
      LSIGDT = .FALSE.
C
      CALL GETHOL ( IH, 4, ISKM )
      JSKM = ISKM
C
      DO 200 I=7,0,-1
         IBIT = JSKM/2**I
         IF (IBIT .NE. 0) THEN
            JSKM = MOD (JSKM, 2**I)
            IF ( JSKM .EQ. 0 ) LRPEAT = .TRUE.
            IF ( JSKM .EQ. 1 ) LDELTA = .TRUE.
            IF ( JSKM .EQ. 2 ) LSIGDT = .TRUE.
         ENDIF
  200 CONTINUE
C
C --- IF INFORMATION ON THE REPEAT SCHEME IS REQUESTED, THE ENTIRE
C --- ARRAY MUST BE UNCOMPRESSED BEFORE DETERMINING LOCATION OF IBGELM.
C --- THEREFORE PRINT MESSAGE THAT THIS IS NOT IMPLEMENTED.
C
      IF ( LRPEAT ) THEN
         ISTAT = -1206
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'DCINFO IS NOT IMPLEMENTED FOR '            D
C           WRITE (MUNIT,*) 'THE REPEAT COUNT SCHEME.      '            D
C           WRITE (MUNIT,*) 'ISKM: ', ISKM, ' ISTAT: ', ISTAT           D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- IF REQUESTING INFORMATION ON AN INVALID COMPRESSION TECHNIQUE,
C --- PRINT ERROR MESSAGE AND RETURN TO THE CALLING PROGRAM.
C
      IF ( .NOT. LDELTA .AND. .NOT. LSIGDT ) THEN
         ISTAT = -1207
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'INVALID DATA COMPRESSION METHOD'           D
C           WRITE (MUNIT,*) 'ISKM: ', ISKM, ' ISTAT: ', ISTAT           D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- GET ISIZE FOR THE DATA COMPRESSION SCHEME USED
C
      IF (LDELTA) THEN
         CALL GETHOL ( IH, ISTBYT(2)+3, ISIZE )
      ELSEIF (LSIGDT) THEN
         CALL GETHOL ( IH, ISTBYT(3)+3, ISIZE )
      ENDIF
C
C --- USING ISIZE AND IBGELM, CALCULATE THE STARTING INTEGER WORD
C --- WHERE COMPRESSION INFORMATION LIVES.
C
      IBBYTE = IBGELM*ISIZE - ISIZE + 1
C
      IREMAN = MOD(IBBYTE,NCPW)
C
      IF ( IREMAN .EQ. 0 ) THEN
         ISTWRD = IBBYTE/NCPW
      ELSE
         ISTWRD = IBBYTE/NCPW + 1
      ENDIF
C
C --- NOW CALCULATE THE NUMBER OF INTEGER WORDS REQUIRED TO CONTAIN
C --- NELMS OF COMPRESSED DATA FOR THE DELTA OR SIGNIFICANT DIGIT
C --- SCHEMES
C
      NBYTES = NELMS*ISIZE
C
      IENBYT = IBBYTE + NBYTES - 1
C
      IREMAN = MOD(IENBYT,NCPW)
C
      IF ( IREMAN .EQ. 0 ) THEN
         IENWRD = IENBYT/NCPW
      ELSE
         IENWRD = IENBYT/NCPW + 1
      ENDIF
C
      NWORDS = IENWRD - ISTWRD + 1
C
C --- RETURN TO CALLING PROGRAM WITH LOCATION AND QUANTITY OF BYTES USED
C --- TO STORE THE COMPRESSED DATA.
C
  900 RETURN
      END

