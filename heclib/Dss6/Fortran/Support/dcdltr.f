      SUBROUTINE DCDLTR ( R1, NR1, IPREC, BASE, LBASE, ICMVAL, NCMVAL,
     .                    KCMVAL, IBBYTE, ISIZE, ISTAT )
C
C --- SUBROUTINE USED TO COMPRESS REAL DATA USING THE DELTA METHOD OF
C --- DATA COMPRESSION
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     LOGICAL LDEBUG, LHEAD                                             D
      LOGICAL LBASE, LINTLZ
C
C     INTEGER*6 ICMVAL(KCMVAL), IDIFF, I2TMP                            H
      INTEGER*4 ICMVAL(KCMVAL), IDIFF, I2TMP                            MLu
C
      INTEGER NR1, IPREC, IBBYTE, ISIZE, ISTAT, ITMP
C
      REAL R1(*), BASE
C
C     COMMON / DCDBUG / LDEBUG, MUNIT                                   D
C
C --- INITIALIZE 'ISTAT' TO ZERO BEFORE EXECUTING SUBROUTINE
C
      ISTAT = 0
C
C --- WRITE OUT SUBROUTINE NAME AND VERSION # IF 'LDEBUG' IS ACTIVE
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'SUBROUTINE DCDLTR (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA COMPRESSION - THE DELTA SCHEME'          D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- IF LDEBUG ACTIVE, PRINT TRACE OF SUBROUTINE INPUT
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'DCDLTR - INPUT ARRAY R1 - NELMS:', NR1        D
C        WRITE (MUNIT,*) ' '                                            D
C        CALL PRTARY ( R1, NR1 )                                        D
C     ENDIF                                                             D
C
C --- CHECK INPUT ARRAY FOR TOO MANY (OR TOO FEW) INPUT VALUES
C
      IF (NR1 .GT. KCMVAL) THEN
         ISTAT = -1101
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'TOO MANY VALUES TO COMPRESS !!'            D
C           WRITE (MUNIT,*)'#VALS: ',NR1,',  ISTAT: ',ISTAT             D
C           WRITE (MUNIT,'(1X,''KCMVAL: '',I5)') KCMVAL                 D
C           WRITE (MUNIT,*) ' '                                         D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
      IF (NR1 .LE. 0) THEN
         ISTAT = -1102
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'TOO FEW VALUES TO COMPRESS !!'             D
C           WRITE (MUNIT,*)'#VALS: ',NR1,',  ISTAT: ',ISTAT             D
C           WRITE (MUNIT,*) ' '                                         D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- CHECK FOR RESONABLE PRECISION SPECIFIED ON INPUT. 'IPREC' SHOULD
C --- CONTAIN AN INTEGER REPRESENTATION OF BASE 10 EXPONENT FOR
C --- REQUIRED PRECISION (2 IS PRECISION TO TENS, AND -2 IS PRECISION
C --- TO HUNDRETHS). IF PRECISION IS REASONABLE, DETERMINE THE SCALING
C --- FACTOR 'REXP' BASED ON THE SPECIFIED INPUT PRECISION, 'IPREC'.
C
      IF (IPREC .GE. -6 .AND. IPREC .LE. 6) THEN
         REXP = 10.**(REAL(-IPREC))
      ELSE
         ISTAT = -1103
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'INVALID NUMBER OF INPUT SIGNIFICANT DIGITS'D
C           WRITE (MUNIT,*) 'VALID NUMBER OF DIGITS ARE: -6 TO 6'       D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- CONVERT THE INPUT ARRAY 'R1(I)' TO THE SPECIFIED INPUT PRECISION
C --- AND THEN DETERMINE THE MAXIMUM AND MINIMUM VALUES OF THE ARRAY.
C --- BYPASS COMPUTATIONS FOR ANY MISSING VALUES THAT ARE FOUND.
C
      LINTLZ = .TRUE.
C
      DO 200 I=1,NR1
C
         IF ( R1(I) .GE. 1.0E+12 .OR. R1(I) .LE. -1.0E+12 ) THEN
         ISTAT = -1108
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'DATA ELEMENT EXCEEDS DATA VALUE RANGE:'    D
C           WRITE (MUNIT,*) '-1.0 * 10**12 TO +1.0 * 10**12'            D
C           WRITE (MUNIT,*) 'DELTA SCHEME - COMPRESSION ABORTED'        D
C           WRITE (MUNIT,*) 'ELEMENT # ',I, '  VALUE: ', R1(I)          D
C           WRITE (MUNIT,*) ' '                                         D
C        ENDIF                                                          D
         GOTO 900
         ENDIF
C
         IF (R1(I) .EQ. -901.) GOTO 200
         IF (R1(I) .EQ. -902.) GOTO 200
C
         RTMP = ANINT(R1(I)*REXP)/REXP
C
         IF ( LINTLZ ) THEN
            RMAX = RTMP
            RMIN = RTMP
            LINTLZ = .FALSE.
         ELSE
            IF (RTMP .GT. RMAX) RMAX = RTMP
            IF (RTMP .LT. RMIN) RMIN = RTMP
         ENDIF
C
  200 CONTINUE
C
C --- IF ARRAY 'R1(I)' CONSISTS ENTIRELY OF MISSING VALUES, THEN
C --- SET 'RMAX' AND 'RMIN' TO ZERO
C
      IF ( LINTLZ ) THEN
         RMAX = 0.
         RMIN = 0.
      ENDIF
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'ARRAY MAX: ', RMAX, '  ARRAY MIN: ', RMIN     D
C     ENDIF                                                             D
C
C --- IF A BASE VALUE IS SPECIFIED ON INPUT, USE IT AS THE MINIMUM
C --- OTHERWISE, USE 'RMIN' AS THE BASE VALUE
C
      IF (LBASE) THEN
         RMIN = ANINT(BASE*REXP)/REXP
      ELSE
         BASE = RMIN
      ENDIF
C
C
C --- PRINT OUT BASE VALUE DETERMINED BY COMPUTATION OR INPUT VALUE
C
C     IF (LDEBUG) THEN                                                  D
C
C        IF (LBASE) THEN                                                D
C           WRITE (MUNIT,*) 'SPECIFIED BASE: ', BASE                    D
C        ELSE                                                           D
C           WRITE (MUNIT,*) 'COMPUTED BASE: ', BASE                     D
C        ENDIF                                                          D
C
C     ENDIF                                                             D
C
C --- TAKE THE DIFFERENCE BETWEEN THE MAXIMUM AND MINIMUM VALUES TO
C --- DETERMINE THE NUMBER OF BYTES REQUIRED TO STORE EACH ELEMENT
C
      RDIFF = RMAX-RMIN
      RTMP  = RDIFF*REXP
C
C --- IF DIFFERENCE REQUIRES MORE THAN TWO BYTES OF STORAGE (65535),
C --- SKIP COMPRESSION AND RETURN WITH STATUS FLAG SET TO -1
C
      IF (RTMP .GE. 65535. ) THEN
         ISTAT = -1104
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'COMPRESSION NOT DONE - DIFFERENCE BETWEEN' D
C           WRITE (MUNIT,*) '   ARRAY MAXIMUM AND MIMIUM TOO GREAT    ' D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- CONVERT DIFFERENCE BETWEEN MAX AND MIN TO INTEGER DIFFERENCE
C
      IDIFF = INT( RTMP + 1. )                                         
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'IDIFF: ', IDIFF, ' RDIFF: ', RDIFF            D
C        WRITE (MUNIT,*) 'IPREC: ', IPREC, ' REXP:  ', REXP             D
C     ENDIF                                                             D
C
      IF (IDIFF .LE. -1) THEN
         ISTAT = -1105
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'ERROR IN DETERMINING DIFFERENCE BETWEEN  ' D
C           WRITE (MUNIT,*) 'MAXIMUM AND MINIMUM - NO COMPRESSION DONE' D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
      ISAV = ISIZE
C
      IF (IDIFF .GE.     0 .AND. IDIFF .LE.      254) ISIZE = 1
      IF (IDIFF .GE.   255 .AND. IDIFF .LE.    65535) ISIZE = 2
C     IF (IDIFF .GE. 65535 .AND. IDIFF .LE. 16777215) ISIZE = 3
C
C --- USE THE SPECIFIED INPUT VALUE FOR 'ISIZE' IF IT EXCEEDS THE
C --- COMPUTED VALUE FOR 'ISIZE'.
C
C     IF ( ISAV .EQ. 1 .OR. ISAV .EQ. 2 .OR. ISAV .EQ. 3 ) THEN
      IF ( ISAV .EQ. 1 .OR. ISAV .EQ. 2 ) THEN
         IF ( ISAV .GE. ISIZE ) ISIZE = ISAV
      ENDIF
C
C --- IF 'ISIZE' IS NOT EQUAL TO 1 OR 2, ABORT COMPRESSION !
C
      IF ( ISIZE .NE. 1 .AND. ISIZE .NE. 2 ) THEN
         ISTAT = -1107
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'ERROR IN DETERMINING NUMBER OF BYTES USED' D
C           WRITE (MUNIT,*) 'TO STORE EACH ELEMENT OF COMPRESSED DATA'  D
C           WRITE (MUNIT,*) 'ISIZE: ', ISIZE, '  ISTAT: ', ISTAT        D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- USING IBBYTE (BEGINNING BYTE), CALCULATE LOCATION IN ICMVAL
C --- WHERE COMPRESSION DATA IS SUPPOSED TO LIVE. IF IBBYTE IS
C --- LESS THAN OR EQUAL TO ZERO, ASSUME BEGINNING BYTE = 1.
C
      IF (IBBYTE .LE. 1) IBBYTE = 1
C
      IREMAN = MOD(IBBYTE,NCPW)
C
      IF ( IREMAN .EQ. 0 ) THEN
         ISTBYT = NCPW
         IBGELM = IBBYTE/NCPW
      ELSE
         ISTBYT = IREMAN
         IBGELM = IBBYTE/NCPW + 1
      ENDIF
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'IBBYTE: ', IBBYTE, ' ISIZE:  ', ISIZE         D
C        WRITE (MUNIT,*) 'IBGELM: ', IBGELM, ' ISTBYT: ', ISTBYT        D
C     ENDIF                                                             D
C
C --- FIND OUT HOW MANY INTEGER WORDS ARE REQUIRED TO STORE NR1
C --- ELEMENTS OF THE R1 ARRAY
C
      NTOTAL = NR1*ISIZE
C
      IREMAN = MOD(NTOTAL,NCPW)
C
      IF ( IREMAN .EQ. 0 ) THEN
         NWORDS = NTOTAL/NCPW
      ELSE
         NWORDS = NTOTAL/NCPW + 1
      ENDIF
C
      NCMVAL = NWORDS
C
C --- CHECK TO SEE IF COMPUTED STARTING AND ENDING POSITIONS OF ICMVAL
C --- DO NOT EXCEED THE KCMVAL DIMENSION LIMIT
C
      IENELM = IBGELM + NWORDS - 1
C
      IF (IBGELM .GT. KCMVAL .OR. IENELM .GT. KCMVAL) THEN
         ISTAT = -1106
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'DIMENSION LIMIT OF KCMVAL EXCEEDED'        D
C           WRITE (MUNIT,*) 'KCMVAL: ', KCMVAL, ' ISTAT:  ', ISTAT      D
C           WRITE (MUNIT,*) 'IBGELM: ', IBGELM, ' IENELM: ', IENELM     D
C           WRITE (MUNIT,*) ' '                                         D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- KNOWING THE MINIMUM VALUE AND # BYTES OF PRECISION FOR EACH
C --- ELEMENT, COMPUTE THE COMPRESSED VERSION OF EACH REAL ELEMENT
C --- AND STORE IN ICMVAL STARTING AT ELEMENT IBBYTE
C
      DO 400 I=1,NR1
C
         IF ((R1(I).NE.-901.).AND.(R1(I).NE.-902.)) THEN
            RTMP = ANINT(R1(I)*REXP)/REXP
            I2TMP = ANINT((RTMP-RMIN)*REXP)
         ENDIF
C
         RSAV = R1(I)
C
         DO 300 J=1,ISIZE
C
C           K1 = NCPW - J + 1                                           H
            K1 = J                                                      MLu
C
            K2 = I*ISIZE + J - ISIZE
C
            IF ((RSAV.NE.-901.).AND.(RSAV.NE.-902.)) THEN
               CALL GETHOL ( I2TMP, K1, ITMP )
               CALL PUTHOL ( ICMVAL(1), K2, ITMP )
            ELSE
C              CALL PUTHOL ( ICMVAL(1), K2, +255 )                      H
               CALL PUTHOL ( ICMVAL(1), K2,   -1 )                      MLu
            ENDIF
C
  300    CONTINUE
C
  400 CONTINUE
C
C --- IF LDEBUG ACTIVE, PRINT TRACE OF 'ICMVAL' OUTPUT ARRAY
C
C     IF (LDEBUG) THEN                                                  D
C
C        LHEAD = .TRUE.                                                 D
C
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'DCDLTR - ICMVAL OUTPUT ARRAY - NELMS:', NCMVALD
C
C        CALL PRTWRD ( ICMVAL(1), NCMVAL, LHEAD )                       D
C
C        WRITE (MUNIT,*) ' '                                            D
C
C     ENDIF                                                             D
C
  900 RETURN
      END

