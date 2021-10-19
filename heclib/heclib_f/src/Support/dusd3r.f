      SUBROUTINE DUSD3R ( R1, NR1, ICMVAL, NCMVAL, IBBYTE, ISTAT )
C
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     LOGICAL LDEBUG                                                    D
C
      INTEGER NR1, IBBYTE, ISTAT, ITMP
C
C     INTEGER*6 ICMVAL(NCMVAL), I2TMP                                   H
      INTEGER*4 ICMVAL(NCMVAL), I2TMP                                   MLu
C
      REAL R1(*)
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
C        WRITE (MUNIT,*) 'SUBROUTINE DUSD3R (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA UNCOMPRESSION - SIGNIFICANT DIGITS'      D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- IF DEBUG OPTION IS TURNED ON, PRINT TRACE SUBROUTINE ARGUMENTS
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'NR1: ', NR1, '  IBBYTE: ', IBBYTE             D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- CHECK FOR POSITIVE NR1, IF NOT DISPLAY ERROR AND RETURN
C
      IF ( NR1 .LE. 0 ) THEN
         ISTAT = -2301
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'NR1 ARRAY ELEMENTS NOT POSITIVE   '        D
C           WRITE (MUNIT,*) 'NR1:    ', NR1, ' ISTAT: ', ISTAT          D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- IF BEGINNING BYTE ( 'IBBYTE' ) IS LESS THAN OR EQUAL TO ZERO,
C --- PRINT ERROR MESSAGE AND RETURN.
C
      IF (IBBYTE .LE. 0) THEN
         ISTAT = -2302
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'BEGINNING BYTE IS LESS THAN OR EQUAL TO 0' D
C           WRITE (MUNIT,*) 'IBBYTE: ', IBBYTE, ' ISTAT: ', ISTAT       D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- SINCE BEGINNING BYTE ('IBBYTE') IS POSITIVE, CALCULATE LOCATION
C --- IN 'ICMVAL' ARRAY WHERE COMPRESSION DATA IS TO BE STORED.
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
C        WRITE (MUNIT,*) 'IBBYTE:', IBBYTE, ' IREMAN:', IREMAN          D
C        WRITE (MUNIT,*) 'IBGELM:', IBGELM, ' ISTBYT:', ISTBYT          D
C     ENDIF                                                             D
C
C --- FIND OUT HOW MANY INTEGER WORDS ARE REQUIRED TO STORE NR1
C --- ELEMENTS OF THE R1 ARRAY
C
      IF ( MOD(NR1,NCPW) .EQ. 0 ) THEN
         NWORDS = NR1/NCPW
      ELSE
         NWORDS = NR1/NCPW + 1
      ENDIF
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'NWORDS:', NWORDS, ' NCPW:', NCPW              D
C     ENDIF                                                             D
C
C --- CHECK TO SEE IF COMPUTED STARTING AND ENDING POSITIONS OF ICMVAL
C --- DO NOT EXCEED THE NCMVAL DIMENSION LIMIT
C
C     IENELM = IBGELM + NWORDS - 1
C
C     IF (IBGELM .GT. NCMVAL .OR. IENELM .GT. NCMVAL) THEN
C        ISTAT = -2303
C        IF (LDEBUG) THEN
C           WRITE (MUNIT,*) 'DIMENSION LIMIT OF NCMVAL EXCEEDED'
C           WRITE (*,*) 'NCMVAL: ', NCMVAL, ' ISTAT:  ', ISTAT
C           WRITE (*,*) 'IBGELM: ', IBGELM, ' IENELM: ', IENELM
C        ENDIF
C        GOTO 900
C     ENDIF
C
C
C --- BUG REPAIR: MUST UNCOMPRESS THE DATA IN REVERSE ORDER TO AVOID
C --- OVERWRITING COMPRESSION INFORMATION.
C
C     DO 100 I=1,NR1
      DO 100 I=NR1,1,-1
C
         IBYTE = ISTBYT + 2*I - 2
C
C --- GET FIRST BYTE OF COMPRESSED REAL AND STORE IT
C
         CALL GETHOL ( ICMVAL(IBGELM), IBYTE, ITMP )
         CALL PUTHOL ( I2TMP, 1, ITMP )
C
C --- GET SECOND BYTE OF COMPRESSED REAL AND STORE IT
C
         CALL GETHOL ( ICMVAL(IBGELM), IBYTE+1, ITMP )
         CALL PUTHOL ( I2TMP, 2, ITMP )
C
         CALL SD32R ( I2TMP, R1(I) )
C
  100 CONTINUE
C
C --- IF LDEBUG ACTIVE, PRINT TRACE OF SUBROUTINE OUTPUT
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'DUSD3R - OUTPUT ARRAY R1 - NELMS:', NR1       D
C        WRITE (MUNIT,*) ' '                                            D
C        CALL PRTARY ( R1, NR1 )                                        D
C     ENDIF                                                             D
C
C --- DONE COMRESSING DATA USING THE THREE SIGNIFICANT DIGITS METHOD.
C --- RETURN TO THE CALLING PROGRAM.
C
  900 RETURN
      END

