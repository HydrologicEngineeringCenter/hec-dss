      SUBROUTINE DCSD3R ( R1, NR1, ICMVAL, NCMVAL, KCMVAL, IBBYTE,
     .                  ISTAT )
C
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     LOGICAL LDEBUG                                                    D
C
      INTEGER NR1, IBBYTE, ISTAT, ITMP
C
C     INTEGER*6 ICMVAL(KCMVAL), I2TMP                                   H
      INTEGER*4 ICMVAL(KCMVAL), I2TMP                                   MLu
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
C        WRITE (MUNIT,*) 'SUBROUTINE DCSD3R (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA COMPRESSION - SIGNIFICANT DIGITS'        D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- IF 'NR1' IS NEGATIVE OR ZERO, DISPLAY ERROR MESSAGE AND RETURN.
C
      IF ( NR1 .LE. 0 ) THEN
         ISTAT = -1501
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'NR1 ARRAY ELEMENTS NOT POSITIVE   '        D
C           WRITE (MUNIT,*) 'NR1:    ', NR1, ' ISTAT: ', ISTAT          D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- IF LDEBUG ACTIVE, PRINT TRACE OF SUBROUTINE INPUT
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'DCSD3R - INPUT ARRAY R1 - NELMS:', NR1        D
C        WRITE (MUNIT,*) ' '                                            D
C        CALL PRTARY ( R1, NR1 )                                        D
C     ENDIF                                                             D
C
C --- IF BEGINNING BYTE ( 'IBBYTE' ) IS LESS THAN OR EQUAL TO ZERO,
C --- PRINT ERROR MESSAGE AND RETURN.
C
      IF (IBBYTE .LE. 0) THEN
         ISTAT = -1502
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
C --- ELEMENTS OF THE R1 ARRAY USING 2 BYTES FOR EACH ELEMENT
C
      NTOTAL = NR1*2
C
      IF ( MOD(NTOTAL,NCPW) .EQ. 0 ) THEN
         NWORDS = NTOTAL/NCPW
      ELSE
         NWORDS = NTOTAL/NCPW + 1
      ENDIF
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'NWORDS:', NWORDS, ' NCPW:', NCPW              D
C     ENDIF                                                             D
C
C --- CHECK TO SEE IF COMPUTED STARTING AND ENDING POSITIONS OF ICMVAL
C --- DO NOT EXCEED THE KCMVAL DIMENSION LIMIT
C
      IENELM = IBGELM + NWORDS - 1
      NCMVAL = IENELM
C
      IF (IBGELM .GT. KCMVAL .OR. IENELM .GT. KCMVAL) THEN
         ISTAT = -1503
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'DIMENSION LIMIT OF KCMVAL EXCEEDED'        D
C           WRITE (MUNIT,*) 'KCMVAL: ', KCMVAL, ' ISTAT:  ', ISTAT      D
C           WRITE (MUNIT,*) 'IBGELM: ', IBGELM, ' IENELM: ', IENELM     D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- CONVERT EACH REAL ARRAY ELEMENT OF ARRAY 'R1' INTO A 2-BYTE
C --- VALUE USING THE THREE SIGNIFICANT DIGITS METHOD OF COMPRESSION.
C --- PLACE PAIRS OF COMPRESSION BYTES INTO THE 'ICMVAL' ARRAY
C --- STARTING AT 'IBBYTE' (REPRESENTED BY 'IBGELM' AND 'ISTBYT').
C
      DO 100 I=1,NR1
C
C --- DETERMINE LOCATION IN 'ICMVAL' WHERE COMPRESSION BYTES
C --- ARE TO BE STORED.
C
         IBYTE = ISTBYT + 2*I - 2
C
C --- GET 2-BYTE COMPRESSED REPRESENTATION OF REAL ARRAY ELEMENT 'R1(I)'
C
         CALL R2SD3 ( R1(I), I2TMP )
C
C --- GET FIRST BYTE OF COMPRESSED REAL AND STORE IT IN 'ICMVAL'.
C
         CALL GETHOL ( I2TMP, 1, ITMP )
         CALL PUTHOL ( ICMVAL(IBGELM), IBYTE, ITMP )
C
C --- GET SECOND BYTE OF COMPRESSED REAL AND STORE IT IN 'ICMVAL'.
C
         CALL GETHOL ( I2TMP, 2, ITMP )
         CALL PUTHOL ( ICMVAL(IBGELM), IBYTE+1, ITMP )
C
  100 CONTINUE
C
C --- DONE COMPRESSING DATA USING THE THREE SIGNIFICANT DIGITS METHOD.
C
  900 RETURN
      END

