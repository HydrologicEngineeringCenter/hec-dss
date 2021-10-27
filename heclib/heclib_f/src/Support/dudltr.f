      SUBROUTINE DUDLTR ( R1, NR1, IPREC, BASE, LBASE, ICMVAL, NCMVAL,
     .                    IBBYTE, ISIZE, ISTAT )
C
C --- SUBROUTINE USED TO COMPRESS REAL DATA USING THE DELTA OR
C --- DIFFERENCE METHOD OF DATA COMPRESSION.
C
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     LOGICAL LDEBUG                                                    D
      LOGICAL LBASE
C
C     INTEGER*6 ICMVAL(NCMVAL), I2TMP, IEXP                             H
      INTEGER*4 ICMVAL(NCMVAL), I2TMP, IEXP                             MLu
C
      INTEGER NR1, IPREC, IBBYTE, ISIZE, ISTAT
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
C        WRITE (MUNIT,*) 'SUBROUTINE DUDLTR (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA UNCOMPRESSION - THE DELTA SCHEME'        D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- MAKE SURE THAT SPECIFIED 'ISIZE' IS A REASONABLE VALUE
C
      IF (ISIZE .LE. 0 .OR. ISIZE .GE. 7) THEN
         ISTAT = -2001
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'INVALID VALUE SPECIFIED FOR ISIZE !'       D
C           WRITE (MUNIT,*) 'VALID RANGE IS: 1 TO 6 BYTES'              D
C           WRITE (MUNIT,*) 'ISIZE: ', ISIZE, ' ISTAT: ', ISTAT         D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- IF DEBUG OPTION IS SET, PRINT TRACE OF INPUT ARGUMENTS
C
C     IF (LDEBUG) THEN
         IF ( LBASE ) THEN
C           WRITE (MUNIT,*) 'SPECIFIED BASE VALUE:', BASE               D
         ELSE
C           WRITE (MUNIT,*) 'COMPUTED BASE VALUE:', BASE                D
         ENDIF
C     ENDIF
C
C --- CHECK NUMBER OF ELEMENTS IN OUTPUT ARRAY - IF TOO FEW,
C --- PRINT MESSAGE AND RETURN WITH ISTAT = -1
C
      IF (NR1 .LE. 0) THEN
C
         ISTAT = -2002
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'INPUT VALUE NR1 NONPOSITIVE '              D
C           WRITE (MUNIT,*) 'UNCOMPRESSION NOT DONE !!   '              D
C           WRITE (MUNIT,*) '# VALS: ', NR1, '  ISTAT: ', ISTAT         D
C           WRITE (MUNIT,*) ' '                                         D
C        ENDIF                                                          D
         GOTO 900
C
      ENDIF
C
C --- CHECK FOR RESONABLE PRECISION SPECIFIED BY 'IPREC'. 'IPREC'
C --- REPRESENTS THE BASE 10 EXPONENT OF THE PRECISION TO WHICH
C --- REAL NUMBERS ARE RESTORED FROM THEIR COMPRESSED FORMS.
C --- (2 IS PRECISION TO TENS, AND -2 IS PRECISION TO HUNDRETHS)
C
      IF (IPREC .LE. -7 .AND. IPREC .GE. 7) THEN
         ISTAT = -2003
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'INVALID NUMBER SPECIFIED FOR INPUT '       D
C           WRITE (MUNIT,*) 'PRECISION OF THE DELTA SCHEME'             D
C           WRITE (MUNIT,*) 'VALID NUMBER OF DIGITS ARE: -6 TO 6'       D
C           WRITE (MUNIT,*) 'IPREC: ', IPREC, ' ISTAT: ', ISTAT         D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- USING IBBYTE (BEGINNING BYTE), CALCULATE LOCATION IN ICMVAL
C --- WHERE COMPRESSION DATA LIVES. IF IBBYTE IS LESS THAN OR EQUAL
C --- TO ZERO, PRINT ERROR MESSAGE AND RETURN.
C
      IF (IBBYTE .LE. 0) THEN
         ISTAT = -2004
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'BEGINNING BYTE IS LESS THAN OR EQUAL TO 0' D
C           WRITE (MUNIT,*) 'IBBYTE: ', IBBYTE, ' ISTAT: ', ISTAT       D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C     IREMAN = MOD(IBBYTE,NCPW)
C
C     IF ( IREMAN .EQ. 0 ) THEN
C        ISTBYT = NCPW
C        IBGELM = IBBYTE/NCPW
C     ELSE
C        ISTBYT = IREMAN
C        IBGELM = IBBYTE/NCPW + 1
C     ENDIF
C
C     IF (LDEBUG) WRITE (*,*) IBBYTE, IREMAN, IBGELM, ISTBYT
C
C --- LOOP THRU NR1 ELEMENTS IN THE ICMVAL ARRAY, CONVERTING EACH
C --- COMPRESSED VALUE BACK TO ITS ORIGINAL VALUE. THE INPUT PRECISION,
C --- THE INPUT BASE VALUE, AND THE INPUT COMPRESSION SIZE WILL ALL BE
C --- USED TO ACCOMPLISH THIS CONVERSION. NOTE THAT VALUES COMPRESSED
C --- BY THIS SCHEME WILL ROUND VALUES TO THE PRECISION SPECIFIED.
C
      DO 400 I=NR1,1,-1
C
         I2TMP = 0
C
         DO 300 J=1,ISIZE
C
C           K1 = NCPW - J + 1                                           H
            K1 = J                                                      MLu
C
            K2 = I*ISIZE + J - ISIZE
C
            CALL GETHOL ( ICMVAL(1), K2, ITMP )
            CALL PUTHOL ( I2TMP, K1, ITMP )
C
  300    CONTINUE
C
         REXP = 10.**(REAL(IPREC))
         R1(I) = BASE + I2TMP*REXP
C
         IEXP = 8*ISIZE
         IEXP = 2**IEXP - 1
         IF ( I2TMP .EQ. IEXP ) R1(I) = -901.
C
  400 CONTINUE
C
C --- IF DEBUG IS ACTIVE, PRINT TRACE OF UNCOMPRESSED OUTPUT ARRAY 'R1'
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'DUDLTR - OUTPUT ARRAY R1 - NELMS:', NR1       D
C        WRITE (MUNIT,*) ' '                                            D
C        CALL PRTARY ( R1, NR1 )                                        D
C     ENDIF                                                             D
C
  900 RETURN
      END

