      SUBROUTINE DUREAL ( R, KR, IBGELM, NELMS, ICMVAL, NCMVAL, KCMVAL,
     .                    IH, NH, ISTAT )
C
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     LOGICAL LDEBUG                                                    D
      LOGICAL LRPEAT, LDELTA, LSIGDT, LBASE
C
      INTEGER NH, IBGELM, IBBYTE, NR1
C
C     INTEGER*3 NELMS                                                   H
      INTEGER*4 NELMS                                                   MLu
C
C     INTEGER*6 IH(NH), ICMVAL(KCMVAL)                                  H
      INTEGER*4 IH(NH), ICMVAL(KCMVAL)                                  MLu
C
      REAL R(KR)
C
C     COMMON / DCDBUG / LDEBUG, MUNIT                                   D
C
      EQUIVALENCE ( BASE, IBASE )
C
      ISTAT = 0
C
C --- WRITE OUT SUBROUTINE NAME AND VERSION # IF DEBUG OPTION IS ON
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'SUBROUTINE DUREAL (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA UNCOMPRESSION - REAL NUMBERS'            D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- IF SPECIFIED BEGINNING ELEMENT IS LESS THAN OR EQUAL TO ZERO,
C --- PRINT ERROR MESSAGE AND RETURN TO CALLING PROGRAM.
C
      IF ( IBGELM .LE. 0 ) THEN
         ISTAT = -2101
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'IBGELM IS LESS THAN OR EQUAL TO ZERO'      D
C           WRITE (MUNIT,*) 'IBGELM: ', IBGELM, ' ISTAT: ', ISTAT       D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- CALL SUBROUTINE 'DHINFO' TO GET VALUES FOR 'ISIZE', 'NELMS',
C --- 'LRPEAT', 'LDELTA', AND 'LSIGDT'
C
      CALL DHINFO ( IH, NH, ISKM, NR2, BASE, LBASE, NELMS, ISIZE,
     .              IPREC, LRPEAT, LDELTA, LSIGDT, ISTAT )
C
      IF ( ISTAT .NE. 0 ) THEN
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'ERROR OCCCURED IN SUBROUTINE DHINFO'       D
C           WRITE (MUNIT,*) 'ISTAT: ', ISTAT                            D
C           WRITE (MUNIT,*) ' '                                         D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- USING ISIZE AND IBGELM, CALCULATE THE STARTING INTEGER WORD
C --- WHERE COMPRESSION INFORMATION LIVES.
C
      IBBYTE = IBGELM*ISIZE - ISIZE + 1
C
C     IREMAN = MOD(IBBYTE,NCPW)
C
C     IF ( IREMAN .EQ. 0 ) THEN
C        ISTWRD = IBBYTE/NCPW
C     ELSE
C        ISTWRD = IBBYTE/NCPW + 1
C     ENDIF
C
C --- NOW CALCULATE THE NUMBER OF INTEGER WORDS REQUIRED TO CONTAIN
C --- NELMS OF COMPRESSED DATA FOR THE DELTA OR SIGNIFICANT DIGIT
C --- SCHEMES
C
      NBYTES = NELMS*ISIZE
C
C     IENBYT = IBBYTE + NBYTES - 1
C
C     IREMAN = MOD(IENBYT,NCPW)
C
C     IF ( IREMAN .EQ. 0 ) THEN
C        IENWRD = IENBYT/NCPW
C     ELSE
C        IENWRD = IENBYT/NCPW + 1
C     ENDIF
C
C --- NOW UNCOMRESS THE DATA USING THE DELTA SCHEME IF IT IS REQUESTED.
C --- THE DELTA OR SIGNIFICANT DIGITS SCHEME MUST BE APPLIED BEFORE
C --- THE REPEAT SCHEME IN ORDER CORRECTLY UNPACK THE DATA.
C
      NR1 = NELMS
C
      IF ( LDELTA ) THEN
C
         IF ( LRPEAT ) THEN
            CALL DUDLTR ( ICMVAL, NR2, IPREC, BASE, LBASE, ICMVAL,
     .                    NCMVAL, IBBYTE, ISIZE, ISTAT )
         ELSE
            CALL DUDLTR ( R, NR1, IPREC, BASE, LBASE, ICMVAL,
     .                    NCMVAL, IBBYTE, ISIZE, ISTAT )
         ENDIF
C
         IF ( ISTAT .NE. 0 ) THEN
C           IF (LDEBUG) THEN                                            D
C              WRITE (MUNIT,*) 'ERROR OCCURRED WHILE UNCOMPRESSING DATA'D
C              WRITE (MUNIT,*) 'USING THE DELTA METHOD'                 D
C              WRITE (MUNIT,*) 'ISTAT: ', ISTAT                         D
C           ENDIF                                                       D
            GOTO 900
         ENDIF
C
         IF ( .NOT. LRPEAT ) GOTO 900
C
      ENDIF
C
C --- NEXT, UNCOMPRESS THE DATA USING THE SIGNIFICANT DIGITS METHOD OF
C --- DATA COMPRESSION IF THIS SCHEME IS REQUESTED.
C
      IF ( LSIGDT ) THEN
C
         IF ( LRPEAT ) THEN
            CALL DUSD3R ( ICMVAL, NR2, ICMVAL, NCMVAL, IBBYTE, ISTAT )
         ELSE
            CALL DUSD3R ( R, NR1, ICMVAL, NCMVAL, IBBYTE, ISTAT )
         ENDIF
C
         IF ( ISTAT .NE. 0 ) THEN
C           IF (LDEBUG) THEN                                            D
C              WRITE (MUNIT,*) 'ERROR OCCURRED WHILE UNCOMPRESSING DATA'D
C              WRITE (MUNIT,*) 'USING THE SIGNIFICANT DIGITS METHOD'    D
C              WRITE (MUNIT,*) 'ISTAT: ', ISTAT                         D
C           ENDIF                                                       D
            GOTO 900
         ENDIF
C
         IF ( .NOT. LRPEAT ) GOTO 900
C
      ENDIF
C
C --- UNCOMPRESS THE DATA USING THE REPEAT COUNT SCHEME, IF REQUESTED
C
      IF ( LRPEAT ) THEN
C
C --- NEXT, MOVE THE REPEAT COUNT INFORMATION STORED IN THE 'IH' HEADER
C --- ARRAY TO THE APPROPRIATE LOCATION IN THE 'ICMVAL' ARRAY.
C
        CALL GETHOL ( IH(1), 28, NBYTES )
        CALL GETHOL ( IH(1), 29, NBITS )
        CALL GETHOL ( IH(1), 30, NHIGH  )
C
        NBYTES = NHIGH*256 + NBYTES
C
C       WRITE (*,*) 'NBYTES: ', NBYTES, ' NBITS: ', NBITS
C
        NBYTES = NBYTES - 4
C
C       NR = 8*NBYTES - ((NCPW*8)-NBITS)
C
        IF ( MOD(NBYTES,NCPW) .EQ. 0 ) THEN
           NCMVAL = NBYTES/NCPW
        ELSE
           NCMVAL = NBYTES/NCPW + 1
        ENDIF
C
C --- NEXT, UNCOMPRESS THE DATA USING THE REPEAT SCHEME.
C
C        CALL DUREPT ( R, NR1, ICMVAL(1), NR2, IH(11), NH, ISTAT )      h
C        CALL DUREPT ( R, NR1, ICMVAL(1), NR2, IH(6), NH, ISTAT )       H
         CALL DUREPT ( R, NR1, ICMVAL(1), NR2, IH(9), NH, ISTAT )       MLu
C
         IF ( ISTAT .NE. 0 ) THEN
C           IF (LDEBUG) THEN                                            D
C              WRITE (MUNIT,*) 'SUBROUTINE DUREAL:'                     D
C              WRITE (MUNIT,*) 'ERROR OCCURRED WHILE UNCOMPRESSING DATA'D
C              WRITE (MUNIT,*) 'USING THE REPEAT METHOD SCHEME.'        D
C              WRITE (MUNIT,*) 'ISTAT: ', ISTAT                         D
C           ENDIF                                                       D
            GOTO 900
         ENDIF
C
      ENDIF
C
C --- RETURN TO CALLING PROGRAM WITH UNCOMPRESSED DATA OR WITH ERROR
C --- MESSAGES DESCRIBING ANY PROBLEMS
C
  900 RETURN
      END

