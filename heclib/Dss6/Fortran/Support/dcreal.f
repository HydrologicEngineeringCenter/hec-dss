      SUBROUTINE DCREAL ( R, NR, ISKM, BASE, LBASE, ISIZE, IPREC,
     .                    ICMVAL, NCMVAL, KCMVAL, IH, NH, KH, ISTAT )
C
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     LOGICAL LDEBUG                                                    D
      LOGICAL LBASE, LRPEAT, LDELTA, LSIGDT
C
C     INTEGER*3 NELMS                                                   H
      INTEGER*4 NELMS                                                   MLu
C
C     INTEGER*6 IH(KH), ICMVAL(KCMVAL), IBASE                           H
      INTEGER*4 IH(KH), ICMVAL(KCMVAL), IBASE                           MLu
C
      INTEGER IPREC, ISIZE, ISKM, ISTAT, NCMVAL, NH, NR
C
      REAL R(*), BASE
C
      EQUIVALENCE ( RBASE, IBASE )
C
C     COMMON / DCDBUG / LDEBUG, MUNIT                                   D
C
      ISTAT = 0
C
C --- WRITE OUT SUBROUTINE NAME AND VERSION # IF DEBUG OPTION IS ON
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'SUBROUTINE DCREAL (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA COMPRESSION - REAL NUMBERS'              D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- CHECK TO SEE IF NUMBER OF INPUT ELEMENTS IS LESS THAN OR EQUAL TO
C --- ZERO. IF SO, PRINT ERROR MESSAGE AND RETURN.
C
      IF ( NR .LE. 0 ) THEN
         ISTAT = -1301
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'NUMBER OF INPUT ELEMENTS LESS THAN 1 '     D
C           WRITE (MUNIT,*) 'NR: ', NR, ' ISTAT: ', ISTAT               D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- DOUBLE CHECK THAT KCMVAL, THE DIMENSIONAL LIMIT OF ICMVAL, IS
C --- POSITIVE. IF NOT, PRINT ERROR MESSAGE AND RETURN.
C
      IF ( KCMVAL .LE. 0 ) THEN
         ISTAT = -1302
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'KCMVAL NOT POSITIVE  '                     D
C           WRITE (MUNIT,*) 'KCMVAL:  ', KCMVAL, ' ISTAT: ', ISTAT      D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- CHECK TO SEE IF VALID SCHEME IS REQUESTED. IF SCHEME NUMBER IS
C --- NOT VALID, PRINT ERROR MESSAGE AND RETURN TO CALLING PROGRAM.
C
      IF ( ISKM .LE. 0 .OR. ISKM .GE. 6 ) THEN
         ISTAT = -1303
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'INVALID SCHEME NUMBER'                     D
C           WRITE (MUNIT,*) 'ISKM:    ', ISKM, ' ISTAT: ', ISTAT        D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- SAVE SCHEME NUMBER BEFORE DETERMINING WHICH SCHEMES ARE USED.
C
      JSKM = ISKM
C
C --- SCHEME NUMBER IS VALID, SO DETERMINE WHICH COMPRESSION SCHEMES
C --- ARE REQUESTED BY SETTING RESPECTIVE LOGICAL VARIABLES.
C
      LRPEAT = .FALSE.
      LDELTA = .FALSE.
      LSIGDT = .FALSE.
C
      DO 100 I=7,0,-1
C
         IF ( JSKM .GE. 2**I ) THEN
            IF ( I .EQ. 0 ) LRPEAT = .TRUE.
            IF ( I .EQ. 1 ) LDELTA = .TRUE.
            IF ( I .EQ. 2 ) LSIGDT = .TRUE.
            JSKM = JSKM - 2**I
         ENDIF
C
  100 CONTINUE
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) 'LRPEAT: ', LRPEAT                             D
C        WRITE (MUNIT,*) 'LDELTA: ', LDELTA                             D
C        WRITE (MUNIT,*) 'LSIGDT: ', LSIGDT                             D
C     ENDIF                                                             D
C
C --- IF BOTH THE DELTA AND SIGNIFICANT DIGITS SCHEMES HAVE BEEN
C --- REQUESTED, PRINT ERROR MESSAGE AND RETURN. THESE SCHEMES ARE
C --- MUTUALLY EXCLUSIVE.
C
      IF ( (LDELTA) .AND. (LSIGDT) ) THEN
         ISTAT = -1304
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'DELTA AND SIGNIFICANT DIGITS SCHEMES'      D
C           WRITE (MUNIT,*) 'HAVE BOTH BEEN REQUESTED - METHODS'        D
C           WRITE (MUNIT,*) 'ARE MUTUALLY EXCLUSIVE.'                   D
C           WRITE (MUNIT,*) 'ISKM:    ', ISKM, ' ISTAT: ', ISTAT        D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- SINCE A VALID COMPRESSION SCHEME OR SCHEME COMBINATION HAS BEEN
C --- REQUESTED, INITIALIZE THE COMPRESSION HEADER ARRAY BEFORE
C --- STARTING THE ACTUAL DATA COMPRESSION.
C
      NELMS = NR
C
      CALL DHINIT ( BASE, IBASE, NBASE, IH, NH, IPREC, ISIZE, ISKM,
     .              NBITS, NBYTES, NELMS, ISTAT, LBASE, LDELTA, LSIGDT )
C
C --- NOW START DATA COMPRESSION. IF THE REPEAT METHOD IS REQUESTED,
C --- COMPRESS THE DATA USING THIS METHOD.
C
      IF ( LRPEAT ) THEN
C
C        CALL DCREPT ( R, NR, ICMVAL(1), NR2, IH(11), NH, KH, ISTAT)    h
C        CALL DCREPT ( R, NR, ICMVAL(1), NR2, IH(6), NH, KH, ISTAT)     H
         CALL DCREPT ( R, NR, ICMVAL(1), NR2, IH(9),  NH, KH, ISTAT)    MLu
C
C --- CHECK TO SEE IF REPEAT COUNT PROCESSED CORRECTLY (ISTAT = 0).
C --- IF NOT, PRINT ERROR MESSAGE AND RETURN.
C
         IF ( ISTAT .NE. 0 ) THEN
C           IF (LDEBUG) THEN                                            D
C              WRITE (MUNIT,*) 'ERROR OCCURRED WHILE COMPRESSING DATA'  D
C              WRITE (MUNIT,*) 'USING THE REPEAT DATA METHOD'           D
C              WRITE (MUNIT,*) 'ISTAT: ', ISTAT                         D
C           ENDIF                                                       D
            GOTO 900
         ENDIF
C
C --- SINCE REPEAT COUNT PROCESSED CORRECTLY, UPDATE THE BYTE COUNT
C --- IN THE 'IH' HEADER ARRAY.
C
         IBYTES = NCPW*NH
C
         CALL GETHOL ( IH(1), 28, NBYTES )
         CALL GETHOL ( IH(1), 30, NHIGH  )
C
         NBYTES = NHIGH*256 + NBYTES
         NBYTES = IBYTES + NBYTES + 1
C
C
         IF ( NBYTES .GE. 256 ) THEN
C
            NTMP = NBYTES
            NBYTES = MOD ( NTMP, 256 )
            NHIGH = NTMP / 256
C
C
            CALL PUTHOL ( IH(1), 28, NBYTES )
            CALL PUTHOL ( IH(1), 30, NHIGH  )
C
         ELSE
            CALL PUTHOL ( IH(1), 28, NBYTES )
         ENDIF
C
         IEND = IBYTES + 33 + 1
         CALL PUTHOL ( IH(1), IEND, ICHAR('E') )
C
C --- COMPUTE THE NUMBER OF BYTES REQUIRED TO STORE ALL OF THE
C --- COMPRESSION BITS AND THE NUMBER OF BITS THAT WILL BE ACTIVE
C --- IN THE LAST BYTE.
C
         NBITS = MOD(NR,NCPW*8)
         CALL PUTHOL ( IH(1), 29, NBITS )
C
         IF (NBITS .EQ. 0) THEN
            NWORDS = NR/(NCPW*8)
         ELSE
            NWORDS = NR/(NCPW*8) + 1
         ENDIF
C
         NBYTES = NWORDS*NCPW
C
C --- PLACE NUMBER OF INTEGER WORDS FOR THE 'IH' ARRAY INTO 'NH'.
C --- THIS VALUE SHOULD INCLUDE THE COMPRESSION BIT INFO.
C
C        NH = NWORDS + 11                                               h
C        NH = NWORDS + 6                                                H
         NH = NWORDS + 9                                                MLu
C
         IF ( NBYTES .NE. IBYTES ) THEN
            ISTAT = -1305
C           IF (LDEBUG) THEN                                            D
C              WRITE (MUNIT,*) 'COMPUTED NUMBER OF COMPRESSED INTEGER'  D
C              WRITE (MUNIT,*) 'WORDS DOES NOT AGREE WITH ACTUAL NUMBER'D
C              WRITE (MUNIT,*) 'NBYTES: ', NBYTES, ' NBYTES: ', NBYTES  D
C              WRITE (MUNIT,*) 'ISTAT: ', ISTAT                         D
C           ENDIF                                                       D
            GOTO 900
         ENDIF
C
C --- IF THE REPEAT COUNT SCHEME IS REQUESTED BY ITSELF, DETERMINE THE
C --- THE NUMBER OF ELEMENTS IN THE 'ICMVAL' ARRAY AND RETURN TO THE
C --- CALLING PROGRAM.
C
         IF ( .NOT. LDELTA .AND. .NOT. LSIGDT ) THEN
C
            NCMVAL = NR2
C
            GOTO 900
C
         ENDIF
C
      ENDIF
C
C --- DELTA SCHEME COMPRESSION BEGINS HERE. BE SURE TO CHECK IF THE
C --- DATA WAS PREVIOUSLY COMPRESSED BY THE REPEAT METHOD. IF THE
C --- REPEAT SCHEME WAS USED, COMPRESS THE 'ICMVAL' ARRAY OUTPUT FROM
C --- THE REPEAT SCHEME, OTHERWISE COMPRESS THE INPUT ARRAY 'R'.
C
      IF ( LDELTA ) THEN
C
         IBBYTE = 1
C
         IF ( LRPEAT ) THEN
            CALL DCDLTR ( ICMVAL, NR2, IPREC, BASE, LBASE, ICMVAL,
     .                    NCMVAL, KCMVAL, IBBYTE, ISIZE, ISTAT )
         ELSE
            CALL DCDLTR ( R, NR, IPREC, BASE, LBASE, ICMVAL,
     .                    NCMVAL, KCMVAL, IBBYTE, ISIZE, ISTAT )
         ENDIF
C
         IF ( ISTAT .NE. 0 ) THEN
C           IF (LDEBUG) THEN                                            D
C              WRITE (MUNIT,*) 'ERROR OCCURRED WHILE COMPRESSING DATA'  D
C              WRITE (MUNIT,*) 'USING THE DELTA METHOD.'                D
C              WRITE (MUNIT,*) 'LDELTA: ', LDELTA, ' LRPEAT: ', LRPEAT  D
C              WRITE (MUNIT,*) 'ISTAT: ', ISTAT                         D
C           ENDIF                                                       D
            GOTO 900
         ENDIF
C
C --- SINCE 'ISIZE' CAN BE CHANGED BY THE 'DCDLTR' SUBROUTINE, PUT
C --- 'ISIZE' INTO THE DATA COMPRESSION HEADER.
C
         CALL PUTHOL ( IH(1), 12, ISIZE )
C
C --- IF 'BASE' IS NOT SPECIFIED ON INPUT TO 'DCDLTR' SUBROUTINE,
C --- PLACE COMPUTED 'BASE' FROM 'DCDLTR' INTO HEADER ARRAY 'IH'
C
         IF (.NOT. LBASE) THEN
C
            RBASE = BASE
C
C           DO 300 I=1,6                                                H
            DO 300 I=1,4                                                MLu
               CALL GETHOL ( IBASE, I,    IBYTE )
               CALL PUTHOL ( IH(1), I+15, IBYTE )
  300       CONTINUE
C
         ENDIF
C
      ENDIF
C
C --- SIGNIFICANT DIGIT COMPRESSION BEGINS HERE. BE SURE TO CHECK IF
C --- THE DATA WAS PREVIOUSLY COMPRESSED USING THE REPEAT SCHEME. IF
C --- THE REPEAT SCHEME WAS USED, COMPRESS THE 'ICMVAL' ARRAY OUTPUT
C --- FROM THE REPEAT SCHEME, OTHERWISE COMPRESS THE INPUT ARRAY 'R'.
C
      IF ( LSIGDT ) THEN
C
         IBBYTE = 1
C
         IF ( LRPEAT ) THEN
            CALL DCSD3R ( ICMVAL, NR2, ICMVAL, NCMVAL, KCMVAL,
     .                    IBBYTE, ISTAT )
         ELSE
            CALL DCSD3R ( R, NR, ICMVAL, NCMVAL, KCMVAL,
     .                    IBBYTE, ISTAT )
         ENDIF
C
         IF ( ISTAT .NE. 0 ) THEN
C           IF (LDEBUG) THEN                                            D
C              WRITE (MUNIT,*) 'ERROR OCCURRED WHILE COMPRESSING DATA'  D
C              WRITE (MUNIT,*) 'USING THE SIGNIFICANT DIGITS METHOD'    D
C              WRITE (MUNIT,*) 'LSIGDT: ', LSIGDT, ' LRPEAT: ', LRPEAT  D
C              WRITE (MUNIT,*) 'ISTAT: ', ISTAT                         D
C           ENDIF                                                       D
            GOTO 900
         ENDIF
C
      ENDIF
C
C --- RETURN TO CALLING PROGRAM WITH COMPRESSION INFORMATION.
C
  900 RETURN
      END

