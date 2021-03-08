      SUBROUTINE DHINIT (BASE, IBASE, NBASE, IH, NH, IPREC, ISIZE, ISKM,
     .              NBITS, NBYTES, NELMS, ISTAT, LBASE, LDELTA, LSIGDT)
C
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     LOGICAL LDEBUG, LHEAD                                             D
      LOGICAL LBASE, LDELTA, LSIGDT
C
      INTEGER ISKM, ISIZE, IPREC, NH, ISTAT
C
C     INTEGER*3 NELMS                                                   H
      INTEGER*4 NELMS                                                   MLu
C
C     INTEGER*6 IH(*), IBASE                                            H
      INTEGER*4 IH(*), IBASE                                            MLu
C
      REAL BASE
C
C     COMMON / DCDBUG / LDEBUG, MUNIT                                   D
C
C --- INITIALIZE 'ISTAT' TO ZERO BEFORE EXECUTING SUBROUTINE
C
      ISTAT = 0
C
C --- WRITE OUT SUBROUTINE NAME AND VERSION # IF DEBUG OPTION IS ON
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'SUBROUTINE DHINIT (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA COMPRESSION HEADER - INITIALIZATION'     D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
      ISTAT = 0
C
      IVERS = 16
      NBYTES = 8
C
      CALL PUTHOL ( IH, 1, ICHAR('C') )
C
      CALL PUTHOL ( IH, 2, IVERS )
      CALL PUTHOL ( IH, 3, NBYTES )
      CALL PUTHOL ( IH, 4, ISKM )
C
      CALL GETHOL ( NELMS, 1, NTMP )
      CALL PUTHOL ( IH, 5, NTMP )
      CALL GETHOL ( NELMS, 2, NTMP )
      CALL PUTHOL ( IH, 6, NTMP )
      CALL GETHOL ( NELMS, 3, NTMP )
      CALL PUTHOL ( IH, 7, NTMP )
C
      CALL GETHOL ( NELMS, 4, NTMP )                                    MLu
      CALL PUTHOL ( IH, 8, NTMP )                                       MLu
C     CALL PUTHOL ( IH, 8, 0 )                                          H
C
C --- THIS PORTION OF THE DATA COMPRESSION HEADER IS RESERVED FOR THE
C --- DELTA SCHEME.
C
      IVERS = 16
      NBYTES = 13
C
      IF ( LDELTA ) THEN
         IF ( ISIZE .LE. -1 .OR. ISIZE .GE. 3 ) THEN
            ISTAT = -1801
C           IF ( LDEBUG ) THEN                                          D
C              WRITE (MUNIT,*) ' '                                      D
C              WRITE (MUNIT,*) 'UNACCEPTABLE ISIZE FOR THE DELTA SCHEME'D
C              WRITE (MUNIT,*) 'ISIZE MUST EQUAL 1 OR 2 FOR THIS SCHEME'D
C              WRITE (MUNIT,*) 'ISIZE: ', ISIZE, ' ISTAT: ', ISTAT      D
C              WRITE (MUNIT,*) ' '                                      D
C           ENDIF                                                       D
         ENDIF
      ENDIF
C
      CALL PUTHOL ( IH, 9, ICHAR('D') )
      CALL PUTHOL ( IH, 10, IVERS )
      CALL PUTHOL ( IH, 11, NBYTES )
      CALL PUTHOL ( IH, 12, ISIZE )
C
C --- ADD BIAS TO 'IPREC' SO THAT IT IS POSITIVE FOR CALL TO 'PUTHOL'
C
      IPREC = IPREC + 10
      CALL PUTHOL ( IH, 13, IPREC )
      IPREC = IPREC - 10
C
      IF (LBASE) THEN
         LB = 1
      ELSE
         LB = 0
      ENDIF
C
      CALL PUTHOL ( IH, 14, LB )
C
C     NBASE = 6                                                         H
      NBASE = 4                                                         MLu
C
      CALL PUTHOL ( IH, 15, NBASE )
C
      DO 200 I=1,NBASE
         IF (LBASE) THEN
            CALL GETHOL ( IBASE, I,    IBYTE )
            CALL PUTHOL ( IH(1), I+15, IBYTE )
         ELSE
            CALL PUTHOL ( IH(1), I+15, 0 )
         ENDIF
  200 CONTINUE
C
C --- THIS PORTION OF THE DATA COMPRESSION HEADER IS RESERVED FOR THE
C --- SIGNIFICANT DIGITS SCHEME.
C
      IVERS = 16
      NBYTES = 4
C
      IF ( LSIGDT .AND. ISIZE .NE. 2 ) THEN
         ISTAT = -1802
C        IF ( LDEBUG ) THEN                                             D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'UNACCEPTABLE ISIZE FOR THE SIGNIFICANT'    D
C           WRITE (MUNIT,*) 'DIGITS SCHEME. ISIZE MUST EQUAL 2 FOR'     D
C           WRITE (MUNIT,*) 'THIS SCHEME.'                              D
C           WRITE (MUNIT,*) 'ISIZE: ', ISIZE, ' ISTAT: ', ISTAT         D
C           WRITE (MUNIT,*) ' '                                         D
C        ENDIF                                                          D
      ENDIF
C
      CALL PUTHOL ( IH, 22, ICHAR('S') )
      CALL PUTHOL ( IH, 23, IVERS )
      CALL PUTHOL ( IH, 24, NBYTES )
      CALL PUTHOL ( IH, 25, ISIZE )
C
C --- THE LAST PORTION OF THE DATA COMPRESSION HEADER IS RESERVED FOR
C --- THE REPEAT COUNT METHOD OF DATA COMPRESSION. THIS SECTION IS
C --- PLACED LAST BECAUSE ITS LENGTH CAN VARY WITH AMOUNT OF DATA
C --- COMPRESSED.
C
      IVERS = 16
      NBYTES = 7
      NBITS = 0
      NHIGH = 0
C
      CALL PUTHOL ( IH, 26, ICHAR('R') )
      CALL PUTHOL ( IH, 27, IVERS )
      CALL PUTHOL ( IH, 28, NBYTES )
      CALL PUTHOL ( IH, 29, NBITS )
      CALL PUTHOL ( IH, 30, NHIGH )
      CALL PUTHOL ( IH, 31, 0 )
      CALL PUTHOL ( IH, 32, 0 )
C
C --- PLACE AN ASCII 'E' IN THE LAST BYTE OF THE HEADER. THIS MAY BE
C --- REPLACED WITH SOME TYPE OF ERROR CHECKING TOTAL LATER ON.
C
      CALL PUTHOL ( IH, 33, ICHAR('E') )
C
C --- INITIALIZE 'NH' TO THE NUMBER OF BYTES IN THE HEADER.
C
      NH = 33
C
C --- DETERMINE LENGTH OF HEADER ARRAY IN INTEGER WORDS
C
      IREMAN = MOD(NH, NCPW)
C
      IF ( IREMAN .EQ. 0 ) THEN
         NH = NH/NCPW
      ELSE
         NH = NH/NCPW + 1
      ENDIF
C
C --- IF DEBUG OPTION IS TURNED ON, PRINT OUT THE HEADER ARRAY 'IH'
C
C     IF (LDEBUG) THEN                                                  D
C        LHEAD = .TRUE.                                                 D
C        CALL PRTWRD ( IH(1), NH, LHEAD )                               D
C     ENDIF                                                             D
C
C     IF ( LDEBUG ) WRITE (MUNIT,*) ' '                                 D
C
C --- RETURN TO THE CALLING MODULE
C
      RETURN
      END

