      SUBROUTINE DHINFO ( IH, NH, ISKM, NR2, BASE, LBASE, NELMS, ISIZE,
     .                    IPREC, LRPEAT, LDELTA, LSIGDT, ISTAT )
C
C
C     LOGICAL LDEBUG                                                    D
      LOGICAL LRPEAT, LDELTA, LSIGDT, LBASE
C
      INTEGER NH, IVERS, ISTBYT(6), IPREC, ISIZE
C
C     INTEGER*3 NELMS                                                   H
      INTEGER*4 NELMS                                                   MLu
C
C     INTEGER*6 IH(*), IBASE                                            H
      INTEGER*4 IH(*), IBASE                                            MLu
C
C     COMMON / DCDBUG / LDEBUG, MUNIT                                   D
C
      EQUIVALENCE ( RBASE, IBASE )
C
      ISTAT = 0
C
C --- WRITE OUT SUBROUTINE NAME AND VERSION # IF DEBUG OPTION IS ON
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'SUBROUTINE DHINFO (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA COMPRESSION HEADER - INFORMATION'        D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- INITIALIZE VARIABLES USED IN SUBROUTINE
C
      ISKM = 0
      IPREC = 0
C
      BASE = 0.
      LBASE = .FALSE.
C
C --- BEFORE RETRIEVING INFORMATION FROM THE DATA COMPRESSION HEADER,
C --- CALL 'DHCHEK' TO CHECK THE INTEGRITY OF THE DATA COMPRESSION
C --  HEADER. IF 'ISTAT' IS NOT EQUAL TO ZERO, PRINT ERROR MESSAGE.
C
      CALL DHCHEK ( ISTBYT, IH, NH, ISTAT )
C
      IF ( ISTAT .NE. 0 ) THEN
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'ERROR OCCCURED IN SUBROUTINE DHCHEK'       D
C           WRITE (MUNIT,*) 'DATA COMPRESSION HEADER PROBABLY CORRUPT'  D
C           WRITE (MUNIT,*) 'ISTAT: ', ISTAT, ' NH: ', NH               D
C           WRITE (MUNIT,*) ' '                                         D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- DETERMINE WHICH COMPRESSION SCHEMES ARE USED. IF SCHEME NUMBER HAS
C --- BEEN CORRUPTED AND IS NOT VALID, PRINT ERROR MESSAGE AND RETURN.
C
      LRPEAT = .FALSE.
      LDELTA = .FALSE.
      LSIGDT = .FALSE.
C
      CALL GETHOL ( IH(1), 4, ISKM )
C
      IF ( ISKM .LE. 0 .OR. ISKM .GE. 6 ) THEN
         ISTAT = -1701
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'INVALID DATA COMPRESSION SCHEME NUMBER'    D
C           WRITE (MUNIT,*) 'ISKM:  ', ISKM, ' ISTAT: ', ISTAT          D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
      JSKM = ISKM
C
      DO 200 I=7,0,-1
C
         IF ( JSKM .GE. 2**I ) THEN
            IF ( I .EQ. 0 ) LRPEAT = .TRUE.
            IF ( I .EQ. 1 ) LDELTA = .TRUE.
            IF ( I .EQ. 2 ) LSIGDT = .TRUE.
            JSKM = JSKM - 2**I
         ENDIF
C
  200 CONTINUE
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) 'LRPEAT: ', LRPEAT                             D
C        WRITE (MUNIT,*) 'LDELTA: ', LDELTA                             D
C        WRITE (MUNIT,*) 'LSIGDT: ', LSIGDT                             D
C     ENDIF                                                             D
C
C --- COMPRESSION SCHEME NUMBER IS VALID. DOUBLE CHECK THAT BOTH THE
C --- DELTA AND SIGNIFICANT DIGITS METHODS HAVE NOT BEEN CALLED
C --- SIMULTANEOUSLY.
C
      IF (  LDELTA .AND. LSIGDT ) THEN
         ISTAT = -1702
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) 'ERROR - THE DELTA AND SIGNIFICANT DIGITS'  D
C           WRITE (MUNIT,*) 'SCHEMES HAVE BOTH BEEN REQUESTED'          D
C           WRITE (MUNIT,*) 'ISKM: ', ISKM, ' ISTAT: ', ISTAT           D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- GET 'NELMS' FROM THE HEADER ARRAY
C
         CALL GETHOL ( IH, 5, NTMP )
         CALL PUTHOL ( NELMS, 1, NTMP )
         CALL GETHOL ( IH, 6, NTMP )
         CALL PUTHOL ( NELMS, 2, NTMP )
         CALL GETHOL ( IH, 7, NTMP )
         CALL PUTHOL ( NELMS, 3, NTMP )
C
C
         CALL GETHOL ( IH, 8, NTMP )                                    MLu
         CALL PUTHOL ( NELMS, 4, NTMP )                                 MLu
C
C        IF ( LDEBUG ) THEN                                             D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) '# ARRAY ELEMENTS: ', NELMS                 D
C        ENDIF                                                          D
C
      IF ( NELMS .LE. 0 ) THEN
         ISTAT = -1703
C        WRITE (MUNIT,*) 'ERROR: NELMS IS LESS THAN 1'                  D
C        WRITE (MUNIT,*) 'NELMS: ', NELMS, ' ISTAT: ', ISTAT            D
         GOTO 900
      ENDIF
C
C --- IF THE REPEAT SCHEME IS REQUESTED, DETERMINE THE NUMBER OF
C --- NON-REPEATED REALS THAT HAVE BEEN COMPRESSED, BY COUNTING
C --- THE NUMBER OF COMPRESSION BITS THAT ARE SET IN THE 'IH' ARRAY
C
      NR2 = 0
C
      IF ( LRPEAT ) THEN
C
         DO 250 I=1,NELMS
C           CALL BITS (IH(6), I, IBIT, .TRUE.)                          H
            CALL BITS (IH(9), I, IBIT, .TRUE.)                          MLu
            IF ( IBIT .EQ. 0 ) NR2 = NR2 + 1
  250    CONTINUE
C
C         IF ( LDEBUG ) WRITE (MUNIT,*) 'NR2: ', NR2                    D
C
      ENDIF
C
C --- IF THE DELTA SCHEME WAS USED FOR DATA COMPRESSION, DETERMINE
C --- 'ISIZE', 'IPREC', 'LBASE', AND 'BASE'.
C
      IF ( LDELTA ) THEN
C
         CALL GETHOL ( IH, 9,  ITMP)
         IF ( CHAR(ITMP) .EQ. 'D' ) THEN
            CALL GETHOL ( IH, 10, IVERS )
            CALL GETHOL ( IH, 11, NBYTES )
C           CALL GETHOL ( IH, ISTBYT(2)+3, ISIZE )
            CALL GETHOL ( IH, 12, ISIZE )
            CALL GETHOL ( IH, 13, IPREC )
         ELSE
C           WRITE (MUNIT,*) 'PROBLEM IN DHINFO'                         D
            ISTAT = -1704
            GOTO 900
         ENDIF
C
C --- REMOVE BIAS FROM IPREC TO GET NEGATIVE 'IPREC'
C
         IPREC = IPREC - 10
C
         CALL GETHOL ( IH, 14, LB )
C
         IF ( LB .NE. 0 ) THEN
            LBASE = .TRUE.
         ELSE
            LBASE = .FALSE.
         ENDIF
C
C        DO 300 I=1,6                                                   H
         DO 300 I=1,4                                                   MLu
            CALL GETHOL ( IH(1), I+15, IBYTE )
            CALL PUTHOL ( IBASE, I,    IBYTE )
  300    CONTINUE
C
         BASE = RBASE
C
C        IF ( LDEBUG ) THEN                                             D
C
C           WRITE (MUNIT,*) 'IPREC: ', IPREC, ' ISIZE: ', ISIZE         D
C
C           IF ( LBASE ) THEN                                           D
C              WRITE (MUNIT,*) 'SPECIFIED BASE VALUE: ', BASE           D
C           ELSE                                                        D
C              WRITE (MUNIT,*) 'COMPUTED BASE VALUE: ', BASE            D
C           ENDIF                                                       D
C
C        ENDIF                                                          D
C
      ENDIF
C
C --- IF THE SIGNIFICANT DIGITS METHOD OF DATA COMPRESSION WAS USED,
C --- DETERMINE THE NUMBER OF BYTES USED TO COMRESS EACH DATA ITEM.
C
      IF ( LSIGDT ) THEN
C
C        CALL GETHOL ( IH, ISTBYT(3)+3, ISIZE )
         CALL GETHOL ( IH, 25, ISIZE )
C
         IF ( LRPEAT ) THEN
         ENDIF
C
      ENDIF
C
C --- IF THE REPEAT COUNT SCHEME OF DATA COMPRESSION IS USED,
C --- GET 'NBYTES' AND 'NBITS' FROM THE HEADER.
C
      IF ( LRPEAT ) THEN
C
        CALL GETHOL ( IH(1), 28, NBYTES )
        CALL GETHOL ( IH(1), 30, NHIGH  )
C
        NBYTES = NHIGH*256 + NBYTES
C
        CALL GETHOL ( IH(1), 29, NBITS )
C
      ENDIF
C
C --- RETURN TO CALLING PROGRAM WITH UNCOMPRESSED DATA OR WITH ERROR
C --- MESSAGES DESCRIBING ANY PROBLEMS
C
  900 RETURN
      END

