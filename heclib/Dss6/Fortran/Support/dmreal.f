      SUBROUTINE DMREAL ( R, IBGELM, NELMS, ICMVAL, NCMVAL, KCMVAL,
     .                    IH, NH, ISTAT, ISKM )
C
      PARAMETER ( NCHARS = 5 )
C
C
C     LOGICAL LDEBUG                                                    D
      LOGICAL LRPEAT, LDELTA, LSIGDT, LBASE
C
      INTEGER ICMVAL(*), IH(*)
      INTEGER IBGELM, NCMVAL, NH, ISTAT
C
C     INTEGER*3 NELMS                                                   H
      INTEGER*4 NELMS                                                   MLu
C
      CHARACTER*1 CSCHMS(NCHARS)
C
      REAL R(*)
C
C --- TURN ON THE DEBUG OPTION IF ISTAT IS SET TO -999,
C --- THEN RESET ISTAT TO 0
C
C     IF (ISTAT .NE. -999) THEN                                         D
C       LDEBUG = .TRUE.                                                 D
C     ELSE                                                              D
C       LDEBUG = .FALSE.                                                D
C     ENDIF                                                             D
C
C --- INITIALIZE ISTAT = 0 TO INDICATE NO EXISTING ERROR CONDITIONS
C --- IF ERROR OCCURS ISTAT WILL BE SET NEGATIVE AT LOCATION OF ERROR.
C
      ISTAT = 0
C
C --- CHECK FOR A POSITIVE NUMBER OF ELEMENTS TO MODIFY
C
      IF ( NELMS .LE. 0 ) THEN
         ISTAT = -1901
C        IF (LDEBUG) THEN                                               D
C           WRITE (*,*) 'ISTAT = ',ISTAT                                D
C           WRITE (*,*) 'NELMS IS LESS THAN OR EQUAL TO 0'              D
C           WRITE (*,*) 'COMPRESSION MODIFICATION NOT DONE'             D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- IF IBGELM (BEGINNING ELEMENT) IS LESS THAN OR EQUAL TO 0,
C --- PRINT ERROR MESSAGE AND THEN RETURN
C
      IF ( IBGELM .LE. 0 ) THEN
         ISTAT = -1902
C        IF (LDEBUG) THEN                                               D
C           WRITE (*,*) 'ISTAT = ',ISTAT                                D
C           WRITE (*,*) 'IBGELM IS LESS THAN OR EQUAL TO 0'             D
C           WRITE (*,*) 'COMPRESSION MODIFICATION NOT DONE'             D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- CHECK TO SEE IF THE HEADER ARRAY HAS BEEN PASSED TO THE
C --- SUBROUTINE BY CHECKING THE VALUE FOR 'NH'. IF 'NH' IS ZERO
C --- OR LESS, PRINT ERROR MESSAGE AND RETURN.
C
      IF ( NH .LE. 0 ) THEN
         ISTAT = -1903
C        IF (LDEBUG) THEN                                               D
C           WRITE (*,*) 'ISTAT = ',ISTAT                                D
C           WRITE (*,*) 'NH IS LESS THAN OR EQUAL TO 0'                 D
C           WRITE (*,*) 'COMPRESSION MODIFICATION NOT DONE'             D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- IF THE NUMBER OF COMPRESSED DATA VALUES (NCMVAL) IS LESS THAN
C --- OR EQUAL TO ZERO, PRINT ERROR MESSAGE AND THEN RETURN
C
      IF ( NCMVAL .LE. 0 ) THEN
         ISTAT = -1904
C        IF (LDEBUG) THEN                                               D
C           WRITE (*,*) 'ISTAT = ',ISTAT                                D
C           WRITE (*,*) 'NCMVAL IS LESS THAN OR EQUAL TO 0'             D
C           WRITE (*,*) 'COMPRESSION MODIFICATION NOT DONE'             D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- INITIALIZE THE HEADER SCHEME INDICATORS
C
      CSCHMS(1) = 'C'
      CSCHMS(2) = 'D'
      CSCHMS(3) = 'S'
      CSCHMS(4) = 'R'
C
C --- NEXT CHECK THE INTEGRITY OF THE HEADER ARRAY
C
      DO 100 I=1,NCHARS
         IPOSIT = IH(I+1) - 1
         IF ( CHAR(IH(IPOSIT)) .NE. CSCHMS(IPOSIT) ) THEN
            ISTAT = -1905
C           IF (LDEBUG) THEN                                            D
C              WRITE (*,*) 'ISTAT = ', ISTAT                            D
C              WRITE (*,*) 'HEADER ARRAY APPEARS TO BE CORRUPTED'       D
C              WRITE (*,*) 'COMPRESSION MODIFICATION NOT DONE'          D
C           ENDIF                                                       D
            GOTO 900
         ENDIF
  100 CONTINUE
C
C --- HEADER IS INTACT, SO DETERMINE WHICH COMPRESSION SCHEMES ARE USED
C
      JSKM = ISKM
C
      LRPEAT = .FALSE.
      LDELTA = .FALSE.
      LSIGDT = .FALSE.
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
C --- IF THE REPEAT SCHEME IS USED BY ITSELF OR IN CONJUCTION WITH
C --- ONE OF THE OTHER SCHEMES, DON'T DO COMPRESSED DATA MODIFICATION.
C
      IF (LRPEAT) THEN
         ISTAT = -1906
C        IF (LDEBUG) THEN                                               D
C           WRITE (*,*) 'ISTAT = ',ISTAT                                D
C           WRITE (*,*) 'DMREAL DOES NOT SUPPORT THE'                   D
C           WRITE (*,*) 'REPEAT METHOD OF DATA COMPRESSION'             D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- IF THE REMAINING SCHEMES HAVE NOT BEEN SELECTED, PRINT ERROR
C --- MESSAGE AND RETURN
C
      IF ( .NOT. LDELTA .AND. .NOT. LSIGDT ) THEN
         ISTAT = -1907
C        IF (LDEBUG) THEN                                               D
C           WRITE (*,*) 'ISTAT = ',ISTAT                                D
C           WRITE (*,*) 'SCHEME NOT RECOGNIZED'                         D
C           WRITE (*,*) 'COMPRESSED DATA MODIFICATION NOT DONE'         D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- IF DELTA SCHEME IS SELECTED, COMPRESS THE NEW DATA AND RETURN
C --- TO CALLING PROGRAM.
C
      IF ( LDELTA ) THEN
C
         CALL GETHOL ( IH(1), 13, IPREC )
C
         IF ( LBASE ) THEN
            CALL GETHOL ( IH(1), 13, IPREC )
         ELSE
            BASE = 0.
         ENDIF
C
         CALL DCDLTR ( R(IBGELM), NELMS, IPREC, BASE, LBASE, ICMVAL,
     .                 NCMVAL, KCMVAL, IBBYTE, ISIZE, ISTAT )
         GOTO 900
C
      ENDIF
C
C --- IF SIGNIFICANT DIGITS SCHEME IS SELECTED, COMPRESS THE NEW DATA
C --- AND RETURN TO THE CALLING PROGRAM.
C
      IF ( LSIGDT ) THEN
         CALL DCSD3R ( R(IBGELM), NELMS, ICMVAL, NCMVAL, KCMVAL,
     .                 IBBYTE, ISTAT )
         GOTO 900
      ENDIF
C
C --- RETURN TO THE CALLING PROGRAM
C
  900 RETURN
      END

