      SUBROUTINE DHCHEK ( ISTBYT, IH, NH, ISTAT )
C
C --- THIS SUBROUTINE CHECKS THE INTEGRITY OF THE DATA COMPRESSION
C --- HEADER - 'ISTAT' IS SET TO -1 WHEN AN ERROR OCCURS AND
C --- PROGRAM CONTROL PASSES BACK TO THE CALLING SUBROUTINE
C
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     LOGICAL LDEBUG                                                    D
      LOGICAL LHEAD
C
      INTEGER NH, ISTBYT(*)
C
C     INTEGER*6 IH(*)                                                   H
      INTEGER*4 IH(*)                                                   MLu
C
      CHARACTER*1 CVAR, CHEAD(5)
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
C        WRITE (MUNIT,*) 'SUBROUTINE DHCHEK (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA COMPRESSION HEADER - INTEGRITY CHECK'    D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- CHECK TO SEE IF THE HEADER HAS BEEN PASSED. IF NH IS LESS THAN
C --- OR EQUAL TO ZERO, PRINT ERROR MESSAGE AND RETURN.
C
      IF ( NH .LE. 0 ) THEN
         ISTAT = -1601
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'NUMBER OF ELEMENTS IN HEADER IS LESS '     D
C           WRITE (MUNIT,*) 'THAN OR EQUAL TO ZERO.'                    D
C           WRITE (MUNIT,*) 'NH: ', NH, ' ISTAT: ', ISTAT               D
C           WRITE (MUNIT,*) ' '                                         D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- CHECK VERSION NUMBERS FOR ALL PORTIONS OF THE DATA COMPRESSION
C --- HEADER. DON'T PROCEED UNLESS ALL VERSION NUMBERS ARE VALID.
C
C --- CHECK THE VERSION NUMBER FOR THE DATA COMPRESSION HEADER. IF THE
C --- WRONG VERSION IS FOUND, PRINT ERROR MESSAGE AND RETURN.
C
      CALL GETHOL ( IH(1), 2, IVERS )
C
      IF ( IVERS .NE. 16 ) THEN
         ISTAT = -1602
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'INVALID DATA COMPRESSION VERSION'          D
C           WRITE (MUNIT,*) 'IVERS:  ', IVERS, ' ISTAT: ', ISTAT        D
C           WRITE (MUNIT,*) ' '                                         D
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
C --- CHECK THE INTEGRITY OF THE DATA COMPRESSION HEADER BY EXAMINING
C --- THE FIRST BYTE OF EACH SECTION FOR THE PROPER ASCII CHARACTER.
C --- IF THE WRONG CHARACTER IS FOUND, PRINT AN ERROR MESSAGE AND
C --- RETURN TO THE CALLING PROGRAM.
C
      ISTBYT(1) = 1
C
      NTOTAL = 0
C
      DO 100 I=1,5
C
      CALL GETHOL ( IH(1), ISTBYT(I), IBYTE )
      CVAR(1:1) = CHAR( IBYTE )
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) 'LETTER: ', CVAR, CHEAD(I), I                  D
C     ENDIF                                                             D
C
      IF ( CVAR(1:1) .NE. CHEAD(I) ) THEN
         ISTAT = -1603
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'CORRUPTED OR INVALID COMPRESSION HEADER'   D
C           WRITE (MUNIT,*) 'CHEAD(I): ', CHEAD(I), ' CVAR: ', CVAR     D
C           WRITE (MUNIT,*) 'ISTAT: ', ISTAT                            D
C           WRITE (MUNIT,*) ' '                                         D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
      IF ( I .EQ. 4 ) THEN
         IBYT = ISTBYT(I) + 2
         CALL GETHOL ( IH, IBYT, NBYTES )
         CALL GETHOL ( IH, IBYT+2, NHIGH )
         NBYTES = 256*NHIGH + NBYTES
         NTOTAL = NTOTAL + NBYTES + 256*NHIGH
      ELSE
         IBYT = ISTBYT(I) + 2
         CALL GETHOL ( IH, IBYT, NBYTES )
         NTOTAL = NTOTAL + NBYTES
      ENDIF
C     WRITE (*,*) 'NBYTES: ', NBYTES, ' NTOTAL: ', NTOTAL
C
      ISTBYT(I+1) = ISTBYT(I) + NBYTES
C
  100 CONTINUE
C
 900  CONTINUE
C
C --- DETERMINE LENGTH OF HEADER ARRAY IN INTEGER WORDS
C
C     IREMAN = MOD(NH, NCPW)
C
C     IF ( IREMAN .EQ. 0 ) THEN
C        NWORDS = NH/NCPW
C     ELSE
C        NWORDS = NH/NCPW + 1
C     ENDIF
C
C --- IF DEBUG OPTION IS TURNED ON, PRINT OUT DATA COMPRESSION HEADER
C
      LHEAD = .TRUE.
C
C     IF (LDEBUG) THEN                                                  D
C        CALL PRTWRD ( IH(1), NH, LHEAD )                               D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
      RETURN
      END

