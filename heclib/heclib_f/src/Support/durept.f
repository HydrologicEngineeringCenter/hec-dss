      SUBROUTINE DUREPT (R1, NR1, R2, NR2, I1, NI1, ISTAT)
C
C --- THIS SUBROUTINE IS USED TO UNCOMPRESS REAL DATA ARRAYS USING
C --- THE REPEAT VALUE DATA COMPRESSION SCHEME
C
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     LOGICAL LDEBUG                                                    D
C
      INTEGER I, ISET, ISTAT, NI1, NR1, NR2
C
C     INTEGER*6 I1(*)                                                   H
      INTEGER*4 I1(*)                                                   MLu
C
      REAL R1(*), R2(*)
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
C        WRITE (MUNIT,*) 'SUBROUTINE DUREPT (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA UNCOMPRESSION - REPEAT VALUE METHOD'     D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- CHECK INPUT ARRAY 'R2' FOR ZERO OR NEGATIVE NUMBER OF ELEMENTS
C
      IF (NR2 .LE. 0) THEN
         ISTAT = -2201
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'NUMBER OF ARRAY ELEMENTS NR2 NOT POSITIVE' D
C           WRITE (MUNIT,*) 'NR2: ', NR2, '  ISTAT: ', ISTAT            D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- IF 'LDEBUG' ACTIVE, PRINT TRACE OF INPUT ARRAY 'R2'
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'DUREPT - INPUT ARRAY R2 - NELMS:', NR2        D
C        WRITE (MUNIT,*) ' '                                            D
C        CALL PRTARY ( R2, NR2 )                                        D
C     ENDIF                                                             D
C
C --- CHECK INPUT ARRAY 'I1' FOR ZERO OR NEGATIVE NUMBER OF ELEMENTS
C
      IF (NI1 .LE. 0) THEN
         ISTAT = -2202
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'NUMBER OF ARRAY ELEMENTS NI1 NOT POSITIVE' D
C           WRITE (MUNIT,*) 'NI1: ', NI1, '  ISTAT: ', ISTAT            D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- THE TOTAL NUMBER OF UNCOMPRESSED DATA ELEMENTS SHOULD BE PASSED
C --- IN 'NR1' FROM THE CALLING PROGRAM. CHECK THAT 'NR1' IS POSITIVE
C
      IF (NR1 .LE. 0) THEN
         ISTAT = -2203
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'NUMBER OF ARRAY ELEMENTS NR1 NOT POSITIVE' D
C           WRITE (MUNIT,*) 'NR1: ', NR1, '  ISTAT: ', ISTAT            D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- COMPUTE THE NUMBER OF INTEGER WORDS REQUIRED TO STORE THE
C --- COMPRESSION BIT INFORMATION. ALSO, COMPUTE THE NUMBER OF ACTIVE
C --- BITS IN THE LAST INTEGER WORD.
C
      NBITS = MOD(NR1,(NCPW*8))
C
      IF (NBITS .EQ. 0) THEN
         NWORDS = NR1/(NCPW*8)
      ELSE
         NWORDS = NR1/(NCPW*8) + 1
      ENDIF
C
C --- IF DEBUG IS ACTIVE, PRINT OUT TRACE OF REPEAT COUNT STATISTICS
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'COMPRESSION DATA LENGTH (WORDS): ', NWORDS    D
C        WRITE (MUNIT,*) 'ACTIVE BITS IN LAST WORD (BITS): ', NBITS     D
C        WRITE (MUNIT,*) 'COMPRESSION DATA LENGTH (BITS):  ', NR1       D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- CHECK TO SEE IF ALL COMPRESSION INFORMATION HAS BEEN PASSED
C
      IF (NI1 .LT. NWORDS) THEN
         ISTAT = -2204
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'COMPRESSION INFORMATION MISSING'           D
C           WRITE (MUNIT,*) 'NI1: ', NI1, '  NWORDS: ', NWORDS          D
C           WRITE (MUNIT,*) 'ISTAT: ', ISTAT                            D
C        ENDIF                                                          D
         GOTO 900
      ENDIF
C
C --- NOW UNCOMPRESS THE DATA USING THE REPEAT VALUE METHOD
C
      R1(1) = R2(1)
      ICOUNT = 1
C
      DO 200 I=2,NR1
C
         RSAV = R1(I-1)
C
         CALL BITS ( I1(1), I, ISET, .TRUE.)
C
         IF (ISET .EQ. 0) THEN
            ICOUNT = ICOUNT + 1
            R1(I) = R2(ICOUNT)
         ELSE
            R1(I) = RSAV
         ENDIF
C
  200 CONTINUE
C
C --- IF DEBUG IS ACTIVE, PRINT TRACE OF UNCOMPRESSED OUTPUT ARRAY 'R1'
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'DUREPT - OUTPUT ARRAY R1 - NELMS:', NR1       D
C        WRITE (MUNIT,*) ' '                                            D
C        CALL PRTARY ( R1, NR1 )                                        D
C     ENDIF                                                             D
C
  900 RETURN
      END

