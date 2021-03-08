      SUBROUTINE DCREPT (R1, NR1, R2, NR2, I1, NI1, KI1, ISTAT)
C
C --- THIS SUBROUTINE IS USED TO COMPRESS REAL DATA ARRAYS USING
C --- THE REPEAT VALUE DATA COMPRESSION SCHEME
C
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
C     LOGICAL LDEBUG                                                    D
C
      INTEGER ISTAT, NI1, NR1, NR2, I, ISTBIT
C
C     INTEGER*6 I1(KI1)                                                 H
      INTEGER*4 I1(KI1)                                                 MLu
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
C        WRITE (MUNIT,*) 'SUBROUTINE DCREPT (VERSION # 1.10)'           D
C        WRITE (MUNIT,*) 'DATA COMPRESSION - REPEAT VALUE SCHEME'       D
C        WRITE (MUNIT,*) ' '                                            D
C     ENDIF                                                             D
C
C --- CHECK INPUT ARRAY FOR ZERO OR NEGATIVE NUMBER OF ELEMENTS
C
      IF (NR1 .LE. 0) THEN
         ISTAT = -1401
C        IF (LDEBUG) THEN                                               D
C           WRITE (MUNIT,*) ' '                                         D
C           WRITE (MUNIT,*) 'INPUT ARRAY R1 HAS NO ELEMENTS !!'         D
C           WRITE (MUNIT,*) '# VALS: ', NR1, ' ISTAT: ', ISTAT          D
C        ENDIF                                                          D
      ENDIF
C
C --- IF LDEBUG ACTIVE, PRINT TRACE OF SUBROUTINE INPUT
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'DCREPT - INPUT ARRAY R1 - NELMS:', NR1        D
C        WRITE (MUNIT,*) ' '                                            D
C        CALL PRTARY ( R1, NR1 )                                        D
C     ENDIF                                                             D
C
C --- NOW COMPRESS THE 'R1' ARRAY USING REPEAT VALUE DATA COMPRESSION.
C
      NR2  = 1
      RSAV = R1(1)
      R2(NR2) = RSAV
C
      CALL BITS (I1(1),1, 0, .FALSE.)
C
      ISTBIT = 2
C
      DO 200 I=ISTBIT,NR1
C
         IF (R1(I) .EQ. RSAV) THEN
            CALL BITS (I1(1),I, 1, .FALSE.)
         ELSE
            CALL BITS (I1(1),I, 0, .FALSE.)
            NR2 = NR2+1
            R2(NR2) = R1(I)
         ENDIF
C
         RSAV = R1(I)
C
  200 CONTINUE
C
C --- COMPUTE THE NUMBER OF INTEGER WORDS REQUIRED TO STORE ALL OF THE
C --- COMPRESSION BITS. ALSO, COMPUTE NUMBER OF BITS THAT WILL BE ACTIVE
C --- IN THE LAST INTEGER WORD.
C
      NBITS = MOD(NR1,(NCPW*8))
C
      IF (NBITS .EQ. 0) THEN
         NWORDS = NR1/(NCPW*8)
      ELSE
         NWORDS = NR1/(NCPW*8) + 1
      ENDIF
C
      NI1 = NWORDS
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
C --- IF LDEBUG ACTIVE, PRINT TRACE OF COMPRESSED OUTPUT
C
C     IF (LDEBUG) THEN                                                  D
C        WRITE (MUNIT,*) ' '                                            D
C        WRITE (MUNIT,*) 'DCREPT - OUTPUT ARRAY R2 - NELMS:', NR2       D
C        WRITE (MUNIT,*) ' '                                            D
C        CALL PRTARY ( R2, NR2 )                                        D
C     ENDIF                                                             D
C
  900 RETURN
      END

