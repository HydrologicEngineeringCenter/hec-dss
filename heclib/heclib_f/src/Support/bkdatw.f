C     BLOCK DATA BKDATW                                                 L
      SUBROUTINE BKDATW                                                 HMu
C
C     BLOCK DATA TO SET COMMON BLOCK WORDS
C
C
      COMMON /WORDS/ IWORD(10)
C
C
C     IWORD(1) - NCPW - NUMBER OF CHAR PER PHYSICAL WORD
C     IWORD(2) - NCLW - NUMBER OF CHAR PER LOGICAL WORD
C     IWORD(3) - NBCH - NUMBER OF BITS PER CHARACTER
C     old IWORD(4) - MASKA - CHARACTER MASK
C     IWORD(4) - Character direction for character-Hollerith
C     Set to 0 = forward, -1 = reverse
C     IWORD(5) - MAS - COMPLEMENT OF MASKA
C     IWORD(6) - NBMW - NUMBER OF BITS PER MACHINE WORD
C     IWORD(7) - NCMW - NUMBER OF CHARACTERS PER MACHINE WORD
C     IWORD(8) - INTEGD - NUMBER OF SINGLE INTEGER WORDS IN
C                         DOUBLE INTEGER WORD
C     IWORD(9) - IREALS - NUMBER OF SINGLE INTEGER WORDS IN
C                         SINGLE REAL WORD
C     IWORD(10) - IREALD - NUMBER OF SINGLE INTEGER WORDS IN
C                          DOUBLE REAL WORD
C
C
C     DATA FOR HARRIS
C     DATA IWORD/3,6,8,'377,'77777400,24,3,2,2,4/                       H
C     DATA FOR CDC
C     DATA IWORD/10,10,6,77B,77777777777777777700B,60,10,1,1,2/
C     DATA FOR Unix 32 bit machines
C     DATA IWORD/4,4,8,0,0,32,4,1,1,2/                                  lgms
C     DATA for Cray Unix 64 bit machine
C     DATA IWORD/8,8,8,0,0,64,8,1,1,2/                                  c
C     DATA FOR IBM-PC USING MS DOS WITH 4 BYTE WORDS
      DATA IWORD/4,4,8,0,0,32,4,1,1,2/                                  p
C     DATA FOR IBM-PC USING MS FORTRAN WITH 2 BYTE WORDS
C     DATA IWORD/2,4,8,0,0,16,2,2,2,4/                                  M
C     DATA FOR IBM
C     DATA IWORD/4,4,8,ZFF,ZFFFFFF00,32,4,1,1,2/
C
      RETURN                                                            HMu
      END

