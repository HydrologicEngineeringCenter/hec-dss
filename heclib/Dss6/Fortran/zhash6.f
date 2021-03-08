      SUBROUTINE zhash6 (CPATH, NPATH, MAX, IHASH)
C
C
C     Written by Art Pabst at HEC, 1988.
C
      CHARACTER CPATH*(*), CT*400
      INTEGER ISIZE(0:13)
C
      INCLUDE 'zdssmz.h'
C
      DATA ISIZE/ 1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192/
C ------          0,1,2,3, 4, 5, 6,  7,  8,  9,  10,  11,  12,  13
      DATA IB / 256 /
C
C
C
      IBYTE = 0
C
      DO 20 I=9,13
      IF (MAX.LE.ISIZE(I)) THEN
      IBIT = I
      GO TO 40
      ENDIF
 20   CONTINUE
C     Should never get here
      IBIT = I
C
 40   CONTINUE
C
C ------ FIND NUMBER OF CHARACTERS THAT INCLUDE FULL NUMBER OF GROUPS
C ------ This rounds up to full number of 8 bit char to process when
C ------ you treat them in IBIT chunks !
C ------ String will be blank filled for any spill-over beyound end.
C
      NT = (NPATH*8-1) / IBIT + 1
      NT = (NT*IBIT-1) / 8 + 1
      CT = CPATH(1:NPATH)
      CT(NPATH+1:NPATH+2) = CHAR(0)//CHAR(0)
C
      IT = 0
      I2 = 0
      INEED = IBIT
      IHAVE = 0
C
 60   CONTINUE
C
C ------ GET A GROUP OF BITS
C
      IF ( IHAVE .GT. 0 ) THEN
      IMOVE = MIN ( INEED, IHAVE )
C ------ MULT BY POWER OF TWO TO CAUSE LEFT SHIFT
      I2 = I2 * ISIZE(IMOVE)
      INEED = INEED - IMOVE
      IHAVE = IHAVE - IMOVE
      IF ( INEED .LE. 0 ) THEN
      ITP = I2 / 256
      I2 = MOD ( I2, IB )
C
C ------ Use Exclusive OR to form value
C
      IT = IEOR ( IT, ITP )
      INEED = IBIT
      ENDIF
      ENDIF
C
C ------ REFILL A CHARACTER
C
      IF ( IHAVE .LE. 0 ) THEN
      IBYTE = IBYTE + 1
      IF ( IBYTE .GT. NT ) GO TO 100
      IC = ICHAR ( CT(IBYTE:IBYTE) )
      I2 = I2 + IC
      IHAVE = 8
      ENDIF
C ------
      GO TO 60
C
C     All Done
 100  CONTINUE
      IHASH = MOD ( IT, MAX ) + 1
C
      IF (MLEVEL.GE.12) WRITE (MUNIT, 120) MAX, IHASH, CPATH(1:NPATH)
 120  FORMAT (T8,'-----DSS---Debug:  Exit zhash6;',/,
     * T12,'Maximum Hash:',I6,',  Computed Hash:',I6,/,
     * T12,'Path: ',A)
C
      RETURN
      END

