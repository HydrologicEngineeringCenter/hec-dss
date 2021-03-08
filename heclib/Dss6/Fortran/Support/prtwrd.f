      SUBROUTINE PRTWRD ( IADDR, NWORDS, LHEAD )
C
C
      COMMON /WORDS/ NCMW, NCPW, IWDS(8)
C
      LOGICAL LBTEST, LHEAD
C
      INTEGER NWORDS, ITMP
C
C     INTEGER*3 IADDR(*)                                                H
      INTEGER*4 IADDR(*)                                                MLu
C
      CHARACTER CBIT*32, CLINE*80, CHEX*8
C
      CLINE(1:15)  = '     BIT #:    '
      CLINE(16:61) = '31......24  23......16  15......08  07......00'
C
      IF (LHEAD) THEN
         WRITE (*,*) ' '
C        WRITE (*,*) CLINE(1:15)//CLINE(28:61)                          H
         WRITE (*,*) CLINE(1:61)                                        MLu
         WRITE (*,*) ' '
      ENDIF
C
C --- IF 'NWORDS' IS LESS THAN OR EQUAL ZERO, ASSUME 'NWORDS' AS 1
C
      IF ( NWORDS .LE. 0 ) NW = 1
C
C --- IF USING INTEGER*6 ON HARRIS COMPUTER, ADJUST PARAMETERS
C
C     IF ( NCPW   .EQ. 6 ) THEN                                         H
C       NBYT = 3                                                        H
C       NW = NWORDS*2                                                   H
C     ELSE                                                              H
        NBYT = NCPW
        NW = NWORDS
C     ENDIF                                                             H
        NBIT = NBYT * 8
C
C --- DETERMINE AND PRINT BIT SETTINGS FOR EACH ELEMENT OF THE INPUT
C --- ARRAY 'IADDR' FOR 'NWORDS' OF THE ARRAY
C
      DO 600 IWORD=1,NW
C
C --- DETERMINE IF EACH BIT IN THE INTEGER WORD 'IADDR' IS SET OR RESET
C
         DO 400 I=1,NBYT
C
            ITMP = 0
            CALL GETHOL ( IADDR(IWORD), I, ITMP )
C
            DO 300 J=1,8
C
               K = I*8 - 8 + J
C
               IF ( LBTEST(ITMP,J-1) ) THEN
                  CBIT(K:K) = '1'
               ELSE
                  CBIT(K:K) = '0'
               ENDIF
C
  300       CONTINUE
C
  400    CONTINUE
C
C --- FOR EACH BYTE IN THE INTEGER WORD 'IADDR', ASSIGN ANY PRINTABLE
C --- ASCII CHARACTERS TO THE 'CHEX' ARRAY.
C
         DO 500 I=1,NBYT
C
            CALL GETHOL ( IADDR(IWORD), I, ITMP )
C
            IF ( ITMP .GE. 32 .AND. ITMP .LE. 126 ) THEN
               CHEX(2*I-1:2*I) = CHAR(ITMP)//' '
            ELSE
               CHEX(2*I-1:2*I) = '..'
            ENDIF
C
  500    CONTINUE
C
C --- NOW PRINT OUT A BIT REPRESENTATION OF THE INTEGER WORD 'IADDR'.
C --- ALSO, PRINT OUT IT'S INTEGER REPRESENTATION AND PRINTABLE
C --- ASCII CHARACTERS.
C
         WRITE (*,'(1X,I11,5X,4(8A1,4X),4(1X,2A1))') IADDR(IWORD),
     .         ( CBIT(I:I),I=NBIT,1,-1 ), ( CHEX(I:I),I=8,1,-1 )
C
C --- LOOP BACK TO TOP OF SUBROUTINE TO PRINT OUT ADDITIONAL ELEMENTS
C --- OF THE INPUT ARRAY 'IADDR'
C
  600 CONTINUE
C
C --- RETURN TO THE CALLING PROGRAM
C
      RETURN
      END

