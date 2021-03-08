      SUBROUTINE CHRONO ( IFIRST,JDATE,MTIME,NXTDAT )
C     SORT IN CHRONOLOGICAL ORDER USING POINTERS
C     <NXTDAT> TO SUBSEQUENT VALUES.  ORDER IS REFLECTED IN <NXTDAT>.
C     <JDATE> AND <MTIME> DEFINE, RESPECTIVELY, THE
C     JULIAN DATE AND TIME IN MINUTES PAST MIDNIGHT FOR
C     EACH OBSERVATION ( REF: HECLIB ROUTINES ).
C     AN <NXTDAT> DATUE OF ZERO FLAGS THE END OF THE SEQUENCE.
C     THE SORTING METHOD IS BY SWAPPING PAIRS USING THE
C     RELATIONSHIP IF A > B AND B > C THEN A > C.
C     <IFIRST> POINTS TO FIRST VALUE IN ORDER AND IS UPDATED.
C                           DENNIS HUFF 11OCT84
C
      DIMENSION JDATE(*),MTIME(*),NXTDAT(*)
      INTEGER*4 JDATE                                                   M
C     LOGICAL LPOPT                                                     H
C
   10 CONTINUE
C     WRITE ( 21,* ) 'BEGIN NEW PASS '                                  DEBUG
      INOW = IFIRST
      ILAST = 0
      ITRY = NXTDAT(INOW)
      NSWAP = 0
C
   20 IF ( ITRY.GT.0 ) THEN
C     WRITE ( 21,* ) 'ILAST=',ILAST,'INOW=',INOW,'ITRY=',ITRY           DEBUG
C     WRITE( 21,*) 'JDATE(INOW)=',JDATE(INOW),'MTIME(INOW)',MTIME(INOW) DEBUG
C     WRITE( 21,*) 'JDATE(ITRY)=',JDATE(ITRY),'MTIME(ITRY)',MTIME(ITRY) DEBUG
         IF ( ( JDATE(ITRY).LT.JDATE(INOW) ) .OR.
     1       ( JDATE(ITRY).EQ.JDATE(INOW).AND.
     2       MTIME(ITRY).LT.MTIME(INOW) ) ) THEN
C
C        SWAP
C           WRITE ( 21,* ) 'SWAP'                                       DEBUG
            NSWAP = NSWAP + 1
            IF ( ILAST.EQ.0 ) THEN
               IFIRST = ITRY
            ELSE
               NXTDAT(ILAST) = ITRY
            ENDIF
            ILAST = ITRY
            NXTDAT(INOW) = NXTDAT(ITRY)
            NXTDAT(ITRY) = INOW
            ITRY = NXTDAT(INOW)
         ELSE
C
C        ORDER OK
C           WRITE ( 21,* )'ORDER OK'                                    DEBUG
            ILAST = INOW
            INOW = ITRY
            ITRY = NXTDAT(INOW)
         ENDIF
         GO TO 20
      ENDIF
      IF ( NSWAP.GT.0 ) GO TO 10
C
      RETURN
C
      END
C     CHARACTER CHR*20
C     INTEGER*6 IB(5)
C
C  10 CONTINUE
C     READ ( 20,'(A)') CHR
C     CALL CTOA4 ( CHR,1,20,IB,1,NB )
C     WRITE ( 21,* ) 'NB = ',NB
C     WRITE ( 21,'(1X,5A4 )') IB
C     IF ( CHR(1:3).NE.'END') GO TO 10
C     STOP
C     END
