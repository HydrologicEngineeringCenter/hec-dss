      SUBROUTINE BSRCH ( KEY,LIST,NLIST,ILIST )
C
C     <GETSEN> FINDS THE POINTER <ILIST> TO A LIST LABEL <LIST>
C     WHICH MATCHES THE STRING <KEY>.
C     THE METHOD IS BINARY SEARCH ( ALSO CALLED BRACKETING )---
C     THE LIST OF LISTS IS ASSUMED TO BE IN ALPHABETICAL ORDER.
C     THE NUMBER OF LISTS IN THE LIST IS <NLIST>.
C     THE POINTER <ILIST> IS RETURNED AS 0 (ZERO) IF
C     <KEY> DOES NOT MATCH ANY ENTRY IN THE LIST LIST.
C                   DENNIS HUFF    MARCH 1 1085
C
      CHARACTER KEY*(*),LIST(*)*(*)
C
*      WRITE ( *,* ) 'SEARCHING FOR MATCH FOR ***',KEY,'***'
      ILIST = 0
	IF (NLIST.LE.0) GO TO 90
      IF (LLT(KEY,LIST(1)).OR.LGT(KEY,LIST(NLIST)) ) GO TO 90
      ILO=1
      IHI=NLIST
      ILAST = 0
 10   CONTINUE
         ILIST=(IHI+ILO)/2
*         WRITE(*,* )CHAR(10),CHAR(11),'ILO =',ILO,
*     1    ' ILIST = ', ILIST,' IHI = ',IHI
            IF ( LLT(KEY,LIST(ILIST))) THEN
               IHI = ILIST
            ELSE IF ( LGT(KEY,LIST(ILIST))) THEN
               ILO = ILIST
            ELSE
              GO TO 90
            ENDIF
            IF ( ILAST.NE.ILIST ) THEN
               ILAST = ILIST
               GO TO 10
            ELSE IF ( ILO.LT.IHI.AND.IHI.EQ.NLIST ) THEN
               ILO = IHI
               ILAST = ILIST
               GO TO 10
            ELSE IF ( IHI.GT.ILO.AND.ILO.EQ.1 ) THEN
               IHI = ILO
               ILAST = ILIST
               GO TO 10
            ELSE
               ILIST = 0
            ENDIF
  90  CONTINUE
      IF (ILIST.EQ.0) THEN
         DO 100 I=1,NLIST
            IF (KEY.EQ.LIST(I)) THEN
               ILIST = I
               GO TO 800
            ENDIF
 100     CONTINUE
      ENDIF
C
 800  CONTINUE
      RETURN
C
      END
