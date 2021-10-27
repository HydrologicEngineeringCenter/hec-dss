C     ---------------------------------------
C
C     DSS Time-Series Buffer Common Block
      integer klbuff, nibuff, ksbuff, kdbuff
      PARAMETER (KLBUFF=500000,NIBUFF=1000)
C	  PARAMETER (KLBUFF=12000,NIBUFF=500)
      PARAMETER (KSBUFF=(KLBUFF/2))
      PARAMETER (KDBUFF=(KLBUFF/2))
C
      REAL BUFF(KLBUFF)
      REAL SBUFF1(KSBUFF), SBUFF2(KSBUFF)
      DOUBLE PRECISION DBUFF(KDBUFF)
      INTEGER ILBUFF(KLBUFF), INTBUF(NIBUFF)
      EQUIVALENCE (ILBUFF,BUFF)
C     Be sure BUFF and SBUFF1 are equivalenced as follows!!
      EQUIVALENCE (BUFF(1), SBUFF1), (BUFF(KSBUFF+1), SBUFF2)
      EQUIVALENCE (BUFF, DBUFF)
      COMMON /ZDSSTS/ BUFF
      COMMON /ZDSSTI/ INTBUF
C
C       Time zone information
      CHARACTER CRTZONE*24, CWTZONE*24
      INTEGER IRTZONE, IWTZONE
      COMMON /ZDSSTZC/ CRTZONE, CWTZONE
      COMMON /ZDSSTZI/ IRTZONE, IWTZONE

C
C     ---------------------------------------
C

