      SUBROUTINE zustfh (CLABEL, CITEM, NITEM, IPOS, IHEAD, NHEAD,
     * IERR)
      implicit none
C
C     Unstuffs the Users header array.
C     See the subroutine zstfh for a description of the header.
C
C     To have zustfh look for specific items, set the label
C     names in CLABEL, and set NITEM to the number of items to
C     search for.
C     For example, if:
C        CLABEL(1) = 'DATUM',
C        CLABEL(2) = 'TRANSFORM',
C        NITEM = 2
C     zustfh would search the header for those items.
C     If an item is not found, CITEM for that label is blank filled.
C
C     To get all items from the header, set NITEM to 0 and IPOS to 0.
C     Create a loop calling ZSTUFH, getting a single lable and item
C     on each call.  Exit the loop when IPOS is -1.  For example:
C         NITEM = 0
C         IPOS = 0
C         LOOP
C         CALL zustfh (
C         WRITE (6,10) CLABEL, CITEM
C         EXIT LOOP IF (IPOS.LT.0)
C         ENDLOOP
C
C     If the header is not valid, IERR is returned as negative.
C
C     Summarizing:  If NITEM .GE.1, IPOS is ignored and the header
C                   is searched for the specified items.
C                   If NITEM = 0, and IPOS = 0 for first call,
C                   all header items will be retrieved by successive
C                   calls.
C
C
C     Written by Bill Charley, HEC, 1989.
C
      
C
      CHARACTER CLABEL(*)*(*), CITEM(*)*(*)
      INTEGER NITEM, NHEAD, IERR, IPOS
C
C
      CHARACTER C*1, CBYTES*4, CIT*60, CL*60
      INTEGER IHEAD(*)
      LOGICAL LSET, LABEL
C
      INTEGER JITEM, J, ICOUNT, NBYTES, NCPW, INTCHR
      INTEGER ILABEL, NL, NIT, I, ICH, NW, IITEM
C
C
C
      IERR = 0
      NCPW = 4
      IF (NHEAD.LE.0) GO TO 900
C
      IF (NITEM.EQ.0) THEN
      ILABEL = LEN(CLABEL(1))
      LSET = .FALSE.
      ELSE
      LSET = .TRUE.
      JITEM = NITEM
      ICOUNT = 0
      DO 20 J=1,JITEM
      CITEM(J) = ' '
 20   CONTINUE
      ENDIF
C
      IF ((LSET).OR.(IPOS.EQ.0)) THEN
      CALL HOLCHR (IHEAD, 1, 4, CBYTES, 1)
      NBYTES = INTCHR (CBYTES)
      IF (NBYTES.EQ.-1) GO TO 900
      ENDIF
C
      IF (NBYTES.LE.0) GO TO 910
      NW = ((NBYTES - 1) / NCPW) + 2
      IF (NHEAD.LT.NW) GO TO 910
C
C
      IITEM = LEN(CITEM(1))
      CL = ' '
      CIT = ' '
      NL = 0
      NIT = 0
      LABEL = .TRUE.
C
C     This is essentially a do loop...
      IF ((LSET).OR.(IPOS.EQ.0)) THEN
      I = 0
      ELSE
      I = IPOS
      ENDIF
 100  CONTINUE
      I = I + 1
      IF (I.GT.NBYTES) GO TO 800
C
      CALL GETHOL (IHEAD(2), I, ICH)
      C = CHAR(ICH)
C
      IF (C.EQ.';') THEN
      I = I + 1
C
      IF (LSET) THEN
      DO 120 J=1,JITEM
      CALL CHRLNB (CLABEL(J), ILABEL)
      IF (NL.EQ.ILABEL) THEN
      IF (CLABEL(J)(1:NL).EQ.CL(1:NL)) THEN
      CITEM(J) = CIT
      IF (NIT.GT.IITEM) IERR = 2
      ICOUNT = ICOUNT + 1
      IF (ICOUNT.GE.JITEM) GO TO 800
      ENDIF
      ENDIF
 120  CONTINUE
C
      CL = ' '
      CIT = ' '
      NL = 0
      NIT = 0
      LABEL = .TRUE.
C
      ELSE
      CLABEL(1) = CL
      CITEM(1) = CIT
      IF ((NL.GT.ILABEL).OR.(NIT.GT.IITEM)) IERR = 2
      GO TO 800
      ENDIF
C
      ELSE IF (C.EQ.':') THEN
      LABEL = .FALSE.
C
      ELSE
      IF (LABEL) THEN
      NL = NL + 1
      IF (NL.GT.60) GO TO 930
      CL(NL:NL) = C
      ELSE
      NIT = NIT + 1
      IF (NIT.GT.60) GO TO 930
      CIT(NIT:NIT) = C
      ENDIF
C
      ENDIF
C
      GO TO 100
C     end of do loop
C
C
C
 800  CONTINUE
      IF (.NOT.LSET) THEN
      IF (I.GE.NBYTES) THEN
      IPOS = -1
      ELSE
      IPOS = I
      ENDIF
      ENDIF
      RETURN
C
 900  CONTINUE
      IERR = -1
      GO TO 800
C
 910  CONTINUE
      IERR = -2
      GO TO 800
C
 930  CONTINUE
      IERR = 4
      GO TO 800
C
      END

