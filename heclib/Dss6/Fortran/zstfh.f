      SUBROUTINE zstfh (CLABEL, CITEM, NITEM, IHEAD, KHEAD, NHEAD,
     * IERR)
C
C
C     Stuffs the Users header array with information.
C     Each piece of information is to be identified by a label.
C     This subroutine adds colons, simicolons, and transfers
C     the data to hollerith (and keeps track of the number of
C     bytes used).
C     For example, if:
C        CLABEL(1) = 'DATUM',     CITEM(1) = '1725.0'
C        CLABEL(2) = 'TRANSFORM', CITEM(2) = 'LOGLOG'
C     The header would contain:
C        0032DATUM:1725.0; TRANSFORM:LOGLOG;
C
C     The header is initialized by setting NHEAD to 0 on input.
C     It is returned with the number to store.
C     If NHEAD is not 0, information is appended to the header
C     This subroutine can be called with arrays CLABEL and CITEM,
C     or can be called several times, appending each time.
C
C     Written by Bill Charley, HEC, 1989.
C
C
      CHARACTER CLABEL(*)*(*), CITEM(*)*(*), CBYTES*4
      INTEGER IHEAD(*)
C
      COMMON /WORDS/ IWORD(10)
C
      INCLUDE 'zdssmz.h'
C
C
      IF (MLEVEL.GE.9) THEN
      WRITE (MUNIT,20) KHEAD, NHEAD, NITEM
 20   FORMAT (T5,'-----DSS---- Enter zstfh;  KHEADU:',I6,',  NHEADU:',
     * I6,',  NITEM:',I6)
      DO 60 I=1,NITEM
      WRITE (MUNIT,40) I, CLABEL(I), CITEM(I)
 40   FORMAT (' Number',I4,',  Label: ',A,',  Item: ',A)
 60   CONTINUE
      ENDIF
C
C
      IERR = 0
      IF (NITEM.LE.0) GO TO 940
C
      IF (NHEAD.EQ.0) THEN
C     Initialize the header
      NBYTES = 0
      ELSE
C     Append to the header:  get the current number of bytes
      CALL HOLCHR (IHEAD, 1, 4, CBYTES, 1)
      NBYTES = INTCHR (CBYTES)
      IF (NBYTES.EQ.-1) GO TO 930
      ENDIF
C
      DO 120 I=1,NITEM
C
C     Compute the length of the label and the item
      CALL CHRFLB (CLABEL(I), IBEG, IEND)
      IF (IBEG.EQ.0) GO TO 900
      ILEN = IEND - IBEG + 1
      CALL CHRFLB (CITEM(I), JBEG, JEND)
      IF (JBEG.EQ.0) GO TO 910
      JLEN = JEND - JBEG + 1
C
C     Compute the new length of the header
      IBYTES = ILEN + JLEN + 3
      JBYTES = NBYTES + IBYTES
      NH = (((JBYTES - 1) / IWORD(2)) + 1) + 1
C     Is this less than the dimension of IHEAD?
      IF (NH.GT.KHEAD) GO TO 920
C
C     Now stuff the header
      IPOS = NBYTES + 1
      CALL CHRHOL (CLABEL(I), IBEG, ILEN, IHEAD(2), IPOS)
      IPOS = IPOS + ILEN
      CALL CHRHOL (':', 1, 1, IHEAD(2), IPOS)
      IPOS = IPOS + 1
      CALL CHRHOL (CITEM(I), JBEG, JLEN, IHEAD(2), IPOS)
      IPOS = IPOS + JLEN
      CALL CHRHOL ('; ', 1, 2, IHEAD(2), IPOS)
      NBYTES = JBYTES
 120  CONTINUE
C
C     Update the number of bytes and the header length
      WRITE (CBYTES, 140) NBYTES
 140  FORMAT (I4.4)
      CALL CHRHOL (CBYTES, 1, 4, IHEAD, 1)
      NHEAD = NH
C
 800  CONTINUE
      IF (MLEVEL.GE.9) WRITE (MUNIT, 820) IERR, NHEAD,
     * (IHEAD(I),I=1,NHEAD)
C820  FORMAT (T5,'----Exit zstfh, STATUS:',I5,',  NHEAD:',I6,
C    * ',  Header:',/,20(1X,12A8))
 820  FORMAT (T5,'----Exit zstfh, STATUS:',I5,',  NHEAD:',I6,
     * ',  Header:',/,20(1X,18A4))
      RETURN
C
 900  CONTINUE
      IERR = 1
      GO TO 800
C
 910  CONTINUE
      IERR = 2
      GO TO 800
C
 920  CONTINUE
      IERR = 3
      GO TO 800
C
 930  CONTINUE
      IERR = 4
      GO TO 800
C
 940  CONTINUE
      IERR = 5
      GO TO 800
C
      END

