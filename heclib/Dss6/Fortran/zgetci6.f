      SUBROUTINE zgetci6 (IFLTAB, CPARTI, JCOMP, DBASE,
     * LDBASE, LDHIGH, NPRE)
C
C
C     Determine if data for this pathname, based on its pathname parts,
C     should be compressed by default.  This routine reads the default
C     compression information block from the file, then checks the
C     pathname parts from that block.  If it is found, JCOMP is
C     returned with a positive value (otherwise it is zero).
C
C     Written by Bill Charley, HEC, August, 1989.
C
      INTEGER IFLTAB(*)
      CHARACTER CPARTI(6)*(*)
      LOGICAL LDBASE, LDHIGH, LFIND
      INTEGER IWILD(6)
C
C     CPARTF are the parts set in the file,
C     CPARTI are the parts being passed in.
      CHARACTER CPARTF(6)*64
      INTEGER ILENF(6), ILENI(6)
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssmz.h'
C
C
C
      IF (MLEVEL.GE.10) WRITE (MUNIT, 20)
 20   FORMAT (T5,'----DSS--- Entering zgetci6')
C
      JCOMP = 0
C     Use LFIND to determine if zsetci6 is calling to find location
C     of compression information to delete or modify
      LFIND = .FALSE.
      IF (NPRE.EQ.-99) LFIND = .TRUE.
C     Is file compression information set?
      IF (IFLTAB(KCOMPN).EQ.0) GO TO 800
C
      ISIZE = IFLTAB(KCOMPN)
      ICPOS = 0
C
C     Has the compression information been read yet?
      IF (IFLTAB(KCOMP).EQ.0) THEN
      CALL zgtrec6(IFLTAB,IFLTAB(KCOMP), NCOMP, IFLTAB(KCOMPI), .FALSE.)
      ENDIF
C
      DO 40 I=1,6
      CALL CHRLNB (CPARTI(I), ILENI(I))
 40   CONTINUE
C
      JNEXT = 1
C
C     Loop through each set, comparing pathname parts
C     LOOP
 60   CONTINUE
C     IF (JNEXT.GT.IFLTAB(KCOMPN)) EXIT LOOP
      IF (JNEXT.GT.IFLTAB(KCOMPN)) GO TO 800
C
      NEXT = KCOMP + JNEXT - ICPOS - 1
      IF (IFLTAB(NEXT).EQ.0) GO TO 800
C
      IF (IFLTAB(NEXT).GT.NCOMP+ICPOS) THEN
      IADD = IFLTAB(KCOMPI) + JNEXT - 1
      ICPOS = JNEXT - 1
      N = IFLTAB(KCOMPN) - ICPOS
      ISIZE = MIN (N,NCOMP)
      CALL zgtrec6 (IFLTAB, IFLTAB(KCOMP), ISIZE, IADD, .FALSE.)
      NEXT = KCOMP
      ENDIF
C
C     Get compression scheme, base, etc. for this set
      CALL CHGTYP (IFLTAB(NEXT+1), BASE)
      CALL GETHOL (IFLTAB(NEXT+2), 1, ISKM)
      IF (ISKM.LE.0) GO TO 200
      CALL GETHOL (IFLTAB(NEXT+2), 2, IBASE)
      CALL GETHOL (IFLTAB(NEXT+2), 3, NBYTES)
      CALL GETHOL (IFLTAB(NEXT+2), 4, NPR)
      CALL GETHOL (IFLTAB(NEXT+2), 5, NPARTS)
C     (NPRE are offset by 50 so that negative
C     numbers can be stored in one byte)
      NPR = NPR - 50
C
C     Clear part lengths (-1 means part is not compared)
      DO 80 I=1,6
      ILENF(I) = -1
      IWILD(I) = 0
 80   CONTINUE
C
C     Get required parts and their lengths
      IPOS = 6
      DO 100 I=1,NPARTS
      CALL GETHOL (IFLTAB(NEXT+2), IPOS, IPART)
      IPOS = IPOS + 1
      CALL GETHOL (IFLTAB(NEXT+2), IPOS, ILENF(IPART))
      IPOS = IPOS + 1
      IF (ILENF(IPART).GT.0) THEN
      CALL HOLCHR (IFLTAB(NEXT+2), IPOS, ILENF(IPART), CPARTF(IPART), 1)
      IPOS = IPOS + ILENF(IPART)
C
C     Check for a wild character at then beginning of the part
      IF (CPARTF(IPART)(1:1).EQ.'@') THEN
      IF (.NOT.LFIND) THEN
      IWILD(IPART) = 1
C     If this part is an @ only, don't check it.
      IF (ILENF(IPART).EQ.1) ILENF(IPART) = -1
      ENDIF
      ENDIF
C
C     Check for a wild character at then end of the part (only)
      IF (CPARTF(IPART)(ILENF(IPART):ILENF(IPART)).EQ.'@') THEN
      IF (.NOT.LFIND) THEN
      IWILD(IPART) = 2
      ILENF(IPART) = ILENF(IPART) - 1
      ENDIF
      ENDIF
C
      ENDIF
 100  CONTINUE
C
C     Look for parts that don't match.  Go to next set if so.
      DO 120 I=1,6
C     Is this part set?
      IF (ILENF(I).GE.0) THEN
C
C     Does the compression part end in a wild character (@)?
      IF (IWILD(I).EQ.1) THEN
C     Yes.  Is the part less than the specified length?
      IF (ILENI(I).LT.ILENF(I)-1) GO TO 200
C     Is the compression part zero length?
      IF (ILENF(I).EQ.0) GO TO 200
C     Do the parts match up to the wild character?
      IF (INDEX(CPARTI(I)(1:ILENI(I)),CPARTF(I)(2:ILENF(I))).EQ.0)
     * GO TO 200
C
C     Does the compression part end in a wild character (@)?
      ELSE IF (IWILD(I).EQ.2) THEN
C     Yes.  Is the part less than the specified length?
      IF (ILENI(I).LT.ILENF(I)) GO TO 200
C     Is the compression part zero length?
      IF (ILENF(I).EQ.0) GO TO 200
C     Do the parts match up to the wild character?
      IF (CPARTI(I)(1:ILENF(I)).NE.CPARTF(I)(1:ILENF(I))) GO TO 200
C
      ELSE
C     Parts must match identically.  Are the lengths the same?
      IF (ILENI(I).NE.ILENF(I)) GO TO 200
      IF (ILENI(I).GT.0) THEN
C     Are the parts the same?
      IF (CPARTI(I)(1:ILENI(I)).NE.CPARTF(I)(1:ILENI(I))) GO TO 200
      ENDIF
      ENDIF
C
      ENDIF
C
 120  CONTINUE
C
C
C     Match Made!!
      JCOMP = ISKM
      GO TO 800
C
 200  CONTINUE
      JNEXT = IFLTAB(NEXT)
C     ENDLOOP
      GO TO 60
C
C
 800  CONTINUE
      IF ((JCOMP.EQ.2).OR.(JCOMP.EQ.3)) THEN
      NPRE = NPR
      IF (IBASE.EQ.1) THEN
      DBASE = BASE
      LDBASE = .TRUE.
      ENDIF
      IF (NBYTES.EQ.2) THEN
      LDHIGH = .TRUE.
      ELSE
      LDHIGH = .FALSE.
      ENDIF
      ENDIF
C
      IF (LFIND) NPRE = JNEXT
C
      IF (ICPOS.NE.0) IFLTAB(KCOMP) = 0
C
      IF (MLEVEL.GE.10) WRITE (MUNIT, 820) JCOMP, JNEXT
 820  FORMAT (T5,'----DSS--- Exiting zgetci6;  JCOMP:',I4,
     * ',   Position:',I4)
C
      RETURN
C
      END

