      SUBROUTINE makedsscatalog (CDSSFI, IFLTAB, CINSTR, NFOUND, IUNIT)
C
C     deprecated.  Catalog "files" are no longer used.
C     Please do not write new code against this.
C
C     Create a catalog scratch file from the instructions in CINSTR
C
      CHARACTER CDSSFI*(*)
      INTEGER IFLTAB(*)
      CHARACTER CINSTR*(*)
      INTEGER NFOUND
      INTEGER IUNIT
C
      LOGICAL LOPEN, LCATLG, LCOND, LCONDO, LABREV, LNEW, LCATCD
      INTEGER NRECS, NFILE, ICDUNT
      LOGICAL LSORT, LRETRY, LDUM
      CHARACTER CSCRATCH*150, CINST2*150, ctemp*1
C
      DATA LSORT /.TRUE./
C
C
C     Instructions contain following key words:
C        NEW - force a new one to be made
C        FULL - Full catalog (as opposed to abbrievated)
C        COND - Create a condensed catalog
C     Followed by any A=..., B=... instructions
C          new full a=scioto
C
	LOPEN = .FALSE.
	LCOND = .FALSE.
	LRETRY = .FALSE.
	LNEW = .FALSE.
      LABREV = .TRUE.
      ICDUNT = 0
	CALL ZINQIR (IFLTAB, 'NREC', ctemp, NFILE)
C
      CSCRATCH = CINSTR
      IEQUAL = INDEX(CINSTR, '=')
      IF (IEQUAL.GT.1) THEN
          CSCRATCH(IEQUAL-1:) = ' '
          CINST2 = CINSTR(IEQUAL-1:)
      ELSE
          CINST2 = ' '
      ENDIF
C
      I = INDEX(CSCRATCH, 'NEW')
      IF (I.GT.0) LNEW = .TRUE.
      I = INDEX(CSCRATCH, 'FULL')
      IF (I.GT.0) LABREV = .FALSE.
      I = INDEX(CSCRATCH, 'COND')
      IF (I.GT.0) THEN
         LCOND = .TRUE.
         ICDUNT = 13
      ENDIF
C
      CALL ZOPNCA (CDSSFI, 12, .TRUE., LOPEN, LCATLG,
     +             ICDUNT, LCOND, LCONDO, LCATCD, NRECS)
C
      IF (INDEX(CSCRATCH,'?').GT.0) THEN
         NFOUND = NRECS
         IF (LOPEN) CLOSE (UNIT=12)
         IF (LCOND) THEN
            CLOSE (UNIT=ICDUNT)
            IF (.NOT.LCATCD) NFOUND = 0
         ENDIF
         RETURN
      ENDIF
C
      IF (.NOT.LOPEN) THEN
         GO TO 50
      ENDIF
C
C     Do we need to update the current catalog file?
      CALL ZINQIR (IFLTAB, 'NREC', ctemp, NFILE)
C
C     If we did not find any pathnames, return
      IF (NFILE.EQ.0) GO TO 900
C
C     Hokey fix to force a new catalog -
C     set NFOUND to -1 on entry!
      IF (NFOUND.EQ.-1) LNEW = .TRUE.
      IF (LCOND) THEN
         IF (.NOT.LCONDO) GO TO 900
         IF (.NOT.LCATCD) LNEW = .TRUE.
      ENDIF
C
C
 40   CONTINUE
C     Do we need to create a new catalog?
      IF ((.NOT.LCATLG).OR.(NRECS.NE.NFILE).OR.(LRETRY).OR.(LNEW)) THEN
         CALL ZCAT (IFLTAB, 12, ICDUNT, 0, ' ', LABREV , LSORT,
     +              LCATCD, NRECS)
         CALL FLUSH(12)
         REWIND 12
      ENDIF
C
C     If we are doing a condensed catalog, don't do any more,
C     but return with the information if we were successful
      IF (LCOND) THEN
         CLOSE (UNIT=12)
         CLOSE (UNIT=ICDUNT)
         IF (LCATCD) THEN
            NFOUND = NRECS
         ELSE
            NFOUND = 0
         ENDIF
         GO TO 800
      ENDIF
C
C     If we did not find any pathnames, return
      IF (NRECS.EQ.0) THEN
C        We cannot write to the catalog.  Try using a scracth file.
         CLOSE (UNIT=12)
         LOPEN = .FALSE.
      ELSE
C        Are there any additional instructions?
C        If not, just use the regular catalog.
         CALL CHRLNB(CINST2, N)
         IF (N.EQ.0) THEN
            NFOUND = NRECS
            GO TO 750
         ENDIF
      ENDIF
C
C
 50	CONTINUE
C     Instructions, make a scratch catalog using them
      CSCRATCH = ' '
      CALL TEMPNAME (CSCRATCH, 13)
      OPEN (UNIT=13, FILE=CSCRATCH, IOSTAT=ISTAT)
      IF (ISTAT.NE.0) GO TO 900
C     CALL CHMODF (CSCRATCH, 438, ISTAT)                                u
C
      IF (LOPEN) THEN
C        Now make the scratch catalog using the instructions given
         CALL ZCAT (IFLTAB, 13, 0, 12, CINST2, .TRUE., .FALSE.,
     +              LDUM, NFOUND)
C
         IF ((NFOUND.LE.0).AND.(.NOT.LRETRY)) THEN
            CALL CHRLNB(CINST2, N)
            IF (N.EQ.0) THEN
               LRETRY = .TRUE.
               GO TO 40
            ENDIF
         ENDIF
      ELSE
C
C        Write the catalog directly to a scratch file
         CALL ZCAT (IFLTAB, 13, 0, 0, CINST2, .TRUE., LSORT,
     +              LDUM, NFOUND)
      ENDIF
C
C
C     We are using a scratch catalog
      CALL FLUSH(12)
      CALL FLUSH(13)
      CLOSE (UNIT=12)
      IUNIT = 13
      REWIND (UNIT=IUNIT)
      GO TO 800
C
C
 750  CONTINUE
C     Use the regular catalog
      IUNIT = 12
      CALL FLUSH(IUNIT)
      REWIND (UNIT=IUNIT)
      GO TO 800
C
C
 800  CONTINUE
      RETURN
C
 900  CONTINUE
      CLOSE (UNIT=12)
      NFOUND = 0
      IUNIT = 0
      GO TO 800
C
      END
      SUBROUTINE closescratchdsscatalog (IUNIT)
C
C     Closes the scratch dss catalog file
C     If it is unit 13, it is deleted
C
      IF (IUNIT.EQ.13) THEN
         CLOSE (UNIT=IUNIT, STATUS='DELETE', IOSTAT=IS)
      ELSE
         CLOSE (UNIT=IUNIT, IOSTAT=IS)
      ENDIF
C
      RETURN
      END

