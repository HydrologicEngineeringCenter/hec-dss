
      SUBROUTINE zcopyRecord6(ifltabFrom, ifltabTo, cpathFrom, cpathTo,
     * ISTAT)
C
C     Depricated.  Use zcopyRecord (C version) instead.
      implicit none
C
C
      INTEGER ifltabFrom(*), ifltabTo(*)
      CHARACTER cpathFrom*(*), cpathTo*(*)
      CHARACTER CPATH*392
      INTEGER ISTAT, iversion, jversion,zdssVersion
C
      INTEGER NDATA, IDTYPE, NPATH, IOFSET, JCOMP, IPLAN, NPREC
      integer npathFrom, npathTo, JSTAT
      INTEGER ICDESC(6), NCDESC
      CHARACTER CDTYPE*5, CDATE*12, CTIME*4, CUNITS*12, CTYPE*12
      LOGICAL LFOUND, LGETDOB, LFILDOB, LQUAL, LQREAD, LCOORDS
      LOGICAL LBASEV, LDHIGH
      DOUBLE PRECISION COORDS(3)
      REAL BASEV
      CHARACTER CPART(6)*64, CEPART*64, CALPHA*4
      INTEGER NPART(6), INUMB
      INTEGER M2IHM
      INTEGER JULS, ISTIME, JULE, IETIME, INTL, NVALS, N, I
      INTEGER NORD, NCURVE, IHORIZ, NCOORDS, KVALS, IBDATE
      CHARACTER C1UNIT*26, C1TYPE*26, C2UNIT*26, C2TYPE*26
C
      INTEGER KLABEL
      PARAMETER (KLABEL=101)
      CHARACTER CLABEL(KLABEL)*25
      LOGICAL LABEL
C
      INTEGER KIHEAD, KCHEAD
      PARAMETER (KIHEAD=1000, KCHEAD=100)
      INTEGER IIHEAD(KIHEAD), ICHEAD(KCHEAD)
      INTEGER NIHEAD, NCHEAD, NUHEAD

      INTEGER munit, mlvl
C
      INTEGER KLBUFF, NIBUFF, KSBUFF, KDBUFF, KJBUFF
C      PARAMETER (KLBUFF=50000,NIBUFF=500)
      PARAMETER (KLBUFF=500000,NIBUFF=5000)
C	PARAMETER (KLBUFF=12000,NIBUFF=500)
      PARAMETER (KSBUFF=(KLBUFF/2))
      PARAMETER (KDBUFF=(KLBUFF/2))
      PARAMETER (KJBUFF=(KLBUFF/3))
C
      REAL BUFF(KLBUFF)
      REAL SBUFF1(KSBUFF), SBUFF2(KSBUFF)
      DOUBLE PRECISION DBUFF(KDBUFF)
      INTEGER ILBUFF(KLBUFF), INTBUF(NIBUFF)
      INTEGER IBUFF1(KSBUFF), IBUFF2(KSBUFF)
      INTEGER JBUFF1(KJBUFF), JBUFF2(KJBUFF), JBUFF3(KJBUFF)
      EQUIVALENCE (ILBUFF,BUFF)
C     Be sure BUFF and SBUFF1 are equivalenced as follows!!
      EQUIVALENCE (BUFF(1), SBUFF1), (BUFF(KSBUFF+1), SBUFF2)
      EQUIVALENCE (BUFF(1), IBUFF1), (BUFF(KSBUFF+1), IBUFF2)
      EQUIVALENCE (BUFF(1), JBUFF1), (BUFF(KJBUFF+1), JBUFF2)
      EQUIVALENCE (BUFF((KJBUFF*2)+1), JBUFF3)
      EQUIVALENCE (BUFF, DBUFF)
C
C
C
      ISTAT = 0
C
C
      CALL CHRLNB(cpathFrom, npathFrom)
      CALL CHRLNB(cpathTo, npathTo)
      CPATH = cpathTo(1:npathTo)
      call zquery('munit', CALPHA, munit)
      call zquery('mlvl', CALPHA, mlvl)

       if ((mlvl.ge.10).and.(munit.gt.0)) then
         write(munit, 1) cpathFrom(1:npathFrom), cpathTo(1:npathTo)
 1       Format('---Enter zcopyRecord',/,' Pathname from: ',A,/,
     *          ' Pathname to:   ',A)
      endif

      DO 30 I=1,6
         CPART(I) = ' '
 30   CONTINUE
      CALL ZUFPN (CPART(1), NPART(1), CPART(2), NPART(2),
     * CPART(3), NPART(3), CPART(4), NPART(4), CPART(5), NPART(5),
     * CPART(6), NPART(6), cpathFrom, npathFrom, ISTAT)
      CDATE = CPART(4)(1:12)
      CEPART = CPART(5)
C
      call zdataType (ifltabFrom, cpathFrom, IDTYPE, ISTAT)
      IF (ISTAT.ne.0) THEN
        if ((mlvl.ge.2).and.(munit.gt.0)) then
            write(munit, 10) cpathFrom(1:npathFrom)
 10         Format('---zcopyRecord, Record Not Found: ',A)
         endif
         ISTAT = -2
         RETURN
      ENDIF
C
      if ((IDTYPE.EQ.20).and.(zdssVersion(ifltabTo).eq.6)) then
C       DATA_TYPE_LOCATION - we don't copy this record to version 6
        if ((mlvl.ge.4).and.(munit.gt.0)) then
            write(munit, 9) cpathFrom(1:npathFrom)
 9          Format('---zcopyRecord; Location Info is not copied ',
     *             'to version 6: ',A)
         endif
         ISTAT = 0
         return
      endif
C
      CUNITS = ' '
      CTYPE = ' '
      if ((IDTYPE.ge.100).and.(IDTYPE.lt.110)) then
C
        CDATE = ' '
        CTIME = ' '
        ISTAT = 1
        IF (IDTYPE.EQ.105) THEN
           LGETDOB = .TRUE.
        ELSE
           LGETDOB = .FALSE.
        ENDIF
        LCOORDS = .TRUE.
       CALL zrrtsi(ifltabFrom,cpathFrom(1:npathFrom),CDATE,CTIME,KDBUFF,
     *  NVALS, LGETDOB, LFILDOB, SBUFF1, DBUFF, IBUFF2, .TRUE., LQREAD,
     *  CUNITS, CTYPE, INTBUF, NIBUFF, NUHEAD, IOFSET, JCOMP,
     *  COORDS, ICDESC, LCOORDS, ISTAT)
        IF ((ISTAT.LT.0).OR.(ISTAT.GT.10)) THEN
           if ((mlvl.ge.2).and.(munit.gt.0)) then
              write(munit, 11) ISTAT, cpathFrom(1:npathFrom)
 11           Format('--zcopyRecord Loc b, Error reading record: ',
     *               I3,2x,A)
           endif
           RETURN
        ELSE IF (ISTAT.EQ.4) THEN
            call zquery('empty', calpha, inumb)
            if (inumb.eq.0) then
               if ((mlvl.ge.2).and.(munit.gt.0)) then
                  write(munit, 14) cpathFrom(1:npathFrom)
14                 Format(' ---zcopyRecordLoc c, Empty Record ',
     *                   '(not copied): ',A)
               endif
               return
            endif
        ENDIF
C
        IF (LCOORDS) THEN
           NCOORDS = 3
           NCDESC = 6
        ELSE
           NCOORDS = 0
           NCDESC = 0
        ENDIF
        IPLAN = 0
        JCOMP = 0
        BASEV = 0.0
        LBASEV = .FALSE.
        LDHIGH = .FALSE.
        NPREC = 0
        CALL CHRLNB(cpathTo, NPATH)
        CALL ZUFPN (CPART(1), NPART(1), CPART(2), NPART(2),
     *    CPART(3), NPART(3), CPART(4), NPART(4), CPART(5), NPART(5),
     *    CPART(6), NPART(6), cpathTo, npathTo, ISTAT)
       ISTAT = 1
       CALL ZGINTL6(INTL, CPART(5), N, ISTAT)
       ISTIME = INTL
       N = M2IHM (ISTIME, CTIME)
        IF (IDTYPE.NE.101) THEN
            CALL DATJUL(CPART(4), JULS, ISTAT)
            IF (ISTAT.NE.0) RETURN
            CALL ZOFSET6 (JULS, ISTIME, INTL, 2, IOFSET)
            CALL JULDAT(JULS, 4, CDATE, N)
       ELSE
          CDATE = CPART(4)(1:12)
       ENDIF

      CALL ZPATH (CPART(1), CPART(2), CPART(3), CDATE, CEPART,
     * CPART(6), CPATH, NPATH)

        CALL zsrtsi (ifltabTo, CPATH(1:NPATH), CDATE, CTIME, NVALS,
     *  LFILDOB, SBUFF1, DBUFF, IBUFF2, LQREAD, CUNITS, CTYPE,
     *  INTBUF, NUHEAD, COORDS, NCOORDS, ICDESC, NCDESC,
     *  IPLAN, JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
        IF ((ISTAT.LT.0).OR.(ISTAT.GT.10)) THEN
           if ((mlvl.ge.2).and.(munit.gt.0)) then
              write(munit, 12) ISTAT, CPATH(1:NPATH)
 12           Format(' ---zcopyRecordLoc d, Error writing record: ',
     *                      I3,2x,A)
            endif
           RETURN
        ENDIF

C
      else if ((IDTYPE.ge.110).and.(IDTYPE.lt.200)) then
C
         IF (IDTYPE.EQ.115) THEN
           LGETDOB = .TRUE.
           KVALS = KJBUFF/2
        ELSE
           LGETDOB = .FALSE.
           KVALS = KJBUFF
        ENDIF
        JULS = 0
        ISTIME = -2
        JULE = 0
        IETIME = 0
        IBDATE = 0
        LQUAL = .TRUE.
        LCOORDS = .TRUE.
        CALL ZRITSI(ifltabFrom,cpathFrom(1:npathFrom),JULS,ISTIME,JULE,
     *  IETIME, LGETDOB, LFILDOB, JBUFF2, BUFF, DBUFF, KVALS, NVALS,
     *  IBDATE, JBUFF3, LQUAL, LQREAD, CUNITS, CTYPE, INTBUF, NIBUFF,
     *  NUHEAD, COORDS, ICDESC, LCOORDS, 0, ISTAT)
        IF ((ISTAT.LT.0).OR.(ISTAT.GT.10)) THEN
           if ((mlvl.ge.2).and.(munit.gt.0)) then
            write(munit, 13) ISTAT, cpathFrom(1:npathFrom)
 13         Format(' ---zcopyRecord Loc e, Error reading record: ',
     *                      I3,2x,A)
           endif
           RETURN
        ENDIF
C
        IF (LCOORDS) THEN
           NCOORDS = 3
           NCDESC = 6
        ELSE
           NCOORDS = 0
           NCDESC = 0
        ENDIF
        CALL ZUFPN (CPART(1), NPART(1), CPART(2), NPART(2),
     *  CPART(3), NPART(3), CPART(4), NPART(4), CPART(5), NPART(5),
     *  CPART(6), NPART(6), cpathTo, npathTo, ISTAT)
        CALL ZPATH (CPART(1), CPART(2), CPART(3), CDATE, CEPART,
     *   CPART(6), CPATH, NPATH)
        CALL ZSITSI (ifltabTo, CPATH(1:NPATH), JBUFF2, BUFF, DBUFF,
     *  LFILDOB, NVALS, IBDATE, JBUFF3, LQREAD, CUNITS, CTYPE,
     *  INTBUF, NUHEAD, COORDS, NCOORDS, ICDESC, NCDESC, 1,
     *  ISTAT)
        IF ((ISTAT.LT.0).OR.(ISTAT.GT.10)) THEN
           if ((mlvl.ge.2).and.(munit.gt.0)) then
              write(munit, 18) ISTAT, CPATH(1:NPATH)
 18           Format(' ---zcopyRecord Loc f, Error writing record: ',
     *                      I3,2x,A)
           endif
           RETURN
        ENDIF
C
      else if ((IDTYPE.ge.200).and.(IDTYPE.lt.300)) then
C
        IF (IDTYPE.EQ.200) THEN
           LGETDOB = .FALSE.
           KVALS = KLBUFF
        ELSE
           LGETDOB = .TRUE.
           KVALS = KLBUFF/2
        ENDIF

        CALL ZRPDI (ifltabFrom, cpathFrom(1:npathFrom), NORD, NCURVE,
     *  IHORIZ, C1UNIT, C1TYPE, C2UNIT, C2TYPE, BUFF, DBUFF,
     *  LGETDOB, KVALS, NVALS, CLABEL, KVALS, LABEL,
     *  INTBUF, NIBUFF, NUHEAD, ISTAT)

        IF ((ISTAT.LT.0).OR.(ISTAT.GT.10)) THEN
           if ((mlvl.ge.2).and.(munit.gt.0)) then
              write(munit, 15) ISTAT, cpathFrom(1:npathFrom)
 15           Format(' ---zcopyRecord Loc g, Error reading record: ',
     *                      I3,2x,A)
           endif
           RETURN
        ENDIF
C
        IPLAN = 0
        CALL ZSPDI (ifltabTo, cpathTo(1:npathTo), NORD, NCURVE,
     *  IHORIZ, C1UNIT, C1TYPE, C2UNIT, C2TYPE, BUFF, DBUFF,
     *  LGETDOB, CLABEL, LABEL, INTBUF, NUHEAD, IPLAN, ISTAT)
        IF ((ISTAT.LT.0).OR.(ISTAT.GT.10)) THEN
           if ((mlvl.ge.2).and.(munit.gt.0)) then
            write(munit, 16) istat, CPATH(1:NPATH)
 16         Format(' ---zcopyRecord Loc h, Error writing record: ',
     *                      I3,2x,A)
           endif
           RETURN
        ENDIF
C
      else
C
        IPLAN = 0

        CALL ZREADX(ifltabFrom, cpathFrom(1:npathFrom), IIHEAD, KIHEAD,
     *  NIHEAD, ICHEAD, KCHEAD, NCHEAD, INTBUF, NIBUFF, NUHEAD, ILBUFF,
     *  KLBUFF, NVALS, IPLAN, LFOUND)
        IF (.NOT.LFOUND) THEN
           if ((mlvl.ge.2).and.(munit.gt.0)) then
              write(munit, 17) cpathFrom(1:npathFrom)
 17           Format(' ---zcopyRecord Loc i, Error reading record: ',A)
            endif
           ISTAT = -1
           RETURN
        ENDIF
C
       CALL ZWRITEX(ifltabTo, cpathTo(1:npathTo), npathTo,IIHEAD,NIHEAD,
     *  ICHEAD, NCHEAD, INTBUF, NUHEAD, ILBUFF, NVALS, IDTYPE,
     *  IPLAN, ISTAT, LFOUND)
       IF (ISTAT.ne.0) THEN
           if ((mlvl.ge.2).and.(munit.gt.0)) then
              write(munit, 22) istat, cpathTo(1:npathTo)
 22           Format(' ---zcopyRecord Loc j, Error writing record: ',
     *                      I3,2x,A)
            endif
           RETURN
        ENDIF
C
      endif
c
      IF ((ISTAT.GE.0).AND.(ISTAT.LT.4)) THEN
          IF (zdssVersion(ifltabFrom).EQ.zdssVersion(ifltabTo)) THEN
            call zgetInfo(ifltabFrom, cpathFrom(1:npathFrom), ILBUFF,
     *                    istat)
            if (istat.eq.0) then
              call zputInfo(ifltabTo, CPATH, ILBUFF, istat)
              if (istat.ne.0) then
                 write (munit,*)'Pathname ', CPATH(1:npathTo)
              endif
            else
              write (munit,*)'Pathname ', cpathFrom(1:npathFrom)
            endif
          endif
          ISTAT = 0
      ENDIF
C
      RETURN
      END

