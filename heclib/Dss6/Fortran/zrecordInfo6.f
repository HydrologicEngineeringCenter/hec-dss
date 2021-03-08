      SUBROUTINE zrecordInfo6(IFLTAB, CPATH, idataType, iversion,
     *  numberVals, logicalNumberVals,
     *  numberVals1, numberInternalHead, numberUserHead,
     *  isizeAllocated, lastWriteSecs, iprecisionTS,
     *  ioffset, ivalSize, iqualSize,
     *  numberCurvesPD, numberOrdinatesPD, ipdValueSize, iaxisFlagPD,
     *  lboolLabelsPD, iprecisionPD, istatus)

      implicit none
C
C     Get internal record sizes
C     Purpose is to know how large to allocate arrays
C
C     ISTATUS is returned as:
C        0  Record found, ok to read/write
C       -1  Record not found
C
C     Written by Bill Charley, HEC, 2014.
C
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*)
      character CSCRAT*30
      INTEGER idataType, iversion, numberVals, logicalNumberVals
      INTEGER numberVals1, numberInternalHead, numberUserHead
      INTEGER isizeAllocated, lastWriteSecs, iprecisionTS
      INTEGER ioffset, ivalSize, iqualSize, ipdValueSize
      INTEGER numberCurvesPD, numberOrdinatesPD, iaxisFlagPD
      INTEGER lboolLabelsPD, iprecisionPD, istatus
      integer IPDVALSIZE,JUL,IERR,NIHEAD
C
      INTEGER JHEAD, JDATA, NPATH
      integer IHR,IMIN,ISEC, IDAYSECS
      INTEGER IIHEAD(50)
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      IF (MLEVEL.GE.11) THEN
         CALL CHRLNB(CPATH, NPATH)
         WRITE (MUNIT, 20) IFLTAB(KUNIT), CPATH(1:NPATH)
 20      FORMAT (T6,'-----DSS---Debug: Enter recordInfo6',/,
     *   T5,'Unit:',I5,'  Path: ',A)
      ENDIF
C
C
C     Read in the main information block
      CALL zrdinf6(IFLTAB, CPATH, JHEAD, JDATA, istatus)
      IF (istatus.NE.0) GO TO 900

      iqualSize = 0
      ivalSize = 0
      ipdValSize = 0
      lboolLabelsPD = 0
C
C     Get the sizes of the headers and data blocks
      numberInternalHead = INFO(NPPWRD+KINIHE)
      numberUserHead = INFO(NPPWRD+KINUHE)
      numberVals  = INFO(NPPWRD+KINDAT)
      numberVals1 = INFO(NPPWRD+KINDAT)
      logicalNumberVals  = INFO(NPPWRD+KILNDA)
      isizeAllocated = INFO(NPPWRD+KINDAT)
      iversion = INFO(NPPWRD+KIVER)
      iprecisionTS = INFO(NPPWRD+KIPREC)
      iprecisionPD = INFO(NPPWRD+KIPREC)
      IF (INFO(NPPWRD+KIQUAL).GT.0) iqualSize = 1
      idataType = INFO(NPPWRD+KITYPE)
      CSCRAT = ' '
      CALL HOLCHR (INFO(NPPWRD+KIDATE), 1, NDATEC, CSCRAT, 1)
      CALL DATJUL(CSCRAT(1:NDATEC), JUL, IERR)
      IF (IERR.eq.0) then
        CSCRAT = ' '
        CALL HOLCHR (INFO(NPPWRD+KITIME), 1, NTIMEC, CSCRAT, 1)
        READ (CSCRAT,30) IHR,IMIN,ISEC
 30     FORMAT (I2,1X,I2,1X,I2)
        IDAYSECS = (IHR * 3600) + (IMIN * 60) + ISEC
C       Adjust julian to Jan 01, 1970
        JUL = JUL - 25568   !  25568 is julian day for 01Jan1970
        lastWriteSecs = (JUL * 86400) + IDAYSECS
      endif

      NIHEAD = INFO(NPPWRD+KINIHE)
      IF (numberInternalHead.GT.0) then
        IF (numberInternalHead.GT.50) numberInternalHead = 50
        CALL zgtrec6(IFLTAB, IIHEAD, numberInternalHead,
     *      INFO(NPPWRD+KIAIHE), .TRUE.)
        IF (IFLTAB(KSTAT).NE.0) GO TO 900
        if ((idataType.ge.100).and.(idataType.lt.200)) then
            if (idataType.lt.105) then
                ivalSize = 1
            else
                ivalSize = 2
            endif
            ioffset = IIHEAD(1)
        else if ((idataType.ge.200).and.(idataType.lt.300)) then
            if (idataType.lt.205) then
                ipdValueSize = 1
            else
                ipdValueSize = 2
            endif
            numberOrdinatesPD = IIHEAD(1)
            numberCurvesPD    = IIHEAD(2)
            iaxisFlagPD       = IIHEAD(3)
            if (NIHEAD.GT.12) lboolLabelsPD = 1
        else
        endif
      endif

C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) ISTATUS
 820  FORMAT (T6,'-----DSS---Debug: Exit recordInfo6, Status:',I4)
      RETURN
C
 900  CONTINUE
      IF (ISTATUS.EQ.0) ISTATUS = -1
      GO TO 800
C
C
      END

