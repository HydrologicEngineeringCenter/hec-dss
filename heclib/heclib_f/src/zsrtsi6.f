      SUBROUTINE zsrtsi6 (IFLTAB, CPATH, CDATE, CTIME, NVALS,
     * LDOUBLE, SVALUES, DVALUES, JQUAL, LQUAL, CUNITS, CTYPE,
     * IUHEAD, NUHEAD, COORDS, NCOORDS, ICDESC, NCDESC,
     * IPLAN, JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
C
C
C
C     Z-Store Regular interval Time-Series data
C     Data is stored according to the
C     time window set from CDATE/CTIME and NVALS.
C
C     Input:
C        IFLTAB:  Working DSS array used in zopen6 call.  Must be
C                 be dimensioned as INTEGER with 1200 words
C        CPATH:   Pathname of the data to be stored.  The "D" part
C                 is ignored.  CPATH sould be declared as
C                 CHARACTER*80.
C        CDATE:   Beginning date of the time window.  This may be
C                 a standard military style date (e.g. 01MAR74).
C        CTIME:   Beginning time of the time window.  This must be
C                 a standard 24 hour clock time (e.g. 1630).  Any
C                 time offset is implied by this date and time.
C                 CTIME should be declared as CHARACTER*4.
C        NVALS:   The number of data values to store.  This number
C                 defines the end of the time window.
C        VALUES:  The data to be stored.
C        CUNITS:  Character string containing the units of the data.
C                 CUNITS must be declared CHARACTER*8
C        CTYPE:   Character string containing the type of the data
C                 (e.g., PER-AVER).  CTYPE must be declared CHARACTER*8
C        IPLAN:   A flag indicating if existing data should be written
C                 over or not:
C                 IPLAN = 0  Always replace data.
C                 IPLAN = 1  Only replace missing data.
C                 IPLAN = 2  Write regardless, even if all missing data.
C                 IPLAN = 3  If a record is all missing, do not write it
C                            and delete it from disk if it exists.
C                 IPLAN = 4  Do not allow a missing input data to
C                            replace a valid data piece.
C     Output:
C        ISTAT:   Integer status parameter, indicating the
C                 successfullness of the data storage.
C                 ISTAT = 0  All ok.
C                 ISTAT = 4  All missing data flags - no data stored
C                            unless IPLAN set to 2.
C                 ISTAT > 9  Illegal call to zsrts6
C
C     Written by Bill Charley at HEC, 1987.
C
C
C     DIMENSION STATEMENTS

      PARAMETER (KIHEAD = 24)
      INTEGER IIHEAD(KIHEAD)
C
C     Argument Dimensions
      CHARACTER CPATH*(*), CTYPE*(*), CUNITS*(*), CDATE*(*), CTIME*(*)
      INTEGER IFLTAB(*), IUHEAD(*), JQUAL(*)
      INTEGER IPLAN, ISTAT, NVALS
      LOGICAL LQUAL, LQREAD, LREADD, LUPRTS, LBASEV, LBASE, LDHIGH
      LOGICAL LHIGH, LPROT2, LDOUBLE
C
      REAL SVALUES(*)
      DOUBLE PRECISION DVALUES(*), COORDS(*)
C
      INTEGER NCDESC, NCOORDS, ICDESC(*)
C
C     Local Dimensions
      CHARACTER CTPATH*260
      CHARACTER CPART(6)*64, CTSPAT*392
      CHARACTER CDATE1*12, CDATE2*12, CTIME1*4, CTIME2*4, CSCRAT*20
      INTEGER NPART(6)
      LOGICAL LFOUND, LERR, LFILDOB
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssdc.h'
C
C
C
C
      CALL zinqir6 ( IFLTAB, 'UNIT',   CDATE1, IFUNIT)
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) THEN
      CALL CHRLNB(CPATH, N)
      IF (N.LE.0) N = 1
      WRITE (MUNIT,20) IFUNIT, CPATH(1:N), IPLAN
 20   FORMAT (T10,'----- Enter zsrts6 for unit',I5,' -----',/,
     *        T10,'      Path: ',A,/,
     *        T10,'      IPLAN: ',I4)
         IF (MLEVEL.GE.8) THEN
         NUMB = 10
         IF (NVALS.LT.NUMB) NUMB = NVALS
         JUMB = NVALS - NUMB
         IF (LDOUBLE) THEN
           DO 4 I=1,NUMB
              WRITE (MUNIT, 5) I, DVALUES(I)
 5            FORMAT (T5, I4,3X,F16.6)
 4         CONTINUE
           IF (JUMB.GT.0) THEN 
             DO 6 I=JUMB,NVALS
                WRITE (MUNIT, 5) I, DVALUES(I)
6            CONTINUE
           ENDIF 
         ELSE
           DO 7 I=1,NUMB
              WRITE (MUNIT, 5) I, SVALUES(I)
 7         CONTINUE
           IF (JUMB.GT.0) THEN 
             DO 8 I=JUMB,NVALS
                WRITE (MUNIT, 5) I, SVALUES(I)
 8           CONTINUE
           ENDIF 
         ENDIF
      ENDIF
      ENDIF
C
C
      NSTART = 1
      ISTAT = 0
      LPROT2 = LPROTC
      IQUAL = 0
C
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zsrtsx6', 0,
     * IFLTAB, ' ', 0, ' ',0)
C
C
C     Are we in a read only state?
      IF (IFLTAB(KREADO).EQ.1) GO TO 987
C
C     Unform the pathname
      DO 30 I=1,6
      CPART(I) = ' '
 30   CONTINUE
      CALL CHRLNB (CPATH, NPATH)
      IF ((NPATH.GT.MXPATH).OR.(NPATH.LE.1)) GO TO 900
      IF (CPATH(1:1).NE.'/') GO TO 900
      CALL zufpn (CPART(1), NPART(1), CPART(2), NPART(2),
     * CPART(3), NPART(3), CPART(4), NPART(4), CPART(5), NPART(5),
     * CPART(6), NPART(6), CPATH, NPATH, IERR)
      IF (IERR.NE.0) GO TO 900
C
      IF (MLEVEL.GE.7) THEN
      WRITE (MUNIT, 40) CPATH(1:NPATH), NPATH
 40   FORMAT (' Pathname: ',A,/,' Length:',I3)
      IF (MLEVEL.GE.8) THEN
      CSCRAT = 'ABCDEF'
      WRITE (MUNIT, 50) (CSCRAT(I:I), NPART(I), CPART(I),I=1,6)
 50   FORMAT (T10, 'Lengths and contents of the pathname parts:',
     * 6(/,T14,'Part ',A,' Length: ',I2,'   Contents: ',A))
      WRITE (MUNIT,55) JCOMP, LDHIGH, NPREC, LBASEV, BASEV
 55   FORMAT (T5,'Compression:',I3,', Size:',L2,', Precision:',I3,
     * ', Base:',L2,2X,F8.2)
      ENDIF
      ENDIF
C
C     If the compression flag is set to zero (use default compression),
C     see if the file has this data type to be compressed
      BASE = BASEV
      LBASE = LBASEV
      LHIGH = LDHIGH
      NPRE = NPREC
      IF (JCOMP.EQ.0) THEN
      IF (IFLTAB(KCOMPN).GT.0) THEN
      CALL zgetci6 (IFLTAB, CPART, IFCOMP, BASE, LBASE, LHIGH, NPRE)
      IF (MLEVEL.GE.8) THEN
      WRITE (MUNIT,255) IFCOMP, LHIGH, NPRE, LBASE, BASE
 255  FORMAT (T5,'File Compression:',I3,', Size:',L2,', Precision:',I3,
     * ', Base:',L2,2X,F8.2)
      ENDIF
      ELSE
      IFCOMP = 0
      ENDIF
      ENDIF
C
C     Determine the time interval, in minutes
      I = 1
      CALL zgintl6 (INTL, CPART(5), NUMDAT, I)
      IF (I.LT.0) GO TO 910
C
C     Be sure the correct E part was given
      CPART(5) = ' '
      I = 2
      CALL zgintl6 (INTL, CPART(5), NUMDAT, I)
C
C     Check that a positive number of data was requested
      IF (NVALS.LE.0) GO TO 920
C
C     Check for 'Pattern' Time Series, data that
C     has no specific time associated with it
C     (such as a unit hydrograph or mean maximum daily temperatures)
C
      CALL UPCASE (CPART(4))
      IF (CPART(4)(1:3).EQ.'TS-') THEN
         IF (LDOUBLE) GO TO 995
         CALL zsrtpa6 (IFLTAB, CPATH, SVALUES, NVALS, CUNITS, CTYPE,
     *                IUHEAD, NUHEAD, IPLAN, ISTAT)
         GO TO 800
      ENDIF
C
C     Get time window
      CALL DATJUL ( CDATE, JULS, IERR)
      IF (IERR.NE.0) GO TO 930
      ISTIME = IHM2M (CTIME)
      IF (ISTIME.EQ.0) THEN
      JULS = JULS - 1
      ISTIME = 1440
      ENDIF
C
C     Check for an illegal starting time
      IF ((ISTIME.LT.0).OR.(ISTIME.GT.1440)) GO TO 940
C     If the time is not on the standard boundaries, adjust it
C     and determine the time offset, in minutes.
      CALL zofset6 (JULS,ISTIME,INTL,1,IOFSET)
C     Compute the ending time from the number of values
      I = INCTIM (INTL, 0, NVALS-1, JULS, ISTIME, JULE, IETIME)
C
      IF (MLEVEL.GE.7) THEN
      WRITE (MUNIT,80)INTL,NVALS
 80   FORMAT (T10,'Time Window set.  Interval:',I6,'  Number of',
     * ' data values:',I7)
      CALL JULDAT (JULS, 1, CDATE1, NDATE1)
      CALL JULDAT (JULE, 1, CDATE2, NDATE2)
      I = M2IHM (ISTIME, CTIME1)
      I = M2IHM (IETIME, CTIME2)
      WRITE (MUNIT,90) CDATE1(1:NDATE1), CTIME1, JULS, ISTIME,
     * CDATE2(1:NDATE2), CTIME2, JULE, IETIME, IOFSET
 90   FORMAT (T10,'Starting date and time:  ',A,2X,A,'  (',I7,I5,')',/,
     *        T10,'Ending   date and time:  ',A,2X,A,'  (',I7,I5,')',/,
     *        T10,'Input time offset:',I7)
      WRITE (MUNIT,95) LTOL, TOL
 95   FORMAT (T10,'Tolerance Set: ',L1,',   Tolerance:',F12.9)
      ENDIF
C
C     Comupute a flag to send to zmovbk6 based upon IPLAN
      IFLAG = 1
      IF (IPLAN.EQ.1) THEN
      IFLAG = -1
      ELSE IF (IPLAN.EQ.4) THEN
      IFLAG = -4
      ENDIF
C
C
C     Obtain the date of the first block
      CALL zbegdt6 (JULS, INTL, IYR, IMON, IDAY, IBLOCK, IFLTAB(KVERNO))
      JULSD = IYMDJL (IYR, IMON, IDAY)
C
C     Get the date of the last block
      CALL zbegdt6 (JULE, INTL, JYR, JMON, JDAY, IBLOCK, IFLTAB(KVERNO))
      JULAST = IYMDJL (JYR, JMON, JDAY)
C
C     Lock the file for multiple user access
      CALL zmultu6 ( IFLTAB, .TRUE., .TRUE.)
C
C     Loop, reading data blocks, changing the D (date)
C     part of the pathname each time
C
      CDATE1 = ' '
 100  CONTINUE
C
C     Get the new D (Date) part
      CALL YMDDAT (IYR, IMON, IDAY, 104, CDATE1, NDATE1, IERR)
      IF (IERR.NE.0) GO TO 950
      JULSD = IYMDJL (IYR, IMON, IDAY)
C
      CTSPAT = ' '
      CALL zpath (CPART(1), CPART(2), CPART(3), CDATE1(1:NDATE1),
     * CPART(5), CPART(6), CTSPAT, NTSPAT)
C
C
C     Get the time of the next data block
      CALL zincbk6 (IBLOCK, JUL, IYR, IMON, IDAY)
C
      IF (INTL.EQ.10080) THEN
C     Weekly data.  Set so that the data is always on
C     Saturday at 2400 hours.
      NDAY = IDAYWK (JUL)
      JUL = JUL - NDAY + 1
      NDAY = IDAYWK (JULSD)
      JULSD = JULSD - NDAY + 1
      ENDIF
C
C
C     See if we need to read this record (does the time window
C     completely span this record, and IPLAN = 0)
      IF ((JULS.LT.JULSD).AND.(JULE.GT.JUL).AND.(IPLAN.EQ.0).AND.
     * (.NOT.LQPBIT).AND.(.NOT.LTOL)) THEN
      LREADD = .FALSE.
      IF (MLEVEL.GE.8) WRITE (MUNIT,210)
 210  FORMAT(T10,'Bypass Read because time window spans entire block')
      ELSE
      LREADD = .TRUE.
      ENDIF
C
C     Check to see if the data record already exists.
      LFILDOB = .FALSE.
      LWRITE = .TRUE.
      CALL zcheck6 (IFLTAB, CTSPAT, NTSPAT, JHEAD, JDATA, LFOUND)
      IF (MLEVEL.GE.8) THEN
      WRITE (MUNIT,220) LFOUND, CTSPAT
 220  FORMAT(T10,'After zcheck, Record found:',L2,/,T10,'Pathname: ',A)
      IF (LFOUND) WRITE (MUNIT,230) JDATA, JHEAD
 230  FORMAT (T10,'Number of data:',I5,'  Header length:',I4)
      ENDIF
      IF (IFLTAB(KSTAT).NE.0) GO TO 961
C
      IF (LFOUND) THEN
C
C     Get Information Block
      IADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL zgtrec6 (IFLTAB, INFO, NINFO+NPPWRD, IADD, .TRUE.)
      IF (IFLTAB(KSTAT).NE.0) GO TO 961
C     Double Check that this is the correct pathname
      IF (NTSPAT.NE.INFO(KINPAT)) GO TO 960
      CALL HOL2CH (INFO(KIPATH), CTPATH, NPMWRD)
      IF (CTSPAT(1:NTSPAT).NE.CTPATH(1:NTSPAT)) GO TO 960
C
C     Is the data set single or double?
      IF (IFLTAB(KDTYPE).EQ.105) THEN
         LFILDOB = .TRUE.
      ENDIF
C
C     Get the internal header size
      JIHEAD = INFO(NPPWRD+KINIHE)
C
C     Is the record compressed?
      IRCOMP = INFO(NPPWRD+KICOMP)
C     Does the record use data quality flags?
      IF (INFO(NPPWRD+KIQUAL).GT.0) THEN
      LQREAD = .TRUE.
      ELSE
      LQREAD = .FALSE.
      ENDIF
C
C
      ELSE
C     No Record found
      IRCOMP = 0
      LREADD = .FALSE.
      LQREAD = .FALSE.
      JIHEAD = 0
      ENDIF
C
C
C     Determine the compression scheme to use (if any)
      IF (JCOMP.EQ.0) THEN
C     The compression scheme of the data read overides the file default
      IF (IRCOMP.EQ.0) THEN
      IACOMP = IFCOMP
      ELSE
      IACOMP = IRCOMP
      ENDIF
      ELSE
      IACOMP = JCOMP
      ENDIF
C
C     Quality and data compression are mutually exclusive;
C     If LQUAL is true, turn off data compression
      IF (LQUAL.OR.LQREAD) IACOMP = 0
C
C     Data compression is not currently supported for doubles
      IF (LDOUBLE) IACOMP = 0
C
C     Be sure we have enough buffer space to use data compression
      IF ((IACOMP.NE.0).AND.(NUMDAT.GT.KSBUFF)) THEN
      IF (MLEVEL.GE.2) WRITE (MUNIT, 238) CTSPAT(1:NTSPAT)
 238  FORMAT (' -----DSS---zsrtsx6:  Unable to compress data with',
     * ' this time interval;',/,' Pathname: ',A)
      IACOMP = 0
      ENDIF
C
C     Determine if we can use the subroutine zuprts6 to store
C     the data.  This subroutine is faster than the alternative
C     and is meant to store a small amount of data in an
C     already existing recrod:
      LUPRTS = .TRUE.
C     Can't use if the record does not exist
      IF (.NOT.LFOUND) LUPRTS = .FALSE.
C     Can't use if data compression is on
      IF (IRCOMP.GT.0) LUPRTS = .FALSE.
      IF (IACOMP.GT.0) LUPRTS = .FALSE.
C     Can't use if we need to rearange data
      IF (LQUAL.NEQV.LQREAD) LUPRTS = .FALSE.
C     Don't use if there are any doubles involved
      IF (LDOUBLE.OR.LFILDOB) LUPRTS = .FALSE.
C     Can't use if we need to update (check missing data)
      IF (IPLAN.NE.0)  LUPRTS = .FALSE.
C     Can't use if we need to change the user header
      IF (JHEAD.NE.NUHEAD) THEN
      IF (NUHEAD.NE.-1) LUPRTS = .FALSE.
      ENDIF
C     Can't use if internal header size different
      IF (JIHEAD.NE.KIHEAD) LUPRTS = .FALSE.
C     Can't use if we need to compare data quality bits
      IF (LQPBIT.AND.LQREAD) LUPRTS = .FALSE.
C
C     Do we need to read data currently on the disk?
      IF ((LREADD).AND.(.NOT.LUPRTS)) THEN
C     If compression is on, and we are not replacing the entire
C     block, read the record and uncompress the data set
      IF (IRCOMP.GT.0) THEN
C     Read the compression header
      NDCH = INFO(NPPWRD+KINCHE)
      CALL zgtrec6 (IFLTAB, IDCH, NDCH, INFO(NPPWRD+KIACHE), .TRUE.)
C     Read the compressed data
      NCDA = INFO(NPPWRD+KINDAT)
      CALL zgtrec6 (IFLTAB, SBUFF2, NCDA, INFO(NPPWRD+KIADAT),
     * .FALSE.)
C     Uncompress the data
      NELMS = -1
      NDCH = NDCH * 2
      CALL DUREAL (SBUFF1, KSBUFF, 1, NELMS, SBUFF2, NCDA, KSBUFF,
     * IDCH, NDCH, IST)
      IF (IST.NE.0) GO TO 980
C     NEED TO GET OTHER COMPRESSION INFORMATION HERE !!!!!!!!!!!!!!!!!!
C     (EG BASE NPREC, ETC!!!)
      ELSE
C
C     Record exists, and is not compressed
C     Read the data for zmovbk6
      NDA = INFO(NPPWRD+KINDAT)
C     Make sure we have enough space
      IF (NDA.GT.KSBUFF) GO TO 925
      CALL zgtrec6 (IFLTAB, BUFF, NDA, INFO(NPPWRD+KIADAT), .FALSE.)
      IF (IFLTAB(KSTAT).NE.0) GO TO 961
      IF (LFILDOB) THEN
         IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(BUFF, NDA)
      ENDIF
      ENDIF
      ENDIF
C
C
C     Compute the logical amount of data to be stored
      NDATA = NOPERS (INTL, 0, JULSD, 0, JUL, 0)
C     NLDATA is the number of logical data values (same regardless
C     if the data is compressed or doubles, or whatever)
      NLDATA = NDATA
C     NTDATA is the total size of the complete data array, and
C     includes any space for quality flags, double, or compression
      NTDATA = NDATA
      IF (LDOUBLE.OR.LFILDOB) THEN
C        If doubles are used alocate 2 words for each value
         NTDATA = NLDATA * 2
      ENDIF
      IF ((LQUAL).OR.(LQREAD)) THEN
C        Add an additional array for quality
         NTDATA = NTDATA + NLDATA
      ENDIF
C
C     If single, store each quality flag after its data value
C     If double, store the entire set of quality flags
C     after the data set (too hard to intersperce between values)
C
C     If no data was read, fill BUFF with missing data flags (-901.0)
 240  CONTINUE
      IF (.NOT.LFOUND) THEN
         IF (LQUAL) THEN
C           BUFF, DBUFF, and ILBUFF are all equivalenced
            IF (LDOUBLE) THEN
               LFILDOB = .TRUE.
               DO 245 I=1,NLDATA
                  DBUFF(I) = -901.0
 245           CONTINUE
               ISTART = NLDATA * 2
               DO 246 I=1,NLDATA
                  ILBUFF(ISTART+I) = 0
 246           CONTINUE
            ELSE
               DO 250 I=1,NTDATA,2
                  BUFF(I) = -901.0
                  ILBUFF(I+1) = 0
250            CONTINUE
            ENDIF
         ELSE
            IF (LDOUBLE) THEN
               LFILDOB = .TRUE.
               DO 260 I=1,NDATA
                  DBUFF(I) = -901.0
 260           CONTINUE
            ELSE
               DO 270 I=1,NDATA
                  BUFF(I) = -901.0
 270           CONTINUE
            ENDIF
         ENDIF
      ELSE
C
C        The record exists, and data was read.
C
C        Valid data length writes are as follows
C        (S=Single, D=Double, Q=Quality Flags)
C        On File     Incoming      Write to File
C  1.       S           S              S
C  2.       D           D              D
C  3.       S           D              D
C  4.       D           S              D
C  5.       S Q         S Q            S Q
C  6.       D Q         D Q            D Q
C  7.       S           S Q            S Q
C
C        All others (quite a few) are invalid
C        Thus, we cannot add quailty to a double
C        (must be there already), nor can we subtract
C        quailty from a data set.  These will raise an error
C
C        Are there any quality flags involved?
         IF ((.NOT.LQUAL).AND.(.NOT.LQREAD)) THEN
C           No.  Are we writing the same word length?
            IF (LDOUBLE.EQV.LFILDOB) THEN
C              Yes.  Case 1 or 2 (Single to Single or
C              Double to Double)
C              Data is ok as is.  We don't do anything
            ELSE
C              No.  Different lenghts.  Double wins out.
C              If the file is double, and the incoming
C              data is single, the block is ok already.
             IF (LFILDOB) THEN
C                 Case 4.  Keep as double.  Block OK
               ELSE
C                 Case 3.  Convert single to double
C                 At this point the data in the file is single,
C                 and the incoming data is double.  Expand the
C                 file data to double.
                  DO 300 I=NDATA,1,-1
                     DBUFF(I) = DBLE(BUFF(I))
 300              CONTINUE
C                 Now we have converted the file data to double
                  LFILDOB = .TRUE.
               ENDIF
            ENDIF
C
         ELSE
C
C           Data quality flags are involved
            IF (LQUAL.AND.LQREAD) THEN
               IF (LDOUBLE.NEQV.LFILDOB) THEN
C                 Invalid. Cannot convert word size with quality
                  GO TO 974
               ELSE
C                  Case 5 and 6.  Block OK as is.
               ENDIF
            ELSE
C           At this point the only exception is case 7.
C             We will add quality to a single data set w/o flags.
               IF ((LQUAL).AND.(.NOT.LQREAD).AND.
     *           (.NOT.LDOUBLE).AND.(.NOT.LFILDOB)) THEN
                  DO 310 I=NDATA,1,-1
                     ILBUFF(I*2) = 0
                     ILBUFF(I*2-1) = ILBUFF(I)
 310              CONTINUE
               ELSE IF ((.NOT.LQUAL).AND.(LQREAD).AND.
     *           (.NOT.LDOUBLE).AND.(.NOT.LFILDOB)) THEN
C                 Writting a data set without quality to one that has it
C                 Just use zeros as quality
               ELSE
C                 Either LQUAL or LQREAD is true (not both).
C                 We are doing something invalid.  Examples include
C                 adding quailty to a double, removing quailty,
C                 or converting word size where quality is involved.
                  GO TO 974
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C
C     Move data values from VALUES into BUFF
      IF (.NOT.LUPRTS) THEN
      CALL zmovbk6 (SVALUES, DVALUES, LDOUBLE, LFILDOB,
     * JQUAL, LQUAL, LQREAD, NSTART, NVALS, JULS, ISTIME,
     * INTL, BUFF, DBUFF, ILBUFF, NDATA, JULSD, JSTAT, IFLAG)
      IF (JSTAT.NE.0) THEN
      ISTAT = JSTAT
      IF (ISTAT.GE.10) GO TO 970
      ENDIF
      ENDIF
C
C     Compress the data, if a compression scheme is used
      IF (IACOMP.GT.0) THEN
C
      IF (IACOMP.GT.5) GO TO 980
      IF ((IACOMP.EQ.2).OR.(IACOMP.EQ.3)) THEN
      IF ((NPRE.LT.-6).OR.(NPRE.GT.6)) GO TO 985
      IF (LHIGH) THEN
      NBYTES = 2
      ELSE
      NBYTES = 0
      ENDIF
      ENDIF
C
      IF (MLEVEL.GE.9) WRITE (MUNIT,320)IACOMP, LHIGH, NPRE, LBASE, BASE
 320  FORMAT (T5,'Compression Set, Scheme:',I3,', Size:',L2,
     * ',  Precision:',I3,', Base:',L2,2X,F8.2)
      NELMS = NLDATA
      CALL DCREAL (SBUFF1, NELMS, IACOMP, BASE, LBASE, NBYTES, NPRE,
     * SBUFF2, NRB, KSBUFF, IDCH, NCHEAD, KDCH, IST)
      IF (MLEVEL.GE.9) WRITE (MUNIT, 330) NRB, NCHEAD, IST
 330  FORMAT (T5,'Compression;  NRB:',I6,'  NCHEAD:',I6,', STATUS:',I6)
C
C     If a combination compression scheme was unsuccessful,
C     try compressing with just the repeat scheme.
      IF (IST.NE.0) THEN
      IF ((IACOMP.EQ.3).OR.(IACOMP.EQ.5)) THEN
      IACOMP = 1
      NELMS = NLDATA
      CALL DCREAL (SBUFF1, NELMS, IACOMP, BASE, LBASE, NBYTES, NPRE,
     * SBUFF2, NRB, KSBUFF, IDCH, NCHEAD, KDCH, IST)
      IF (MLEVEL.GE.9) WRITE (MUNIT, 340) NRB, NCHEAD, IST
      ENDIF
      ENDIF
C
      N = NRB + NCHEAD
      IF ((IST.NE.0).OR.(NRB.LE.0).OR.(N.GE.NLDATA)) THEN
      IACOMP = 0
      NCHEAD = 0
      IF (IST.EQ.0) IST = -1
      IF (MLEVEL.GE.2) WRITE (MUNIT,340) IST, CTSPAT(1:NTSPAT)
 340  FORMAT (' -----DSS---zsrtsx6;  WARNING:  Unable to Compress',
     * ' Data.  Status:',I6,/,' Pathname: ',A)
      ELSE
      NTDATA = NRB
      ENDIF
      ELSE
      NCHEAD = 0
      ENDIF
C
C     Add the Header Information
      IIHEAD(1) = IOFSET
      CSCRAT = CUNITS
      CALL CHRHOL (CSCRAT, 1, 8, IIHEAD(2),  1)
      CSCRAT = CTYPE
      CALL CHRHOL (CSCRAT, 1, 8, IIHEAD(4), 1)
      IIHEAD(6) = IWTZONE
      CALL CHRHOL (CWTZONE, 1, 24, IIHEAD(7), 1)
C	Add coordinate info
      DO 350 I=13,24
         IIHEAD(I) = 0
 350	CONTINUE
      IF (NCOORDS.GT.0) THEN
      CALL zmovwd6(COORDS(1), IIHEAD(13), NCOORDS*2)
      NDES = MIN(NCDESC, 6)
      DO 355 I=1,NDES
         IIHEAD(I+18) = ICDESC(I)
 355  CONTINUE
      ENDIF
C
C     Set the compression value and quality flag to be used in the write
      ICOMP = IACOMP
      IQUAL = 0
      IF ((LQUAL).OR.(LQREAD)) IQUAL = 1
C
C     If we are updating, and NUHEAD is -1, do not write over the
C     the existing user header
      IF (NUHEAD.GE.0) THEN
      NUH = NUHEAD
      ELSE
      NUH = JHEAD
      ENDIF
C
C     Now reserve space in the DSS file
      IF (LDOUBLE.OR.LFILDOB) THEN
      ITYPE = 105
      ELSE
      ITYPE = 100
      ENDIF
C
C     Check to see if we should write the record if all missing
      IF ((IPLAN.EQ.3).AND.(IACOMP.LE.0).AND.(.NOT.LFOUND).AND.
     *    (IQUAL.EQ.0)) THEN
         IF (LDOUBLE) THEN
            DO 360 I=1,NLDATA
               IF (DBUFF(I).NE.-901.0) GO TO 390
 360        CONTINUE
         ELSE
            DO 370 I=1,NLDATA
               IF (BUFF(I).NE.-901.0) GO TO 390
 370        CONTINUE
         ENDIF
C        Data is all missing, bypass
         IF (MLEVEL.GE.3) WRITE (MUNIT, 380) CTSPAT(1:NTSPAT)
 380        FORMAT (' -----DSS---zsrts6: All values set to missing',/,
     *      ' Pathname: ',A)
         GO TO 600
      ENDIF
C
C
C     Actual write
 390  CONTINUE
      IFLTAB(KBSADD) = IFLTAB(KFSIZE)
      IF (LFOUND) THEN
C
C     Be sure that we have permission to write this record
      IF (LPROT2) THEN
      IF (MLEVEL.GE.2) WRITE (MUNIT, 400) CTSPAT(1:NTSPAT)
 400  FORMAT (' -----DSS---zsrtsx6:  Write Protection for Existing',
     * ' Record (no data written)',/,
     * ' Pathname: ',A)
      GO TO 600
      ENDIF
C
      IF (LQUAL.NEQV.LQREAD) THEN
         IF (LQUAL) THEN
            IF (MLEVEL.GE.3) WRITE (MUNIT, 410) CTSPAT(1:NTSPAT)
 410        FORMAT (' -----DSS---zsrts6: Caution:  Writing flags to an',
     *      ' existing data set that does not have flags.',/,
     *      ' Pathname: ',A)
         ELSE
            IF (LQPBIT) THEN
C              If the protection bit flag is on, we cannot write a
C              record without flags to one that has flags
               IF (MLEVEL.GE.2) WRITE (MUNIT, 400) CTSPAT(1:NTSPAT)
               GO TO 600
            ENDIF
            IF (MLEVEL.GE.3) WRITE (MUNIT, 420) CTSPAT(1:NTSPAT)
 420        FORMAT (' -----DSS---zsrts6: Caution:  Writing data without',
     *      ' flags to an existing data set that has flags.',/,
     *      ' Pathname: ',A)
         ENDIF
      ENDIF
C
C     If we are updating data in a record, we don't need to rewrite
C     the entire record (i.e., not changing the number of data)
      IF (.NOT.LUPRTS) THEN
      CALL zowrit6 (IFLTAB, CTSPAT, NTSPAT, KIHEAD, NCHEAD, NUH,
     * NTDATA)
      ENDIF
      ELSE
C     Write a new record to the DSS file
      CALL znwrit6 (IFLTAB, CTSPAT, NTSPAT, KIHEAD, NCHEAD, NUH,
     * NTDATA)
      ENDIF
      IF (IFLTAB(KSTAT).NE.0) GO TO 961
C
      ITYPE = 0
C
C     Now store the header arrays
      CALL zptrec6(IFLTAB, IIHEAD, KIHEAD, INFO(NPPWRD+KIAIHE), .FALSE.)
      IF (NCHEAD.GT.0) CALL zptrec6 (IFLTAB, IDCH, NCHEAD,
     * INFO(NPPWRD+KIACHE), .FALSE.)
      IF (NUHEAD.GT.0) CALL zptrec6 (IFLTAB, IUHEAD, NUHEAD,
     * INFO(NPPWRD+KIAUHE), .FALSE.)
C
C     Now store the data array
C
      IF (LUPRTS) THEN
C     We are only updating a portion of the record (already in the file)
C     Only store the data passed to us (faster than zmovbk6)
      CALL zuprts6 (IFLTAB, JULS, ISTIME, INTL, JULSD, NSTART,
     * NDATA, NVALS, SVALUES, JQUAL, LQUAL, IPLAN, LERR)
      IF (LERR) GO TO 990
      ELSE
      IF (IACOMP.LE.0) THEN
      IPOS = 1
      ELSE
      IPOS = KSBUFF + 1
      ENDIF
         IF (LDOUBLE.OR.LFILDOB) THEN
            IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DBUFF(IPOS), NTDATA)
            CALL zptrec6 (IFLTAB, DBUFF(IPOS), NTDATA,
     *                   INFO(NPPWRD+KIADAT),.FALSE.)
         ELSE
            CALL zptrec6 (IFLTAB, BUFF(IPOS), NTDATA,
     *                   INFO(NPPWRD+KIADAT),.FALSE.)
         ENDIF
      ENDIF
C
C
C
C     Check to see if we should delete the record if all missing
      IF ((IPLAN.EQ.3).AND.(IACOMP.LE.0).AND.(LFOUND).AND.
     *    (IQUAL.EQ.0)) THEN
         NLDATA = INFO(NPPWRD+KILNDA)
         IF (.NOT.LUPRTS) THEN
            IF (LDOUBLE) THEN
               DO 460 I=1,NLDATA
                  IF (DBUFF(I).NE.-901.0) GO TO 500
 460           CONTINUE
            ELSE
               DO 470 I=1,NLDATA
                  IF (BUFF(I).NE.-901.0) GO TO 500
 470           CONTINUE
            ENDIF
         ELSE
C        Data to store is all missing.  Read record to see if it is all
C        missing
            IADD = INFO(NPPWRD+KIADAT)
            NTDATA = INFO(NPPWRD+KINDAT)
            IF (LDOUBLE) THEN
               CALL zgtrec6 (IFLTAB, DBUFF, NTDATA, IADD, .FALSE.)
               IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DBUFF, N)
               DO 480 I=1,NLDATA
                  IF (DBUFF(I).NE.-901.0) GO TO 500
 480           CONTINUE
            ELSE
               CALL zgtrec6 (IFLTAB, BUFF, NTDATA, IADD, .FALSE.)
               DO 490 I=1,NLDATA
                  IF (BUFF(I).NE.-901.0) GO TO 500
 490           CONTINUE
            ENDIF
         ENDIF
         IF (MLEVEL.GE.3) WRITE (MUNIT, 380) CTSPAT(1:NTSPAT)
         CALL zdelet6 (IFLTAB, CTSPAT, NTSPAT, LFOUND)
         GO TO 600
      ENDIF
C
C     Are we done storing the data?
 500  CONTINUE
      LWRITE = .FALSE.
      IF (MLEVEL.GE.3) THEN
      IF (L80COL) THEN
      WRITE ( MUNIT,520) CTSPAT(1:NTSPAT)
 520  FORMAT(' --ZWRITE: ',A)
      ELSE
      WRITE (MUNIT,540)IFLTAB(KUNIT),INFO(NPPWRD+KIVER),CTSPAT(1:NTSPAT)
 540  FORMAT(' -----DSS---ZWRITE Unit',I5,'; Vers.',I5,':',2X,A)
      ENDIF
      ENDIF
      IF (IFLTAB(KSTAT).NE.0) GO TO 961
C
 600  CONTINUE
      IF (NSTART.GT.NVALS) GO TO 800
      IF (JUL.GT.JULAST) GO TO 800
C
C     Clear some space in the buffer arrays
      CALL zbdump6 (IFLTAB, 2)
C
C     Need to store more data, loop back to 100
      GO TO 100
C
C
C     Done.  Exit zsrts6.
 800  CONTINUE
C     Unlock the file and dump all buffers
      LWRITE = .FALSE.
      LTOL = .FALSE.
      IWTZONE = -1
      CWTZONE  = ' '
      CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
C
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NVALS, ISTAT
 820  FORMAT(T10,'----- Exit zsrts6, Number of data values ',
     * 'stored:',I7,',  Status:',I4,/)
*      CALL FLUSH(MUNIT)                                         Mu
C
      RETURN
C
C
C     --- ERROR STATEMENTS ---
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,901) NPATH, CPATH(1:NPATH)
 901  FORMAT (/,' *****DSS*** zsrts6:  ERROR  - ILLEGAL PATHNAME',
     * ' OR PATHAME LENGTH',/,' Length: ',I5,/,' Pathname: ',A,/)
      ISTAT = 24
      GO TO 990
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) CPART(5), INTL, CPATH(1:NPATH)
 911  FORMAT (/,' *****DSS*** zsrts6:  ERROR  - NON-STANDARD TIME',
     * ' INTERVAL',/,' Interval: ',A,2X,I8,/,' Pathname: ',A,/)
      ISTAT = 12
      GO TO 990
C
 920  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,921) NVALS, CPATH(1:NPATH)
 921  FORMAT (/,' *****DSS*** zsrts6:  ERROR  - NUMBER OF VALUES',
     * ' TO STORE IS LESS THAN 1',/,' NVALS: ',I8,/,' Pathname: ',A,/)
      ISTAT = 11
      GO TO 990
C
 925  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,926) NVALS, CPATH(1:NPATH)
 926  FORMAT (/,' *****DSS*** zsrts6:  ERROR  - Insufficent',
     * ' internal memory to store data',/,
     * ' NVALS: ',I8,/,' Pathname: ',A,/)
      ISTAT = 16
      GO TO 990
C
 930  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,931) CDATE, JULS, CPATH(1:NPATH)
 931  FORMAT (/,' *****DSS*** zsrts6:  ERROR  - ILLEGAL STARTING',
     * ' DATE SPECIFIED',/,' Date: ',A,3X,I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 940  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,941) CTIME, ISTIME, CPATH(1:NPATH)
 941  FORMAT (/,' *****DSS*** zsrts6:  ERROR  - ILLEGAL STARTING',
     * ' TIME SPECIFIED',/,' Time: ',A,3X,I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 950  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,951) CDATE1, IYR, IMON, IDAY,
     * CPATH(1:NPATH)
 951  FORMAT(/,' *****DSS*** zsrts6:  ERROR  - UNABLE TO GENERATE',
     * ' BLOCK DATE',/,' Date: ',A,3X,3I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 960  CONTINUE
      NP = INFO(KINPAT)
      CALL zerror6 (IFLTAB, 11, 'zsrtsx6', 0, IADD, CTSPAT, NTSPAT,
     * CTPATH, NP)
C
 961  CONTINUE
      ISTAT = IFLTAB(KSTAT)
C
 970  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,971) ISTAT, CPATH(1:NPATH)
 971  FORMAT (/,' *****DSS*** zsrts6:  ERROR  - UNABLE TO ',
     * ' STORE DATA',/,' Status: ',I8,/,' Pathname: ',A,/)
      GO TO 990
C
 974  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,975) CPATH(1:NPATH)
 975  FORMAT (/,' *****DSS*** zsrts6:  ERROR  - Attempting ',
     * ' to store',/,' a different type of data set than'
     * ' what already exists.',/,' Pathname: ',A,/)
      ISTAT=511
      GO TO 990
C
 980  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 981) IACOMP
 981  FORMAT (/,' *** ERROR:  zsrtsx6;  Illegal Data compression scheme',
     * /,' Setting:',I6,';  Min Allowed: 0,  Max: 5')
      ISTAT = 51
      GO TO 990
C
 985  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 986) NPRE
 986  FORMAT (/,' *** ERROR:  zsrtsx6;  Illegal Data Compression',
     * ' Precision Value'/,' Value:',I6,';  Min Allowed: -6,  Max: 6')
      ISTAT = 52
      GO TO 990
C
 987  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 988) CPATH(1:NPATH)
 988  FORMAT (' -----DSS---zsrtsx6:  ERROR;  File has Read Access Only',
     * /,' Pathname: ',A)
      ISTAT = 30
      GO TO 990
C
 995  CONTINUE
      WRITE (MUNIT, 996) CPATH(1:NPATH)
 996  FORMAT (' -----DSS---zsrtsx6:  ERROR;  Unable to store time ',
     * 'pattern data in double precision.',
     * /,' Pathname: ',A)
      ISTAT = 30
      GO TO 990
C
C
 990  CONTINUE
      IF (MLEVEL.GE.7) WRITE (MUNIT,991) ISTAT
 991  FORMAT(T10,'----- Exit zsrts6, Error return; Status:',I4)
      GO TO 800
C
      END

