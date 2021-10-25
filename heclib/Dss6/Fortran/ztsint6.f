      SUBROUTINE ztsint6 (IFLTAB, CA, NA, CB, NB, CC, NC, CF, NF, JULIS,
     1 ISITIM, JULE, IETIME, INTIN, INTOUT, IHEAD, NH, WRKSPC, VALUES,
     2 NVALS, CUNITS, CTYPE, ISTAT)
C
C
C     Given pathname components, a date/time window and time
C     interval, return interpolated data in users array.
C                      ------------
C
C     Time window may cross the data blocks actually stored.
C
C
C
C     Input:
C        IFLTAB:  Working DSS array used in zopen6 call.  Must be
C                 be dimensioned as INTEGER with 1200 words
C        CA-CF :  Pathname parts ins array form (for harris integer*6).
C        NA-NF :  Respective pathname part lenghts.
C        JULIS :  The julian startin date of the time window.
C        ISITIM:  The starting time of the time window, in minutes.
C        JULE  :  The julian ending date of the time window.
C        IETIME:  The ending time of the time window, in minutes
C        INTIN :  The interval of the data in the array  WRKSPC, in min
C                 5 minutes is the minimum interval.
C        IHEAD :  This is not currently being used.
C        NH    :  This is also not used.
C        WRKSPC:  Work space - area used to actually read data from dss.
C                 Dimension WRKSPC to be at least as large as the number
C                 of data in the time window specified.
C        NVALS :  Dimension limit of array  VALUES.
C
C
C     Output:
C        NVALS:   The number of interpolated data retrieved.  Note that
C                 this is also and input argument.
C        VALUES:  The interpolated data retrieved.
C        CUNITS:  Character string returning the units of the data.
C                 CUNITS must be declared CHARACTER*8
C        CTYPE:   Character string returning the type of the data
C                 (e.g., PER-AVER).  CTYPE must be declared CHARACTER*8
C        ISTAT:   Integer status parameter, indicating the
C                 successfullness of the retrieval.
C                 ISTAT = 0  All ok.
C                 ISTAT = 1  Some missing data (still ok)
C                 ISTAT = 2  Missing data blocks, but some data found
C                 ISTAT = 3  Combination of 1 and 2 (some data found)
C                 ISTAT = 4  No data found, although a pathname was read
C                 ISTAT = 5  No pathname(s) found
C                 ISTAT > 9  Illegal call to zrrts6
C
      INCLUDE 'zdssmz.h'
C
C
C
C        Parameters ----------------------------------------------------
      CHARACTER*(*) CA, CB, CC, CF
      CHARACTER*(*) CTYPE, CUNITS
      CHARACTER CE*16
      INTEGER IFLTAB(*), IHEAD(*)
      REAL WRKSPC(*), VALUES(*)
      CHARACTER PATH*400
C
C       Local Variables -----------------------------------------------
C
      CHARACTER CJTYPE(4)*8
      CHARACTER CDATE*20, CTIME*4
C
      DATA CJTYPE/'PER-AVER','PER-CUM ','INST-VAL','INST-CUM'/
C
C ----------------------------------------------------------------------
C
C
C
C     Copy starting date and time so they can be changed.
      JULS = JULIS
      ISTIME = ISITIM
C
      IF (MLEVEL.LT.9) GO TO 60
      WRITE(MUNIT,30)JULS,ISTIME,JULE,IETIME,INTIN,INTOUT
 30   FORMAT(T20,'----- ENTERING ztsint6-----------------',
     */,T10,'STARTING JULIAN DATE, TIME = ',3X,2I8,
     */,T10,'ENDING   JULIAN DATE, TIME = ',3X,2I8,
     */,T10,'TIME INTERVAL IN, OUT = ',2I7)
      WRITE(MUNIT,40)CA(1:4),CB(1:4),CC(1:4),CF(1:4),NA,NB,NC,NF
 40   FORMAT(T10,'FIRST 4 CHARS AND CHAR LENGTH OF CA, CB, CC, CF',
     */,T15,4(A4,4X),/,T15,4(I4,4X))
 60   CONTINUE
C
C     Check that time does not start or end on 00 mins (illegal time).
      IF (ISTIME.NE.0) GO TO 70
          ISTIME = 1440
          JULS = JULS - 1
 70   IF (IETIME.NE.0) GO TO 75
          IETIME = 1440
          JULE = JULE - 1
 75   CONTINUE
C
C     Initialize work space.
      DO 77 I=1,35
      WRKSPC(I) = -901.0
 77   CONTINUE
C
      NSD = 1
      ISTAT = 0
C
C     Calculate the number of periods asked for.
      NED = NOPERS(INTOUT,0,JULS,ISTIME,JULE,IETIME) + 1
      IF (NED.GT.NVALS) GO TO 910
C
C
C     Get alpha value of interval.
      I = 2
      CALL zgintl6 ( INTIN, CE, NUMDAT, I)
      IF (I.LT.0) GO TO 900
C
C     Get start date and time of first data to be read.
C
      INCBAK = MAX(INTOUT,INTIN)
      ILARGE = 1440
      INCBAK = MAX(INCBAK,ILARGE)
      IDUMMY = INCTIM(INCBAK,0,-2,JULS,ISTIME,JULSD,ISTIM)
C
      CALL zofset6(JULSD,ISTIM,INCBAK,1,IOFS)
      IF (MLEVEL.GE.8) WRITE (MUNIT, 79) JULSD, ISTIM, INCBAK, IOFS
 79   FORMAT (T8,'ztsint6: After zofset6, JULSD:',I8,',  ISTIM:',I8,/,
     * T10,'INCBAK:',I8,',  IOFS:',I8)
      INCFWD = MIN (INTOUT,INTIN)
      IDUMMY = INCTIM (INCFWD, 0, 1, JULSD, 0, JUL, ISTIM)
      IF ((ISTIM.EQ.1440).AND.(INCFWD.NE.1440)) THEN
      JUL = JUL + 1
      ISTIM = 5
      ENDIF
C
C
C     Determine the number of periods (a block may have an
C     incorrect amount of data.)
C     NDW = Number in WRKSPC, ND = number in VALUES array.
      NDW = NOPERS(INTIN,0,JULSD,0,JULE,IETIME) + 2
      ND = NOPERS(INTOUT,0,JULSD,0,JULE,IETIME)
C
      I = M2IHM(ISTIM,CTIME)
      CALL JULDAT (JUL,104,CDATE,N)
C     Form pathname.
      CALL zfpn(CA,NA,CB,NB,CC,NC,CDATE,9,CE,8,CF,NF,PATH,NPATH)
C     Read data from DSS file.
      CALL zrrts(IFLTAB,PATH(1:NPATH),CDATE,CTIME,NDW,WRKSPC(36),
     +CUNITS,CTYPE,IWOFS,ISTAT)
C
      IF (ISTAT.GE.9) GO TO 920
      IF (ISTAT.GE.4) GO TO 800
C
C     Determine data type.
C
      DO 80 I=1,4
      IFLAG = I
      IF (CTYPE .EQ. CJTYPE(IFLAG)) GO TO 85
 80   CONTINUE
C
C     Could not recognize type - Fatal Error.
      GO TO 930
C
 85   CONTINUE
C
C
      OFSETW = FLOAT(IWOFS)/FLOAT(INTIN)
C
      IF (MLEVEL.GE.8) WRITE (MUNIT, 86) CTYPE, IFLAG, IWOFS, INTIN,
     * OFSETW
 86   FORMAT ('  ztsint6:  Data Type -',A,'-   Flag:',I4,/,
     * T10,'IWOFS:',I8,',  INTIN:',I8,',   OFSETW:',F12.5)
C
      CALL zintbk6 (VALUES, NSD, NED, JULS, ISTIME, INTOUT, ND,
     * WRKSPC, NDW, JULSD, INTIN, OFSETW, ISTAT, IFLAG)
      IF (ISTAT.GE.9) GO TO 920
C
      NVALS = NED
C
C     Locate any missing data, and indicate in istat.
      DO 100 I=1,NVALS
      IF (VALUES(I).NE.-901.0.AND.VALUES(I).NE.-902.0)GO TO 110
 100  CONTINUE
      ISTAT=4
       GO TO 800
C
C     Locate first and last data.
 110  IFIRST = I
C
      DO 120 J=1,NVALS
      I = NVALS - J + 1
 120  CONTINUE    
C
      IF(VALUES(I).NE.-901.0.AND.VALUES(I).NE.-902.0) GO TO 130
      GO TO 140
 130  CONTINUE
      K = IFIRST
C
C
 140  DO 150 I=K,NVALS
      IF (VALUES(I).EQ.-901.0)GO TO 160
 150  CONTINUE
       GO TO 800
C
 160  IF(ISTAT.EQ.2)ISTAT=3
      IF(ISTAT.EQ.0)ISTAT=1
C
 800  CONTINUE
      IF(MLEVEL.GE.9)WRITE(MUNIT,810)NVALS
 810  FORMAT(T20,'----- EXITING ztsint6, NO DATA READ =',I6)
C
      RETURN
C
C     --- Error Statements ---
C
 900   WRITE(MUNIT,901) INTIN
 901  FORMAT(/,'*****  ERROR-ztsint6-INTERVAL NON-STANDARD',I10/)
      ISTAT = 12
      GO TO 800
C
 910  WRITE(MUNIT,911)NED,NVALS
 911  FORMAT(//,'*****  ERROR - ztsint6 - NO PERIODS.GT.NVALS '  ,
     +/, 'NO OF PERIODS = ' ,I6,'   NVALS = ',I6)
      ISTAT = 11
      GO TO 800
C
 920  WRITE(MUNIT,921)ISTAT
 921  FORMAT('***** - ztsint6 - ERROR.  UNABLE TO',
     *' RETRIEVE DATA - STATUS = ',I3)
      GO TO 800
C
C
 930  CONTINUE
      CALL CHRLNB(PATH,N)
      WRITE(MUNIT,931)PATH(1:N),CUNITS,CTYPE
 931  FORMAT(//' ***** ERROR - ztsint6;  COULD NOT RECOGNIZE DATA TYPE',
     */,' PATHNAME = ',A,/,' UNITS = ',A,'  TYPE = ',A,/)
      ISTAT = 35
      GO TO 800
C
      END

