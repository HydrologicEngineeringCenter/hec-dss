      SUBROUTINE zintbk6 (DATA, NSD, NED, JULS, ISTIME, INTD,
     + NDINWS, WRKSPC, NDATA, JULSWK, INTLW, OFSETW, ISTAT, IFLAG)
C
C
C     Interpolate block for ztsint6
C
C     Given info in array  WRKSPC  with the date for period 1 of
C     JULSWK.  WRKSPC will contain some data before and after
C     the time window for interpolation needs.
C
C     Put interpolated data from array WRKSPC into DATA array, up to
C     maximum of NDINWS with date/time for location 1 of JULS/ISTIME
C     and date/time for the last location of JULE/IETIME.
C
C     The interval for array WRKSPC is INTLW and for array DATA is INTD.
C
C
C     If IFLAG = 1, data is PERIOD - AVERAGE
C     If IFLAG = 2, data is PERIOD - CUMULATIVE
C     If IFLAG = 3, data is INSTANEOUS VALUE
C     If IFLAG = 4, data is INSTANEOUS CUMULATIVE
C
C
      INCLUDE 'zdssmz.h'
      external lismissingf
      logical lismissingf
C
C
      DIMENSION DATA(*), WRKSPC(*)
C
C
      ISTAT = 0
      IF(MLEVEL.GE.9)WRITE(MUNIT,10)NSD,NED,JULS,ISTIME,NDINWS,JULSWK
 10   FORMAT(T20,'----- ENTERING zintbk6 ',
     */,T10,'NSD = ',I5,T25,'NED = ',I5,/,T10,'JULS = ',I5,
     *T25,'ISTIME = ',I5,/,T10,'NDINWS = ',I5,T25,'JULSWK = ',I5)
C
C
C     Get the number of periods from start of WRKSPC to data.
C     Update the time to prevent truncation.
      J = JULS
      I = ISTIME
      CALL zofset6(J,I,INTD,1,IOFS)
C
      NSWKS = NOPERS(INTD,0,JULSWK,0,J,I)
C     If an offset existed, zofset6 upped time to nearest interval.
C     Therefore NSWKS needs to be decremented ot adhust for this.
      IF (IOFS.GT.0) NSWKS = NSWKS - 1
      OFSETD = FLOAT(IOFS)/FLOAT(INTD)
      IF (MLEVEL.GE.9) WRITE(MUNIT,20)OFSETD,OFSETW
 20   FORMAT(' CALCULATED OFFSETS, OFSETD, OFSETW = ',2F12.6)
C
      IF (MLEVEL.GE.9) WRITE(MUNIT,'('' NSWKS = '',I5)')NSWKS
C     Cechk for a starting date earlier than what we have.
      IF (NSWKS.LT.-35) GO TO 910
C
C     Find upper limit.
C
      N = NDINWS - NSWKS + 1
      NX = NED
      IF (NX.LT.N) N = NX
      LIM = N
C
      M = NSWKS
      KINT = INTD/INTLW
      IDAYOF = 0
      IF ((INTD.EQ.10080).AND.(INTLW.EQ.1440)) THEN
      NDAY = IDAYWK(JULSWK)
      IDAYOF = 8 - NDAY
      ENDIF
C
C
      IF (MLEVEL.GE.9) WRITE(MUNIT,25)NSWKS,NDINWS,N,LIM,KINT
 25   FORMAT(T20,' --- zintbk6 CALCULATIONS --',
     */,T10,'NSWKS = ',I5,T25,'NDINWS = ',I5,
     */,T10,'N = ',I5,T25,'LIM = ',I5,
     */,T10,'KINT = ',I5)
C
C     Determine if we need to interpolate the data.
      IF ((INTD.EQ.43200).AND.(INTLW.EQ.1440)) GO TO 700
      IF (MOD(INTD,INTLW).NE.0) GO TO 300
      IF (INTLW.GT.INTD) THEN
      GO TO 300
      ENDIF
C
C     Check for equal interval data with the same offset.
      IF ((OFSETD.EQ.OFSETW).AND.(KINT.EQ.1)) THEN
      IF (OFSETD.EQ.0) THEN
      M = NSWKS - 1
      ELSE
      M = NSWKS
      ENDIF
      GO TO 200
      ENDIF
C
      IF ((OFSETD.NE.0.0).OR.(OFSETW.NE.0.0)) THEN
      GO TO 300
      ENDIF
C
C     Now actually move block.
C     Simple move - No interpolation required (may require average).
      M = NSWKS - 1
C     If instantaneous data or equal intervals, just make simple move.
      IF ((IFLAG.GE.3).OR.(KINT.EQ.1)) GO TO 200
C
C     Move PERIOD AVERAGE or CUM.
C     Locate array location, and location one data interval earlier.
C     Add all data in this rnage, then divide this by the relative
C     time length.
      DO 150 I=NSD,LIM
      K = (M*KINT) + 1 + IDAYOF
      M = M + 1
      J = M * KINT + IDAYOF
C
      TEMP = 0.0
      DO 110 N=K,J
      IF (MLEVEL.GE.9)WRITE(MUNIT,100)N,WRKSPC(N+35),TEMP
 100   FORMAT(' zintbk6,II,N,WRK,TEMP=',I8,2F12.1)
C     Check for missing data.
      IF (lismissingf(WRKSPC(N+35))) GO TO 140
C     Sum data.
      TEMP = TEMP + WRKSPC(N+35)
 110  CONTINUE
C
C     Divide by relative time length.
      IF (IFLAG.EQ.1) THEN
          DATA(I) = TEMP/FLOAT(KINT)
      ELSE IF (IFLAG.EQ.2) THEN
          DATA(I) = TEMP
      ELSE
          DATA(I) = -901.
      ENDIF
C
      IF (MLEVEL.GE.9) WRITE(MUNIT,130)KINT,I,M,K,J,TEMP,DATA(I)
 130  FORMAT(' MOVBK, KINT,I,M,K,J,TEMP,DATA = ',5I5,2F12.1)
      GO TO 150
C
C     Missing data flag.
 140  CONTINUE
      DATA(I) = WRKSPC(N+35)
C
 150  CONTINUE
      GO TO 900
C
C
C     Move instantaneous or equal interval data.
 200  CONTINUE
      DO 210 I=NSD,LIM
      M = M + 1
      J = M * KINT
      DATA(I) = WRKSPC(J+35)
 210  CONTINUE
      GO TO 900
C
C
C
 300  CONTINUE
C     Non divisable intervals - interpolation required.
C     Get real ratio of intervals.
      RINT = FLOAT(INTD)/FLOAT(INTLW)
      IF (OFSETW.LE.0.0001) OFSETW = 1.0
C
C     Check if instantaneous or period.
      IF (IFLAG.LE.2) GO TO 400
C
C     Instantaneous data.
C     Get the real array location.  Then find the integer location
C     prior to and after this point.
C     Interpolate between these two data values.
      DO 330 I=NSD,LIM
      RM = FLOAT(M) + OFSETD
      R = RM * RINT + (1.0 - OFSETW) + 0.0001
C     R1 will be truncated - make sure truncation is in a
C     negative direction (IE., 134.653 goes to 134, and -1.32 goes
C     to -2).  This is done by adding 40 as a real
C     and then subtracting 40 as an integer.
      R1 = R + 40.0
      J = IFIX(R1) - 40
      K = J + 1
      M = M + 1
      IF (K.GT.NDATA) GO TO 900
      RFRAC = AMOD(R,1.0)
C
C     Check for missing data values.
      IF ((RFRAC.GT..001).AND.lismissingf(WRKSPC(K+35))) GO TO 320
      IF ((RFRAC.LT..99) .AND.lismissingf(WRKSPC(J+35))) GO TO 325
C
C     Interpolate data values.
      DATA(I) = (WRKSPC(K+35)*RFRAC) + (WRKSPC(J+35)*(1.0-RFRAC))
      GO TO 330
C
C     Missing data values.
 320  CONTINUE
      DATA(I) = WRKSPC(K+35)
      GO TO 330
 325  CONTINUE
      DATA(I) = WRKSPC(J+35)
C
 330  CONTINUE
      GO TO 900
C
C
C     PERIOD AVERAG or  CUM data.
C     Find real location of data point in array, then get closest
C     following integer location.  Subtract one interval from real
C     location, and get closest prior integer location.
C     Sum all data between these points, subtract end values not
C     in our range, then divide by the realtive time length.
 400  CONTINUE
      DO 430 I=NSD,LIM
      RM = FLOAT(M-1) +OFSETD
      R1 = RM * RINT + (1.0 - OFSETW) + 0.0001
      R2 = R1 + RINT
C
C     R1 and R2 will be truncated - make sure truncation is in a
C     positive direction (IE., 134.653 goes to 135, and -1.32 goes
C     to -1).  This is done by adding 41   as a real and
C     then subtracting 40 as an integer.
      R11 = R1 + 41.0
      R21 = R2 + 41.0
      K = IFIX(R11) - 40
      J = IFIX(R21) - 40
      M = M + 1
C
C     Make sure we are not getting data beyond what we have.
      IF (J.GT.NDATA) GO TO 900
C
      DATA(I) = 0.0
      DO 410 N=K,J
C     Check for missing values.
      IF((WRKSPC(N+35).LT.-900.).AND.(WRKSPC(N+35).GT.-903.)) THEN
      IF ((N.EQ.J).AND.((K.LT.J).AND.(INTD.LT.INTLW))) THEN
      IF (IFLAG.EQ.2) DATA(I) = DATA(I)*RINT
      IF (IFLAG.EQ.1) DATA(I) = DATA(I)
      GO TO 430
      ENDIF
      GO TO 420
      ENDIF
C     Add data.
      DATA(I) = DATA(I) + WRKSPC(N+35)
 410  CONTINUE
C
C     Subtract off data portions out of range.
      DATA(I) = DATA(I) - (R1-FLOAT(K-1)) * WRKSPC(K+35) -
     + (FLOAT(J)-R2) * WRKSPC(J+35)
C     Divide by the relative time length.
      IF (IFLAG.EQ.1) DATA(I) = DATA(I)/RINT
C
C
      GO TO 430
C
C     Missing data found.
 420  DATA(I) = WRKSPC(N+35)
C
 430  CONTINUE
      GO TO 900
C
C
C     Move monthly data
C
 700  CONTINUE
      IDUMMY = JLIYMD(JULS,IEYR,IEMON,IEDAY)
C
      IF (IFLAG.LE.2) THEN
      DO 750 I=NSD,LIM
      NNN=1440
      MN = M - 1
      K = INCTIM(INTD,0,MN,JULSWK,0,IBEG,IT)
      K = INCTIM(INTD,0,1,IBEG,NNN,IEND,IT)
      IF (IEDAY.GE.27) THEN
      IBEG = IBEG - JULSWK + 2
      IEND = IEND -JULSWK
      ELSE
      IBEG = IBEG - JULSWK  + IEDAY  + 2
      IEND = IEND - JULSWK  + IEDAY
      ENDIF
      M = M + 1
C
      TEMP = 0.0
      DO 710 N=IBEG,IEND
C     Check for missing data.
      IF(lismissingf(WRKSPC(N+35))) GO TO 740
C     Sum data.
      TEMP = TEMP + WRKSPC(N+35)
 710  CONTINUE
C
C     Divide by relative time length.
      IF (IFLAG.EQ.1) THEN
      IT = IEND - IBEG + 1
      DATA(I) = TEMP/FLOAT(IT)
      ELSE
      DATA(I) = TEMP
      ENDIF
      GO TO 750
C
C     Missing data flag.
 740  CONTINUE
      DATA(I) = WRKSPC(N+35)
C
 750  CONTINUE
C
      ELSE
C
C     - - Instantaneous data
      DO 860 I=NSD,LIM
      NNN=1440
      MN = M - 1
      K = INCTIM(INTD,0,MN,JULSWK,0,IBEG,IT)
      K = INCTIM(INTD,0,1,IBEG,NNN,IEND,IT)
      IF (IEDAY.GE.27) THEN
      IBEG = IBEG - JULSWK + 2
      IEND = IEND -JULSWK
      ELSE
      IBEG = IBEG - JULSWK  + IEDAY  + 2
      IEND = IEND - JULSWK  + IEDAY
      ENDIF
      IF ((OFSETD.EQ.0).AND.(OFSETW.EQ.0)) THEN
      DATA(I) = WRKSPC(IEND+35)
      ELSE
      RINT = FLOAT(INTD)/FLOAT(INTLW)
      IF (OFSETW.LE.0.0001) OFSETW = 1.0
      RM = FLOAT(M) + OFSETD
      R = RM * RINT + (1.0 - OFSETW) + 0.0001
      RFRAC = AMOD(R,1.0)
C
C     Check for missing data values
      IF ((RFRAC.GT..001).AND.(WRKSPC(IEND+36).LT.-900.).AND.
     *(WRKSPC(IEND+36).GT.-903.)) GO TO 820
      IF ((RFRAC.LT..99).AND.(WRKSPC(IEND+35).LT.-900.).AND.
     *(WRKSPC(IEND+35).GT.-903.)) GO TO 825
C
C     Interpolate data values.
      DATA(I) = (WRKSPC(IEND+36)*RFRAC) + (WRKSPC(IEND+35)*(1.0-RFRAC))
      GO TO 850
C
C     Missing data flag
 820  CONTINUE
      DATA(I) = WRKSPC(IEND + 36)
      GO TO 850
 825  CONTINUE
      DATA(I) = WRKSPC(IEND + 35)
C
C
      ENDIF
 850  CONTINUE
      M = M + 1
 860  CONTINUE
      ENDIF
 900  CONTINUE
      RETURN
C
C     ------ERROR CONDITIONS-------
C
C
 910  WRITE(MUNIT,911)JULS,ISTIME,NSWKS,JULSWK
 911  FORMAT(//,' *****  ERROR - zintbk6 - STARTING DATE BAD',
     +4(2X,I6)/)
      ISTAT = 21
      RETURN
C
      END

