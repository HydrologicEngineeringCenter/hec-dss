      PROGRAM WRKDSS
C
C     WORKS THE DSS DATA BASE SOFTWARE
C
C     FORTRAN 77 VERSION
C     USER MUST OPEN UNIT 6 AND 5
      INTEGER*4 ML
C
      CHARACTER*32 A, B, C, D, E, F
      CHARACTER*80 PATH 
      CHARACTER CNAME*30, q*1
	INTEGER*2 IHR,IMIN,ISEC,I100SC                                    M
	INTEGER*2 JHR,JMIN,JSEC,J100SC                                    M
	INTEGER(8) SYSCLOCK
C
      LOGICAL LPOPT
      REAL DATA(2000), RD
      INTEGER IH(500)
      INTEGER IFLTAB(1200)
      LOGICAL LF
      INTEGER*4 ITEMP
      CHARACTER CTEMP*20
C
      WRITE (6,*)'Enter message level. 2: terse output, ' //
     *'3: ZWRITE messages,  4: ZREAD messages'
      READ (5,*) ML
      CALL ZSET ('MLVL',' ', ML)
C     IF (LPOPT('D')) CALL ZSET6 ('MLVL', ' ', 14)
      WRITE (6,*)'Enter DSS database file name'
      READ (5,10) CNAME
 10   FORMAT(A)
C
*      WRITE (6,*)'Memory test version'
*	call zset ('MEMORY', 'ON', 20000000)
      call chrlnb(cname,n)
      CALL ZOPEN (IFLTAB, CNAME(1:n), ISTAT)
      WRITE (6,*)'Open status = ',ISTAT
      CALL ZINQIR(IFLTAB, 'ERRO', CTEMP, IERROR)
      IF (IERROR.GT.2) CALL ABORT() 
C
      WRITE (6,*)'Enter A part for all writes'
      READ (5,4) A
 4    FORMAT (A)
      WRITE (6,*)'Enter number of records to write'
      READ (5,*) NLOOPS
C
C
      PATH = ' '
C
      B = 'PART B'
      C = 'C'
      D = 'PART D'
      E = 'PART E'
      F = 'F'
C
C
C     ***** SET TIMER INITIZALIATION HERE ******
c     CALL BTIME

!       IREC = SYSCLOCK()
      ireport = nloops/10;
      if (ireport.eq.0) ireport = 1
C
  !    CALL GETTIM(IHR,IMIN,ISEC,I100SC)

C     GENERATE PATHNAMES WITH DIFFERENT C AND F PARTS
C
C     WRITE OUT NLOOPS RECORDS
      DO 100 I=1,NLOOPS
	NN = MOD (i, ireport)
	if (NN.eq.0) then
	write (6,*)'Processed writing records: ', I
C	call waits (1.0)
	endif
      J = NLOOPS - I
      CALL INTGRC ( I, C, 2, 10)
      CALL INTGRC ( J, F, 2, 10)

      NPATH = 80
C
      CALL ZPATH (A,B,C,D,E,F,PATH,NPATH)
C
C     IF WE COMPARE TO VERS 5, THEN EQUIVALENT NUMBER OF DATA
C     IS ONE-HALF, SINCE VERS 6 USES INTEGER*4 WORDS
      NH = 100
      NDA = 600
      IP = 0
C
      DO 20 K=1,NDA
      DATA(K) = REAL(I) * 100 + K
 20   CONTINUE
C
C     CALL ZCHECK ( IFLTAB, PATH, NPATH, NH, NDA, LF)
C     CALL ZWRITE ( IFLTAB, PATH, NPATH, IH, NH, DATA, NDA, IP, LF)
      CALL ZWRITX ( IFLTAB, PATH, NPATH, IH, NH, IH, NH,
     * IH, NH, DATA, NDA, 0, IP, IST, LF)
      CALL ZINQIR(IFLTAB, 'ERRO', CTEMP, IERROR)
      IF (IERROR.GT.2) CALL ABORT() 
c
c	 READ (5,*) KKK
c	WRITE (6,*) KKK
C
 100  CONTINUE
C
C     ***** CHECK TIMER HERE ******
C     CALL ETIME
      WRITE (6,*)'WRITE COMPLETE:  Proceed with reads'
C
       
C
 77   continue
C     READ OUT NLOOPS RECORDS
      DO 200 I=1,NLOOPS
      J = NLOOPS - I
	CALL INTGRC ( I, C, 2, 10)
      CALL INTGRC ( J, F, 2, 10)
C
      NPATH = 80
C
      CALL ZFPN ( A, 32, B, 32, C, 32, D, 32, E, 32, F, 32, PATH, NPATH)
C
      NH = 100
      NDA = 600
      IP = 0
C      
      NN = MOD (i, ireport)
      if (NN.eq.0) then
	write (6,*)'Processed reading records: ', I
C	call waits (1.0)
	endif
C
C     CALL ZREAD ( IFLTAB, PATH, NPATH, IH, NH, DATA, NDA, IP, LF)
      CALL ZREADX (IFLTAB, PATH, IH, 100, NH, IH, 100, NH, IH, 100, NH,
     * DATA, 600, NDA, 0, LF)
      CALL ZINQIR(IFLTAB, 'ERRO', CTEMP, IERROR)
      IF (IERROR.GT.2) CALL ABORT() 
C
      IF (.NOT.LF) WRITE (6, 120) PATH
 120  FORMAT (//,' **** RECORD NOT FOUND *****',/' Path: ',A)
      IF (.NOT.LF) GO TO 200
C
      DO 140 K=1,NDA
      RD = REAL(I) * 100 + K
      IF (DATA(K).NE.RD) THEN
      WRITE (6,*)'Data not the same: ',RD,DATA(K),K
      GO TO 200
      ENDIF
 140  CONTINUE
C
 200  CONTINUE
C
      CALL ZCLOSE ( IFLTAB)
C
      STOP
      END
