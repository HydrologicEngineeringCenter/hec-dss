      SUBROUTINE DBGOUT ( LFNOUT,HEAD,IFIRST,ILAST,MAXHD,NHEAD,
     1                  JDATE,MTIME,DATA,DQ,NXTDAT,MAXDAT,NDATA )
C
C      SUBROUTINE DBGOUT PRINTS OUT THE CONTENTS OF THE HEADER AND DATA
C      TABLES TO AID IN DIAGNOSING AND DEBUGGING PROBLEMS IN SHFDSS.
C             DENNIS HUFF
C             18 APR 86
C
      CHARACTER HEAD(*)*(*),DQ(*)*(*),CCOUT*80
      DIMENSION IFIRST(*),ILAST(*),JDATE(*),MTIME(*),DATA(*),NXTDAT(*)
      INTEGER*4 JDATE                                                   M
C
C     WRITE OUT DATA BY HEADER
C
c     id - shef id, idur - duration, kodt - type, kods - send code
c     irev - revision code, kodex - extremum code
      DO 50 N=1,NHEAD
         Write(lfnout,'(/8x,a)')
     . '  ID     PE IDUR KODT KODS IREV KODEX'
         CCOUT=HEAD(N)(1:8)//'-'//HEAD(N)(9:10)//'-'//HEAD(N)(11:14)
     .   //'---'//HEAD(N)(15:15)//'----'//HEAD(N)(16:16)//'----'//
     .   HEAD(N)(17:17)//'----'//head(n)(18:18)//'--+'
C         WRITE ( LFNOUT,10 ) HEAD(N)
         WRITE ( LFNOUT,10 ) CCOUT
  10     FORMAT ( ' HEAD = ',A//' JDATE MTIME     DATA     DQ'/)
         I = IFIRST(N)
  20     CONTINUE
            WRITE ( LFNOUT,25 ) JDATE(I),MTIME(I),DATA(I),DQ(I)
  25        FORMAT ( 2I6,1X,E10.3,1X,A )
            I = NXTDAT(I)
            IF ( I.GT.0 ) GO TO 20
  50  CONTINUE
      RETURN
      END
