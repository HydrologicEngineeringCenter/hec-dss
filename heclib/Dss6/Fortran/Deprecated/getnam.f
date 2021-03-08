      SUBROUTINE GETNAM (ILFN,CNAME,IFLAG)
C
C       THIS ROUTINE COPIED FROM QL=HECC ON 21 MAY 85 FOR USE
C       WITH MODIFICATIONS TO PREAD FUNCTION FILE ASSIGNMENTS
C       BY DON HANSEN.  ( HECC IS A TEMPORARY REPOSITORY OF
C       CHARACTER MANIPULATIONS ROUTINES BEING WRITTEN AT THIS
C       TIME.
C                         DENNIS HUFF 22MAY85
C
C     INTEGER NAME1(6)                                                  H
      CHARACTER CNAME*(*)
C     CHARACTER CNAME1*18                                               H
C     EQUIVALENCE (NAME1,CNAME1)                                        H
C
C
C     NOTE: INQUIRE DOES NOT WORK WELL ON HARRIS
      CNAME = ' '
      INQUIRE (UNIT=ILFN,NAME=CNAME,IOSTAT=IFLAG)                       MLu
C
C     IF ((ILFN.EQ.5).OR.(ILFN.EQ.6)) THEN                              u
C     IF ((CNAME(1:2).EQ.'  ').AND.(IFLAG.EQ.0)) CNAME = '/dev/tty'     u
C     IF (CNAME(1:3).EQ.'std') CNAME = '/dev/tty'                       l
C     ENDIF                                                             u
C
C     CALL LFNAME (ILFN,NAME1,IFLAG)                                    H
C
C     IF (IFLAG.EQ.-1)THEN                                              H
      RETURN
C     ELSE IF(IFLAG.EQ.1) THEN                                          H
C     IFLAG=NAME1(1)                                                    H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C     CNAME(1:8) = CNAME1(10:17)                                        H
C     CALL CHRLNB (CNAME(1:8),IPOS)                                     H
C     IPOS = IPOS + 1                                                   H
C     CNAME(IPOS:IPOS) = '*'                                            H
C     IPOS = IPOS + 1                                                   H
C     JPOS = IPOS + 7                                                   H
C     CNAME(IPOS:JPOS) = CNAME1(1:8)                                    H
C     IF (JPOS.LT.17) THEN                                              H
C     JPOS = JPOS + 1                                                   H
C     CNAME(JPOS:17) = ' '                                              H
C     ENDIF                                                             H
C
C     RETURN                                                            H
      END

