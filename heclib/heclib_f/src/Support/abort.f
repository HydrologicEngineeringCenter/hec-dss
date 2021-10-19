      SUBROUTINE ABORT                                                  MLu
C
C     TEMPORARY ABORT ROUTINE FOR NON-HARRIS SITES
C
      WRITE (6,10)                                                      MLu
 10   FORMAT (//,' ***** PROGRAM CALLED ABORT *****',//)                MLu
C
C     CALL EXIT (2)                                                     u
C
C     GET THE SQUARE ROOT OF A NEGATIVE NUMBER TO CAUSE A FATAL ERROR
      X = SIN (0.2)                                                     L
      X = -X                                                            L
      Y = SQRT (X)                                                      L
      WRITE (*,*) Y                                                     L
      STOP                                                              MLu
      END                                                               MLu

