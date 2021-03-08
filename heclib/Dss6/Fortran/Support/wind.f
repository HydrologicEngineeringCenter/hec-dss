      SUBROUTINE WIND (ILFN)                                            MLu
C
      CHARACTER CA*1                                                    MLu
C
      DO 20 I=1,30000                                                   MLu
      READ (ILFN,10,ERR=100,END=100) CA                                 MLu
 10   FORMAT (A)                                                        MLu
 20   CONTINUE                                                          MLu
C
 100  CONTINUE                                                          MLu
      BACKSPACE ILFN                                                    M
      RETURN                                                            MLu
      END                                                               MLu

