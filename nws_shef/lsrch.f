      SUBROUTINE LSRCH ( KEY,LIST,NLIST,ILIST )
C
      CHARACTER KEY*(*),LIST(*)*(*)
C
      ILIST = 1
   20      CONTINUE
      IF ( KEY.NE.LIST(ILIST) ) THEN
         IF ( ILIST.LT.NLIST ) THEN
            ILIST = ILIST + 1
            GO TO 20
         ELSE
            ILIST = 0
         ENDIF
      ENDIF
      RETURN
      END
