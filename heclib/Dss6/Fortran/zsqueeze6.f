      SUBROUTINE ZSQUEEZE6(CNAME, ISTAT)
#ifdef _MSC_VER
      USE IFPORT
      USE IFCORE
#endif
      implicit none 
C
C     Squeeze a DSS file.
C     (Removes all Dead space, and can recover deleted
C     records or fix a damaged file.)
C

#ifdef __sun__  
      integer rename
#endif 
C
      CHARACTER CNAME*(*)
      CHARACTER CNAME2*256
      CHARACTER CTEMP*256
	CHARACTER CSCRAT*25
      LOGICAL LRETAG
C
      INTEGER IFTAB1(1200)
      INTEGER IFTAB2(1200)
C
      INTEGER KBUFF1, KBUFF2
      PARAMETER (KBUFF1=10000, KBUFF2=2000)
      INTEGER IBUFF1(KBUFF1), IBUFF2(KBUFF2)
C
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB, CURRENT_NUMB, INTERRUPT
      INTEGER NERROR, MAXERROR
      INTEGER ISTAT, ILAST, IVER,MLVL,MUNIT,IERR
      INTEGER ISIZE,IDEAD,JSIZE,NRECS,I,MAXPATHS,ICOLL
      INTEGER N,N2
      REAL RDEAD,SIZE,FACTOR
C
      LRETAG = .FALSE.
      ISTAT = 0
      INTERRUPT = 0
C
C
      TOTAL_NUMB = 0
      CURRENT_NUMB = 0
C
C
      CALL CHRLNB (CNAME, ILAST)
      CALL zcheckFileVer(CNAME(1:ILAST), IVER)
      IF (IVER.EQ.7) THEN
        call zsqueeze7(CNAME(1:ILAST), ISTAT)
        RETURN
      ENDIF
C
      CSCRAT = ' '
      CNAME2 = CNAME
      CNAME2(ILAST:ILAST) = '-'

      CALL ZQUERY('MLVL', CSCRAT, MLVL)
      CALL ZQUERY('MUNIT', CSCRAT, MUNIT)
      if (munit.gt.0) then
      write (munit, *)'Entered zsqueeze for file ', cname(1:ILAST)
      endif
      IF (MLVL.LE.4) THEN
         CALL zset('MLEVEL', ' ', 1)
      ENDIF
C
C     Check that we have access to the file
      CALL ZOPEN6(IFTAB1, CNAME(1:ILAST), ISTAT)
      if (ISTAT.NE.0) THEN
        CALL ZCLOSE(IFTAB1)
        CALL zset('MLEVEL', ' ', MLVL)
        if (munit.gt.0) then
            write (munit, *)'Cannot access file ',cname(1:ILAST)
         endif
        RETURN
      ENDIF
C
C
C     Is someone else using the file?
      CALL ZLOCKRD(IFTAB1, 2, ISTAT)
      IF (ISTAT.GT.0) THEN
        CALL ZCLOSE(IFTAB1)
        CALL zset('MLEVEL', ' ', MLVL)
        if (munit.gt.0) then
            write(munit,*)'Cannot get access to squeeze ',cname(1:ILAST)
         endif
        RETURN
      ENDIF
C
C
C     Okay, looks like we are good to go
      CALL ZCLOSE(IFTAB1)
C
C     Rename the file to make sure that we have access to it
C

C     Do the rename.  The new file name is the same
C     file name with the last character replaced with a '-'
      ierr = rename(cname, cname2(1:ILAST))
C     Is the file typed for delete access?
C     An old DSS file there?
C
C     Is someone using the file?  (If so, wait for a time in batch mode)
C
      IF (IERR.NE.0) GO TO 910
C
C     The rename was successful.  Rename it back.
 60   CONTINUE
      ierr = rename(cname2(1:ILAST), cname)
C
C
C     Open the file in exclusive mode
      IF (IVER.EQ.6) THEN
         CALL ZOPEN6(IFTAB1, CNAME(1:ILAST), IERR)
         IF (IERR.NE.0) THEN
            IVER = 0
            CALL zset6("CLEAR", "", 0)
         ENDIF
      ELSE IF (IVER.EQ.7) THEN
         CALL ZOPEN7(IFTAB1, CNAME(1:ILAST), IERR)
         IF (IERR.NE.0) THEN
            IVER = 0
         ENDIF
      ELSE
        WRITE(*,*)'Unrecongized DSS file, version: ', IVER
        ISTAT = -10
        RETURN
      ENDIF
C
      IF (IVER.EQ.6) THEN

C        Get the current size of the DSS file
         CALL zinqir6 (IFTAB1, 'SIZE', CSCRAT, ISIZE)
         CALL zinqir6 (IFTAB1, 'DEAD', CSCRAT, IDEAD)
         RDEAD = REAL(IDEAD) / 100.
         SIZE = REAL(ISIZE) * (1.0 - RDEAD)
         JSIZE = INT(SIZE)
C
C
         CALL zinqir6 (IFTAB1, 'NRECS', CSCRAT, NRECS)
C        Set future size optimization here ************************
C        (Check how old the file is etc.)
C
C       See if the file size or table type are specified by the user
C       CALL NEWFS (CLINE, NLINE, CSCRAT, .FALSE.)
        CALL zset6 ('WLOCK', 'ON', I)
C
C       Open the new file with write lock on
        maxPaths = REAL(NRECS) * 1.2
        IF (maxPaths.GT.1000) then
            CALL zset6 ('SIZE', ' ', maxPaths)
         endif
         CALL ZOPEN6(IFTAB2, CNAME2(1:ILAST), IERR)
         call zinqir6 (IFTAB1, 'MAXP', CSCRAT, ISIZE)
         !  If the size is way larger than the current size, reset it to something more reasonable
         IF ((ISIZE > 0).AND.(NRECS < ISIZE)) THEN
            JSIZE = NRECS * 3
            IF (JSIZE < ISIZE) THEN
                maxPaths = JSIZE
            ENDIF
         ENDIF
C        Set the hash and bin size corresponding to our new value
         CALL zset6 ('MAXP', ' ', maxPaths)
C        Check if a collections file.  If so, set a flag to increase the bin size
         call zinqir6(IFTAB1, 'coll', cscrat, icoll)
         factor = float(icoll) / float(nrecs);
         if (factor >= 0.50) then
           if (munit.gt.0) then
           IF (MLVL.LE.5) write (munit, *)'Collection size increase set'
           endif
           call zset ('coll', ' ', 1)
         endif
         IF (ISIZE > 0) THEN
C           But keep the original size set in the file
            CALL ZSETFI(IFTAB2, 'MAXP', CSCRAT, ISIZE, ISTAT)
         ENDIF
         CALL ZCOFIL6(IFTAB1, IFTAB2, IBUFF1, KBUFF1, IBUFF2, KBUFF2,
     *                .FALSE., LRETAG)
C
      ELSE ! version 7
C
        CALL ZOPEN7(IFTAB2, CNAME2(1:ILAST), IERR)
        call zcopyfile(IFTAB1, IFTAB2, ierr)
        IF (ierr.NE.0) GO TO 840
      endif
C
      IF (INTERRUPT.NE.0) go to 840
C
C     Now close files and rename back
      CALL ZCLOSE (IFTAB1)
      CALL ZCLOSE (IFTAB2)
C
C
C
C     Get the permissions on the old file
C     CALL CHRLNB (CNAME, ILAST)                                        u
C     CTEMP = CNAME(1:ILAST) // CHAR(0)                                 u
C     CALL PERMISSIONS (CTEMP, IPERM, IST)                              u
C
      call deletefile(CNAME, IERR)
      IF (IERR.NE.0) THEN
#ifdef _MSC_VER          
         IERR = GETLASTERRORQQ()
#endif         
         CTEMP = ' '
         CALL GERROR (CTEMP)
         CALL CHRLNB(CTEMP, N)
         if (munit.gt.0) then
         WRITE(MUNIT, 101) CTEMP(1:N)
 101	   FORMAT (' Unable to remove old DSS file, error: ', A)
         endif
         GO TO 930
      ENDIF
C
      !CALL CRENAM (CNAME2, CNAME, IERR)
      ierr = rename(cname2(1:ILAST), cname(1:ILAST))
      IF (IERR.NE.0) GO TO 930
C
C     Set the permissions on the new file
C     IF (IST.EQ.0) CALL CHMODF (CTEMP, IPERM, IST)                     u
C
C
 800  CONTINUE
      write(*, *)'Exit zsqueeze, status = ', ISTAT
      if (munit.gt.0) then
      write(MUNIT, *)'Exit zsqueeze, status = ', ISTAT
      endif
      CALL zset6 ('MLEVEL', ' ', MLVL)
      IF ((ISTAT.NE.0).and.(munit.gt.0)) THEN
#ifdef _MSC_VER           
        IERR = GETLASTERRORQQ()
#endif         
         CTEMP = ' '
         CALL GERROR (CTEMP)
         CALL CHRLNB(CTEMP, N)
         WRITE(MUNIT, 804) CTEMP(1:N)
 804	   FORMAT (' Error: ', A)
      ENDIF
      call flush(MUNIT)
      RETURN
C
 840  CONTINUE
C     Interrupt squeezed, files opened
      CALL ZCLOSE (IFTAB1)
      CALL ZCLOSE (IFTAB2)
      call deletefile(CNAME2(1:ILAST), IERR)
      GO TO 800
C
C
 910  CONTINUE
      ISTAT = -1
      CALL CHRLNB(CNAME, N)
      CALL CHRLNB(CNAME2, N2)
      if (munit.gt.0) then
      WRITE (MUNIT, 913) CNAME(1:N), CNAME2(1:N2), IERR
 913  FORMAT(' *** ERROR:  Unable to rename DSS file to temporary name.'
     * /,' Current Name: ',A,/,' Temporary Name: ',A,/,' Error:',I5)
      endif
      GO TO 800
C
 920  CONTINUE
      ISTAT = -2
      CALL CHRLNB(CNAME, N)
      WRITE (MUNIT, 921) CNAME(1:N), IERR
 921  FORMAT (' *** ERROR:  Unable to attach to DSS file ',A,/
     * ' in an exclusive mode.  Error: ',I5)
      GO TO 800
C
 930  CONTINUE
      ISTAT = -3
      CALL CHRLNB(CNAME, N)
      CALL CHRLNB(CNAME2, N2)
      WRITE (MUNIT, 931) CNAME(1:N), CNAME2(1:N2), IERR
 931  FORMAT (' *** ERROR:  Unable to eliminate old DSS file ',A,/
     * ' Current Squeezed DSS file is named ',A,/,' Error: ',I5)
      GO TO 800
C
C
      END

