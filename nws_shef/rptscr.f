      SUBROUTINE RPTSCR ( CRPT,IFIRST,JDATE,MTIME,NXTDAT )
C
C     RPTSCR SCREENS FOR DUPLICATE OBSERVATIONS AS
C     INDICATED BY IDENTICAL OBSERVATION TIMES FOR
C     A PAIR OF OBSERVATIONS.
C      THE  ORDER OF THE DATA VALUES IS CHRONOLOGICAL,
C     WITH DUPLICATE TIMES IN THE ORDER
C      IN WHICH THEY WERE RECEIVED.
C      THIS PROCEEDURE IS IN ACCORDANCE WITH
C     SHEF RULES FOR INTERPRETING IDENTICAL OBSERVATIONS
C     ( REF:  SHEF MANUAL ).  DATES AND TIMES FOR
C     A HOMOGENEOUS SERIES OF DATA ARE IDENTIFIED
C     IN THE ORDER RECEIVED BY FOLLOWING THE
C     CHAIN OF POINTERS TO SUCCEEDING VALUES
C     <NXTDAT> BEGINNING WITH THE POINTER TO THE
C     FIRST VALUE <IFIRST>.
C                DENNIS HUFF 11 OCT 84
C
!
!     Completely re-wrote filter code - the old code didn't keep the 
!     correct values when data sets contained multiple values at the 
!     same date/time when the revison flag was used (don't know about
!     non-revised data).  Oh, well, it was only broken for ~30 years!
!
!     Mike Perryman
!     22 April 2013
!	
      CHARACTER CRPT*1
      DIMENSION JDATE(*),MTIME(*),NXTDAT(*)
C     INTEGER*4 JDATE                                                   M
C
C
      iprev = 0
      i = ifirst
      if (crpt.eq.'R') then
         !------------------------------------------------!
         ! keep the LAST in a sequence of same date/times !
         !------------------------------------------------!
         do
            j = nxtdat(i)
            if (j.gt.0               
     1          .and.jdate(j).eq.jdate(i)
     2          .and.mtime(j).eq.mtime(i)) 
     3      then
               do while (j.gt.0
     1                   .and.jdate(j).eq.jdate(i)
     2                   .and.mtime(j).eq.mtime(i))
                  nxtdat(iprev) = j
                  k = nxtdat(j)
                  if (k.gt.0 
     1                .and.jdate(k).eq.jdate(j)
     2                .and.mtime(k).eq.mtime(j))
     3            then
                     do while (k.gt.0
     1                         .and.jdate(k).eq.jdate(j)
     2                         .and.mtime(k).eq.mtime(j))
                        nxtdat(iprev) = k
                        k = nxtdat(k)
                     end do
                     j = k
                  else
		     j = nxtdat(j)
                  end if
               end do
            else
               iprev = i
	    end if
	    if (j.eq.0) exit
	    i = nxtdat(i)
         end do
      else
         !-------------------------------------------------!
         ! keep the FIRST in a sequence of same date/times !
         !-------------------------------------------------!
         do
            j = nxtdat(i)
            do while (j.gt.0
     1                .and.jdate(j).eq.jdate(i)
     2                .and.mtime(j).eq.mtime(i))
               j = nxtdat(j)                   
            end do
            if (j.eq.0) exit
            nxtdat(i) = j
            i = nxtdat(i) 
         end do
      end if
	
      RETURN
      END
