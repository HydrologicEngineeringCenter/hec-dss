     subroutine test_RTS_Main (ifltab1, messageUnit, status)
!
!
!   Purpose:  Calls all test functions for regular interval time series data
!
      implicit none
!
      integer messageUnit, status
!
!
      integer(8) ifltab1(600)  

      common /lchk/ lcheck
      logical lcheck
      
!      test_RTS_Basic	                Initial basic test for regular interval time series data
!      test_RTS_SecondIntervals	        Tests seconds interval
!      test_RTS_Double	                Initial tests using doubles
!      test_RTS_Expansions	            Tests basic expansion capability of floats and doubles
!      test_RTS_DoubleQual	            Tests doubles with quaitly and int notes
!      test_RTS_NoRepeats	            Tests doubles without quaitly and int notes, but sub block functions, and compression function with no repeat values
!      test_RTS_NoRepeatsQual	        Tests doubles with quaitly and int notes,  sub block functions, and compression function with no repeat values
!      test_RTS_NoRepeatsFloatsToDouble	Tests writing floats with quaitly and int notes, then reading doubles with sub block functions and compression function with no repeat values
!      test_RTS_NoRepeatsDoubleToFloat	Tests writing doubles with quaitly and int notes, then reading floats with sub block functions and compression function with no repeat values
!      test_RTS_CharNotesBasic	        Initial test writing character notes
!      test_RTS_CharNotes	            Tests writing character notes
!      test_RTS_CharNotesFull	        Tests interaction of writing character notes
!      test_RTS_Repeats	                Tests basic data compression with doubles (some values repeated)
!      test_RTS_RepeatsQual	            Tests data compression with doubles, quality and notes
!      test_RTS_RepeatsFloatsToDouble	Tests data compression writing floats with quaitly and int notes, then reading doubles
!      test_RTS_RepeatsDoubleToFloat	Tests data compression writing doubles with quaitly and int notes, then reading floats
!      test_RTS_AllSame	                Tests data compression where all values are the same
!      test_RTS_AllSameDouble	        Tests data compression with doubles where all values are the same
!      test_RTS_AllSameQual	            Tests data compression with doubles, quality and notes, where all values are the same
!      test_RTS_AllSameFloatsToDouble	Tests data compression writing floats with quaitly and int notes, where all values are the same, then reading doubles
!      test_RTS_AllSameDoubleToFloat	Tests data compression writing doubles with quaitly and int notes, where all values are the same, then reading floats

      
      
!    Initial basic test for regular interval time series data
     call test_RTS_Basic(ifltab1, messageUnit, status)
     if (status.ne.0) return
      
!    Tests seconds interval
     call test_RTS_SecondIntervals(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Initial tests using doubles
     call test_RTS_Double(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests basic expansion capability of floats and doubles
     call test_RTS_Expansions(messageUnit, status)
     if (status.ne.0) return
           
!    Tests doubles with quaitly and int notes
     call test_RTS_DoubleQual(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests doubles without quaitly and notes, but sub block functions
     call test_RTS_AllSameDouble(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests doubles without quaitly and int notes, but sub block functions, and compression function with no repeat values
     call test_RTS_NoRepeats(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests doubles with quaitly and int notes,  sub block functions, and compression function with no repeat values
     call test_RTS_NoRepeatsQual(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests writing floats with quaitly and int notes, then reading doubles with sub block functions and compression function with no repeat values
     call test_RTS_NoRepeatsFloatsToDouble(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests writing doubles with quaitly and int notes, then reading floats with sub block functions and compression function with no repeat values
     call test_RTS_NoRepeatsDoubleToFloat(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Initial test writing character notes
     call test_RTS_CharNotesBasic(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests writing character notes
     call test_RTS_CharNotes(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests interaction of writing character notes
     call test_RTS_CharNotesFull(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests basic data compression with doubles (some values repeated)
     call test_RTS_Repeats(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests data compression with doubles, quality and notes
     call test_RTS_RepeatsQual(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests data compression writing floats with quaitly and int notes, then reading doubles
     call test_RTS_RepeatsFloatsToDouble(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests data compression writing doubles with quaitly and int notes, then reading floats
     call test_RTS_RepeatsDoubleToFloat(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests data compression where all values are the same
     call test_RTS_AllSame(ifltab1, messageUnit, status)
     if (status.ne.0) return
                     
!    Tests data compression with doubles where all values are the same
     call test_RTS_AllSameDouble(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests data compression with doubles, quality and notes, where all values are the same
     call test_RTS_AllSameQual(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests data compression writing floats with quaitly and int notes, where all values are the same, then reading doubles
     call test_RTS_AllSameFloatsToDouble(ifltab1, messageUnit, status)
     if (status.ne.0) return
           
!    Tests data compression writing doubles with quaitly and int notes, where all values are the same, then reading floats
     call test_RTS_AllSameDoubleToFloat(ifltab1, messageUnit, status)
     if (status.ne.0) return

      
      if (lcheck) call zcheckFile(ifltab1, status)
      if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('test_RTS_Main Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED test_RTS_Main') 
        endif 
!   
      return
      end

