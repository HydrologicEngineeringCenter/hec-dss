

         program interop

        implicit none
    
        character notes(2200)*50
        character c*10
         c = '0123456789'
        notes(1)='abcdefghijklmnopqrstuvwxyzAbcdefghijklmnopqrstuvwx'
        call interop_char_array(notes)

        call interop_char(c)


        
         end program