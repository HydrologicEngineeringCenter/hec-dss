      subroutine crack_unit_spec(unit_spec, unit, vertical_datum)

      implicit none
      character*(*) unit_spec, unit, vertical_datum
      integer i1, i2

      unit = unit_spec
      vertical_datum = ' '

      i1 = index(unit_spec, 'U=')
      if (i1.gt.0) then
        i1 = i1 + 2
        i2 = index(unit_spec(i1:), '|')
        if (i2.eq.0) then
          i2 = len_trim(unit_spec)
        else
          i2 = i1 + i2 - 2
        end if
        unit = unit_spec(i1:i2)
        i1 = index(unit_spec, 'V=')
        if (i1.gt.0) then
          i1 = i1 + 2
          i2 = index(unit_spec(i1:), '|')
          if (i2.eq.0) then
            i2 = len_trim(unit_spec)
          else
            i2 = i1 + i2 - 2
          end if
          vertical_datum = unit_spec(i1:i2)
        end if
      end if

      end subroutine crack_unit_spec

