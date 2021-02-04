submodule(rhyme_nombre_base_unit) parse_smod
contains
   module function rhyme_nombre_base_unit_parse(str) result(unit)
      implicit none

      character(len=*), intent(in) :: str
      type(nombre_base_unit_t), pointer :: unit

      integer :: u, p
      type(nombre_base_unit_t), pointer :: new_unit

      unit => null()

      do u = 1, size(si_base_units)
         if (str .eq. .print.si_base_units(u)) then
            unit => .clone.si_base_units(u)
            exit
         end if
      end do

      prefix_loop: do p = -24, 24
         if (len_trim(prfx_si(p)%symb) .eq. 0) cycle

         if (prfx_si(p)%symb(1:1) .eq. str(1:1)) then
            do u = 1, size(si_base_units)
               new_unit => .clone.si_base_units(u)
               new_unit%prefix = prfx_si(p)

               if (str .eq. .print.new_unit) then
                  unit => new_unit
                  exit prefix_loop
               end if

               deallocate (new_unit)
            end do
         end if
      end do prefix_loop
   end function rhyme_nombre_base_unit_parse
end submodule parse_smod
