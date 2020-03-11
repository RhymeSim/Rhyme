submodule(rhyme_nombre_derived_unit) parse_smod
contains
module function rhyme_nombre_derived_unit_parse(str) result(dunit)
   implicit none

   character(len=*), intent(in) :: str
   type(nombre_unit_t), pointer :: dunit

   integer :: u, p
   type(nombre_unit_t), pointer :: new_dunit

   dunit => null()

   do u = 1, size(derived_units)
      if (str .eq. .print.derived_units(u)) then
         dunit => .clone.derived_units(u)
         exit
      end if
   end do

   prefix_loop: do p = -24, 24
      if (len_trim(prfx_si(p)%symb) .eq. 0) cycle

      if (prfx_si(p)%symb(1:1) .eq. str(1:1)) then
         do u = 1, size(derived_units)
            new_dunit => .clone.derived_units(u)
            new_dunit%prefix = prfx_si(p)

            if (str .eq. .print.new_dunit) then
               dunit => .clone.new_dunit
               exit prefix_loop
            end if

            deallocate (new_dunit)
         end do
      end if
   end do prefix_loop
end function rhyme_nombre_derived_unit_parse
end submodule parse_smod
