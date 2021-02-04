submodule(rhyme_periodic_table) get_element_by_name_smod
contains
   module function rhyme_periodic_table_get_element_by_name(pt, element_symb) result(element)
      implicit none

      class(periodic_table_t), intent(in) :: pt
      character(len=*), intent(in) :: element_symb
      type(periodic_table_element_t) :: element

      integer :: ei

      do ei = 1, size(pt%elements)
         if (pt%elements(ei)%symb .eq. element_symb) then
            element = pt%elements(ei)
         end if
      end do
   end function rhyme_periodic_table_get_element_by_name
end submodule get_element_by_name_smod
