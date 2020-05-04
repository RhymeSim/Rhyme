module rhyme_periodic_table_factory
   use rhyme_periodic_table

contains

   function periodic_table_factory_generate(factory_type) result(pt)
      implicit none

      character(len=*), intent(in) :: factory_type
      type(periodic_table_t) :: pt

      if (factory_type == 'empty') then
         pt = periodic_table_t()
      else
         print *, 'Unknown periodic table factory type!', factory_type
      end if
   end function periodic_table_factory_generate
end module rhyme_periodic_table_factory
