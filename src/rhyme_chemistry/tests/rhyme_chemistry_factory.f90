module rhyme_chemistry_factory
   use rhyme_chemistry

   implicit none

   character(len=8), parameter :: chemistry_factory_H_He(3) = ['HII  ', 'HeII ', 'HeIII']

contains
   function chemistry_factory_generate(factory_type) result(chem)
      implicit none

      character(len=*), intent(in) :: factory_type
      type(chemistry_t) :: chem

      integer :: si

      if (factory_type == 'H+He') then
         chem = chemistry_t()

         do si = 1, NSPE
            chem%species_name(si) = chemistry_factory_H_He(si)
         end do
      end if
   end function chemistry_factory_generate
end module rhyme_chemistry_factory
