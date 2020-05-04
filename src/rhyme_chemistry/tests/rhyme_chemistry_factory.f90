module rhyme_chemistry_factory
   use rhyme_chemistry

   implicit none

   type rhyme_chemistry_factory_t
      character(len=8) :: H_He(3) = ['HII  ', 'HeII ', 'HeIII']
   contains
      procedure :: generate => rhyme_chemistry_factory_generate
   end type rhyme_chemistry_factory_t

   type(rhyme_chemistry_factory_t) :: chemistry_factory = rhyme_chemistry_factory_t()

contains
   function rhyme_chemistry_factory_generate(self, factory_type) result(chem)
      implicit none

      class(rhyme_chemistry_factory_t), intent(inout) :: self
      character(len=*), intent(in) :: factory_type
      type(chemistry_t) :: chem

      integer :: si

      if (factory_type == 'H+He') then
         chem = chemistry_t()

         do si = 1, NSPE
            chem%species_name(si) = self%H_He(si)
         end do
      end if
   end function rhyme_chemistry_factory_generate
end module rhyme_chemistry_factory
