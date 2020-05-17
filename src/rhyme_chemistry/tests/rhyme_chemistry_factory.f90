module rhyme_chemistry_factory
   use rhyme_chemistry

   implicit none

   character(len=8), parameter :: chemistry_factory_element_names(2) = ['H ', 'He']
   real(kind=8), parameter :: chemistry_factory_element_abundances(2) = [.75d0, .25d0]

contains
   function chemistry_factory_generate(factory_type) result(chem)
      implicit none

      character(len=*), intent(in) :: factory_type
      type(chemistry_t) :: chem

      chem%element_names = ''
      chem%element_abundances = 0d0

      select case (factory_type)
      case ('H+He')
         chem%element_names(1:2) = chemistry_factory_element_names(1:2)
         chem%element_abundances(1:2) = chemistry_factory_element_abundances(1:2)
      case default
         print *, 'Unknonw factory type!', factory_type
      end select
   end function chemistry_factory_generate
end module rhyme_chemistry_factory
