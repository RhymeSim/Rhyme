module rhyme_chemistry_factory
   use rhyme_chemistry

   implicit none

   type rhyme_chemistry_factory_t
      private
      character(len=128) :: factory_type = ''
   contains
      procedure :: init => rhyme_chemistry_factory_init
      procedure :: generate => rhyme_chemistry_factory_generate
      procedure :: final => rhyme_chemistry_factory_final
   end type rhyme_chemistry_factory_t

   type(rhyme_chemistry_factory_t) :: chemistry_factory = rhyme_chemistry_factory_t()

contains

   subroutine rhyme_chemistry_factory_init(self, factory_type)
      implicit none

      class(rhyme_chemistry_factory_t), intent(inout) :: self
      character(len=*), intent(in) :: factory_type

      self%factory_type = trim(factory_type)
   end subroutine rhyme_chemistry_factory_init

   function rhyme_chemistry_factory_generate(self) result(chem)
      implicit none

      class(rhyme_chemistry_factory_t), intent(inout) :: self
      type(chemistry_t) :: chem

      if (self%factory_type == 'Case1') then
         chem = chemistry_t()
      else if (self%factory_type == 'Case2') then
         chem = chemistry_t()
      else
         chem = chemistry_t()
      end if
   end function rhyme_chemistry_factory_generate

   subroutine rhyme_chemistry_factory_final(self)
      implicit none

      class(rhyme_chemistry_factory_t), intent(inout) :: self

      self%factory_type = ''
   end subroutine rhyme_chemistry_factory_final

end module rhyme_chemistry_factory
