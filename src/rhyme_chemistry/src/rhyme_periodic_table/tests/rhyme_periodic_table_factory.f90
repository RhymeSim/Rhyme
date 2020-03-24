module rhyme_periodic_table_factory
   use rhyme_periodic_table

   implicit none

   type rhyme_periodic_table_factory_t
      private
      character(len=128) :: factory_type = ''
   contains
      procedure :: init => rhyme_periodic_table_factory_init
      procedure :: generate => rhyme_periodic_table_factory_generate
      procedure :: final => rhyme_periodic_table_factory_final
   end type rhyme_periodic_table_factory_t

   type(rhyme_periodic_table_factory_t) :: periodic_table_factory = rhyme_periodic_table_factory_t()

contains

   subroutine rhyme_periodic_table_factory_init(self, factory_type)
      implicit none

      class(rhyme_periodic_table_factory_t), intent(inout) :: self
      character(len=*), intent(in) :: factory_type

      self%factory_type = trim(factory_type)
   end subroutine rhyme_periodic_table_factory_init

   function rhyme_periodic_table_factory_generate(self) result(pt)
      implicit none

      class(rhyme_periodic_table_factory_t), intent(inout) :: self
      type(periodic_table_t) :: pt

      if (self%factory_type == 'Case1') then
         pt = periodic_table_t()
      else if (self%factory_type == 'Case2') then
         pt = periodic_table_t()
      else
         pt = periodic_table_t()
      end if
   end function rhyme_periodic_table_factory_generate

   subroutine rhyme_periodic_table_factory_final(self)
      implicit none

      class(rhyme_periodic_table_factory_t), intent(inout) :: self

      self%factory_type = ''
   end subroutine rhyme_periodic_table_factory_final

end module rhyme_periodic_table_factory
