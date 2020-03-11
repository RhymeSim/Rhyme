module rhyme_xyz_factory
   use rhyme_xyz

   implicit none

   type rhyme_xyz_factory_t
      private
      character(len=128) :: factory_type = ''
   contains
      procedure :: init => rhyme_xyz_factory_init
      procedure :: generate => rhyme_xyz_factory_generate
      procedure :: final => rhyme_xyz_factory_final
   end type rhyme_xyz_factory_t

   type(rhyme_xyz_factory_t) :: xyz_factory = rhyme_xyz_factory_t()

contains

   subroutine rhyme_xyz_factory_init(self, factory_type)
      implicit none

      class(rhyme_xyz_factory_t), intent(inout) :: self
      character(len=*), intent(in) :: factory_type

      self%factory_type = trim(factory_type)
   end subroutine rhyme_xyz_factory_init

   function rhyme_xyz_factory_generate(self) result(xxx)
      implicit none

      class(rhyme_xyz_factory_t), intent(inout) :: self
      type(xyz_t) :: xxx

      if (self%factory_type == 'Case1') then
         xxx = xyz_t()
      else if (self%factory_type == 'Case2') then
         xxx = xyz_t()
      else
         xxx = xyz_t()
      end if
   end function rhyme_xyz_factory_generate

   subroutine rhyme_xyz_factory_final(self)
      implicit none

      class(rhyme_xyz_factory_t), intent(inout) :: self

      self%factory_type = ''
   end subroutine rhyme_xyz_factory_final

end module rhyme_xyz_factory
