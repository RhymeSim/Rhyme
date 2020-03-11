module rhyme_color_factory
   use rhyme_color

   implicit none

   type rhyme_color_factory_t
      private
      character(len=128) :: factory_type = ''
   contains
      procedure :: init => rhyme_color_factory_init
      procedure :: generate => rhyme_color_factory_generate
      procedure :: final => rhyme_color_factory_final
   end type rhyme_color_factory_t

   type(rhyme_color_factory_t) :: color_factory = rhyme_color_factory_t()

contains

   subroutine rhyme_color_factory_init(self, factory_type)
      implicit none

      class(rhyme_color_factory_t), intent(inout) :: self
      character(len=*), intent(in) :: factory_type

      self%factory_type = trim(factory_type)
   end subroutine rhyme_color_factory_init

   function rhyme_color_factory_generate(self) result(clr)
      implicit none

      class(rhyme_color_factory_t), intent(inout) :: self
      type(color_t) :: clr

      if (self%factory_type == 'Case1') then
         clr = colors(1)
      else if (self%factory_type == 'Case2') then
         clr = colors(2)
      else
         clr = colors(1)
      end if
   end function rhyme_color_factory_generate

   subroutine rhyme_color_factory_final(self)
      implicit none

      class(rhyme_color_factory_t), intent(inout) :: self

      self%factory_type = ''
   end subroutine rhyme_color_factory_final
end module rhyme_color_factory
