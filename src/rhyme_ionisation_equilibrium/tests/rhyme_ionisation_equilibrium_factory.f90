module rhyme_ionisation_equilibrium_factory
   use rhyme_ionisation_equilibrium

   implicit none

   type rhyme_ionisation_equilibrium_factory_t
      private
      character(len=128) :: factory_type = ''
   contains
      procedure :: init => rhyme_ionisation_equilibrium_factory_init
      procedure :: generate => rhyme_ionisation_equilibrium_factory_generate
      procedure :: final => rhyme_ionisation_equilibrium_factory_final
   end type rhyme_ionisation_equilibrium_factory_t

   type(rhyme_ionisation_equilibrium_factory_t) :: ie_factory = rhyme_ionisation_equilibrium_factory_t()

contains

   subroutine rhyme_ionisation_equilibrium_factory_init(self, factory_type)
      implicit none

      class(rhyme_ionisation_equilibrium_factory_t), intent(inout) :: self
      character(len=*), intent(in) :: factory_type

      self%factory_type = trim(factory_type)
   end subroutine rhyme_ionisation_equilibrium_factory_init

   function rhyme_ionisation_equilibrium_factory_generate(self) result(ie)
      implicit none

      class(rhyme_ionisation_equilibrium_factory_t), intent(inout) :: self
      type(ionisation_equilibrium_t) :: ie

      if (self%factory_type == 'CaseA') then
         ie = ionisation_equilibrium_t(ieid%case_a, .false., .false., .false.)
      else if (self%factory_type == 'CaseB') then
         ie = ionisation_equilibrium_t(ieid%case_b, .false., .false., .false.)
      else
         ie = ionisation_equilibrium_t()
      end if
   end function rhyme_ionisation_equilibrium_factory_generate

   subroutine rhyme_ionisation_equilibrium_factory_final(self)
      implicit none

      class(rhyme_ionisation_equilibrium_factory_t), intent(inout) :: self

      self%factory_type = ''
   end subroutine rhyme_ionisation_equilibrium_factory_final

end module rhyme_ionisation_equilibrium_factory
