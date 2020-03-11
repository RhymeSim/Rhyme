module rhyme_thermo_base_factory
   use rhyme_thermo_base

   implicit none

   integer, parameter, private :: som = thid%diatomic

   type rhyme_thermo_base_factory_t
      integer :: state_of_matter = som
      real(kind=8) :: kb_amu = 0.d0
      logical :: initialized = .false.
   contains
      procedure :: init => rhyme_thermo_base_factory_init
      procedure :: generate => rhyme_thermo_base_factory_generate
   end type rhyme_thermo_base_factory_t

   type(rhyme_thermo_base_factory_t) :: th_factory = rhyme_thermo_base_factory_t()

contains

   subroutine rhyme_thermo_base_factory_init(this, physics)
      implicit none

      class(rhyme_thermo_base_factory_t), intent(inout) :: this
      type(physics_t), intent(in) :: physics

      this%state_of_matter = som
      this%kb_amu = physics%kb%v/physics%amu%v

      this%initialized = .true.
   end subroutine rhyme_thermo_base_factory_init

   function rhyme_thermo_base_factory_generate(this, physics, gas_type) result(thermo)
      implicit none

      class(rhyme_thermo_base_factory_t), intent(inout) :: this
      type(physics_t), intent(in) :: physics
      integer, intent(in), optional :: gas_type
      type(thermo_base_t) :: thermo

      if (.not. this%initialized) call this%init(physics)

      if (present(gas_type)) then
         thermo%state_of_matter = gas_type
      else
         thermo%state_of_matter = this%state_of_matter
      end if
   end function rhyme_thermo_base_factory_generate
end module rhyme_thermo_base_factory
