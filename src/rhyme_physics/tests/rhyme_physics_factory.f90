module rhyme_physics_factory
   use rhyme_physics

   implicit none

   character(len=32), parameter, private :: rho_str_param = 'kg / m^3'
   character(len=32), parameter, private :: length_str_param = 'm'
   character(len=32), parameter, private :: time_str_param = 's'

   type rhyme_physics_factory_t
      character(len=32) :: rho_str = rho_str_param
      character(len=32) :: length_str = length_str_param
      character(len=32) :: time_str = time_str_param
      logical :: initialized = .false.
      type(nombre_unit_t), pointer :: rho => null()
      type(nombre_unit_t), pointer :: length => null()
      type(nombre_unit_t), pointer :: time => null()
      type(nombre_unit_t), pointer :: velocity => null()
      type(nombre_unit_t), pointer :: pressure => null()
      type(nombre_unit_t), pointer :: temperature => null()
      type(nombre_unit_t), pointer :: kb_unit => null()
      type(nombre_unit_t), pointer :: r_unit => null()
      type(nombre_unit_t), pointer :: amu_unit => null()
   contains
      procedure :: init => rhyme_physics_factory_init
      procedure :: generate => rhyme_physics_factory_generate
   end type rhyme_physics_factory_t

   type(rhyme_physics_factory_t) :: ph_factory = rhyme_physics_factory_t()

contains

   subroutine rhyme_physics_factory_init(this)
      implicit none

      class(rhyme_physics_factory_t), intent(inout) :: this

      this%rho => .parse.this%rho_str
      this%length => .parse.this%length_str
      this%time => .parse.this%time_str
      this%velocity => this%length/this%time
      this%pressure => this%rho*this%length**2/this%time**2
      this%temperature => 1*kelvin

      this%kb_unit => this%rho*this%length**5/(this%time**2*kelvin)
      this%r_unit => this%rho*this%length**5/(this%time**2*mole*this%temperature)
      this%amu_unit => this%rho*this%length**3

      this%initialized = .true.
   end subroutine rhyme_physics_factory_init

   function rhyme_physics_factory_generate(this) result(physics)
      implicit none

      class(rhyme_physics_factory_t), intent(inout) :: this
      type(physics_t) :: physics

      if (.not. this%initialized) call this%init

      physics%rho_str = this%rho_str
      physics%length_str = this%length_str
      physics%time_str = this%time_str

      physics%rho => this%rho
      physics%length => this%length
      physics%time => this%time
      physics%velocity => this%velocity
      physics%pressure => this%pressure
      physics%temperature => this%temperature

      physics%kb = 1.38064852e-23.unit.meter**2*kilogram/(second**2*kelvin) &
                   .to.this%kb_unit
      physics%r = 8.314462618.unit.kilogram*(meter/second)**2/(mole*kelvin) &
                  .to.this%r_unit
      physics%amu = 1.6605e-27.u.kilogram.to.this%amu_unit
   end function rhyme_physics_factory_generate
end module rhyme_physics_factory
