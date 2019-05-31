module rhyme_physics_factory
  use rhyme_physics

  implicit none

  character ( len=32 ), parameter, private :: rho_str_param = 'kg / m^3'
  character ( len=32 ), parameter, private :: length_str_param = 'm'
  character ( len=32 ), parameter, private :: time_str_param = 's'


  type rhyme_physics_factory_t
    character ( len=32 ) :: rho_str = rho_str_param
    character ( len=32 ) :: length_str = length_str_param
    character ( len=32 ) :: time_str = time_str_param
    logical :: initialized = .false.
    type ( nombre_unit_t ), pointer :: rho => null()
    type ( nombre_unit_t ), pointer :: length => null()
    type ( nombre_unit_t ), pointer :: time => null()
    type ( nombre_unit_t ), pointer :: velocity => null()
    type ( nombre_unit_t ), pointer :: pressure => null()
    type ( nombre_unit_t ), pointer :: temperature => null()
  contains
    procedure :: init => rhyme_physics_factory_init
    procedure :: generate => rhyme_physics_factory_generate
  end type rhyme_physics_factory_t

  type ( rhyme_physics_factory_t ) :: ph_factory = rhyme_physics_factory_t()

contains

  subroutine rhyme_physics_factory_init ( this )
    implicit none

    class ( rhyme_physics_factory_t ), intent ( inout ) :: this

    this%rho => rhyme_nombre_units_parse( this%rho_str )
    this%length => rhyme_nombre_units_parse( this%length_str )
    this%time => rhyme_nombre_units_parse( this%time_str )
    this%velocity => this%length / this%time
    this%pressure => this%rho * this%length**2 / this%time**2
    this%temperature => rhyme_nombre_unit_clone( kel )

    this%initialized = .true.
  end subroutine rhyme_physics_factory_init


  function rhyme_physics_factory_generate ( this ) result ( physics )
    implicit none

    class ( rhyme_physics_factory_t ), intent ( inout ) :: this
    type ( physics_t ) :: physics

    if ( .not. this%initialized ) call this%init

    physics%rho_str = this%rho_str
    physics%length_str = this%length_str
    physics%time_str = this%time_str

  end function rhyme_physics_factory_generate
end module rhyme_physics_factory
