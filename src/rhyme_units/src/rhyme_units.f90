module rhyme_units
  use rhyme_nombre

  implicit none

  type rhyme_units_t
    character ( len=1024 ) :: rho_str, length_str, time_str
    type ( nombre_unit_t ), pointer :: rho => null()
    type ( nombre_unit_t ), pointer :: length => null()
    type ( nombre_unit_t ), pointer :: time => null()
    type ( nombre_unit_t ), pointer :: pressure => null()
    type ( nombre_unit_t ), pointer :: temperature => null()
  contains
    procedure :: init => rhyme_units_init
  end type rhyme_units_t

contains

  module subroutine rhyme_units_init ( this )
    implicit none

    class ( rhyme_units_t ), intent ( inout ) :: this

    this%rho => rhyme_nombre_units_parse( this%rho_str )
    this%length => rhyme_nombre_units_parse( this%length_str )
    this%time => rhyme_nombre_units_parse( this%time_str )

    this%pressure => this%rho * this%length**2 / this%time**2

    this%temperature => rhyme_nombre_unit_clone( kel )
  end subroutine rhyme_units_init
end module rhyme_units
