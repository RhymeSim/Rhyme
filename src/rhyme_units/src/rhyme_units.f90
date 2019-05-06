module rhyme_units
  use rhyme_nombre
  use rhyme_log

  implicit none

  type rhyme_units_t
    character ( len=1024 ) :: rho_str, length_str, time_str
    type ( nombre_unit_t ), pointer :: rho => null()
    type ( nombre_unit_t ), pointer :: length => null()
    type ( nombre_unit_t ), pointer :: time => null()
    type ( nombre_unit_t ), pointer :: pressure => null()
    type ( nombre_unit_t ), pointer :: temperature => null()
  end type rhyme_units_t

contains

  module subroutine rhyme_units_init ( units, logger )
    implicit none

    type ( rhyme_units_t ), intent ( inout ) :: units
    type ( log_t ), intent ( inout ) :: logger

    call logger%set_sub_section( 'units' )

    units%rho => rhyme_nombre_units_parse( units%rho_str )
    call logger%log( '', 'rho:', '[ '//trim(units%rho%p())//' ]' )

    units%length => rhyme_nombre_units_parse( units%length_str )
    call logger%log( '', 'length:', '[ '//trim(units%length%p())//' ]' )

    units%time => rhyme_nombre_units_parse( units%time_str )
    call logger%log( '', 'time:', '[ '//trim(units%time%p())//' ]' )

    units%pressure => units%rho * units%length**2 / units%time**2
    call logger%log( '', 'pressure:', '[ '//trim(units%pressure%p())//' ]' )

    units%temperature => rhyme_nombre_unit_clone( kel )
    call logger%log( '', 'temperature:', '[ '//trim(units%temperature%p())//' ]' )

    call logger%set_sub_section( '' )
  end subroutine rhyme_units_init
end module rhyme_units
