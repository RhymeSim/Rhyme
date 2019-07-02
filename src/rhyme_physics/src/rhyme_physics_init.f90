submodule ( rhyme_physics ) init_smod
contains
  module subroutine rhyme_physics_init ( physics, logger )
    implicit none

    type ( physics_t ), intent ( inout ) :: physics
    type ( log_t ), intent ( inout ) :: logger

    type ( nombre_unit_t ), pointer :: kb_unit, r_unit, amu_unit

    call logger%begin_section( 'physics' )

    physics%rho => rhyme_nombre_units_parse( physics%rho_str )
    call logger%log( '', 'rho:', '[ '//trim(physics%rho%p())//' ]' )

    physics%length => rhyme_nombre_units_parse( physics%length_str )
    call logger%log( '', 'length:', '[ '//trim(physics%length%p())//' ]' )

    physics%time => rhyme_nombre_units_parse( physics%time_str )
    call logger%log( '', 'time:', '[ '//trim(physics%time%p())//' ]' )

    physics%velocity => physics%length / physics%time
    call logger%log( '', 'velocity', '[ '//trim(physics%velocity%p())//' ]' )

    physics%pressure => physics%rho * physics%length**2 / physics%time**2
    call logger%log( '', 'pressure:', '[ '//trim(physics%pressure%p())//' ]' )

    physics%temperature => rhyme_nombre_unit_clone( kel )
    call logger%log( '', 'temperature:', '[ '//trim(physics%temperature%p())//' ]' )

    kb_unit => rhyme_nombre_units_parse( kb_unit_str )

    physics%kb = kb_value .unit. kb_unit &
      .to. physics%rho * physics%length**5 / ( physics%time**2 * kel )
    call logger%log( 'kB', physics%kb%v, '[ '//trim( physics%kb%u%p() )//' ]' )

    r_unit => rhyme_nombre_units_parse( r_unit_str )

    physics%r = r_value .unit. r_unit &
      .to. physics%rho * physics%length**5 / ( physics%time**2 * mol * physics%temperature )
    call logger%log( 'R', physics%r%v, '[ '//trim( physics%r%u%p() )//' ]' )

    amu_unit => rhyme_nombre_units_parse( amu_unit_str )

    physics%amu = amu_value .u. amu_unit .to. physics%rho * physics%length**3
    call logger%log( '1 amu', physics%amu%v, '[ '//trim( physics%amu%u%p() )//' ]' )

    call logger%end_section
  end subroutine rhyme_physics_init
end submodule init_smod
