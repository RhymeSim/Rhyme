submodule ( rhyme_physics ) init_smod
contains
  module subroutine rhyme_physics_init ( physics, logger )
    implicit none

    type ( physics_t ), intent ( inout ) :: physics
    type ( log_t ), intent ( inout ) :: logger

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

    call logger%end_section
  end subroutine rhyme_physics_init
end submodule init_smod
