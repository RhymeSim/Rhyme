submodule ( rhyme_physics ) init_smod
contains
  module subroutine rhyme_physics_init ( physics, logger )
    implicit none

    type ( physics_t ), intent ( inout ) :: physics
    type ( logger_t ), intent ( inout ) :: logger

    type ( nombre_unit_t ), pointer :: kb_unit, r_unit, amu_unit

    call logger%begin_section( 'physics' )

    call logger%log( '', '# of components', '=', [ NCMP ] )
    call logger%log( '', 'component labels', '=', cid%labels )

    physics%rho => .parse. physics%rho_str
    call logger%log( '', 'rho:', '[ '//trim( .printchain. physics%rho )//' ]' )

    physics%length => .parse. physics%length_str
    call logger%log( '', 'length:', '[ '//trim( .printchain. physics%length )//' ]' )

    physics%time => .parse. physics%time_str
    call logger%log( '', 'time:', '[ '//trim( .printchain. physics%time )//' ]' )

    physics%velocity => physics%length / physics%time
    call logger%log( '', 'velocity', '[ '//trim( .printchain. physics%velocity )//' ]' )

    physics%pressure => physics%rho * physics%length**2 / physics%time**2
    call logger%log( '', 'pressure:', '[ '//trim( .printchain. physics%pressure )//' ]' )

    physics%temperature => 1 * kelvin
    call logger%log( '', 'temperature:', '[ '//trim( .printchain. physics%temperature )//' ]' )

    kb_unit => .parse. kb_unit_str

    physics%kb = kb_value .unit. kb_unit &
      .to. physics%rho * physics%length**5 / ( physics%time**2 * kelvin )
    call logger%log( 'kB = ', physics%kb%v, '[ '//trim( .printchain.  physics%kb%u )//' ]' )

    r_unit => .parse. r_unit_str

    physics%r = r_value .unit. r_unit &
      .to. physics%rho * physics%length**5 / ( physics%time**2 * mole * physics%temperature )
    call logger%log( 'R = ', physics%r%v, '[ '//trim( .printchain.  physics%r%u )//' ]' )

    amu_unit => .parse. amu_unit_str

    physics%amu = amu_value .u. amu_unit .to. physics%rho * physics%length**3
    call logger%log( '1 amu = ', physics%amu%v, '[ '//trim( .printchain.  physics%amu%u )//' ]' )

    call logger%end_section
  end subroutine rhyme_physics_init
end submodule init_smod
