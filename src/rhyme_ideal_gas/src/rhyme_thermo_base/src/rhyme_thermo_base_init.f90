submodule ( rhyme_thermo_base ) rhyme_thermo_base_init_smod
contains
  module subroutine rhyme_thermo_base_init ( this, units, logger )
    implicit none

    type ( thermo_base_t ), intent ( inout ) :: this
    type ( rhyme_units_t ), intent ( in ) :: units
    type ( log_t ), intent ( inout ) :: logger

    call logger%set_section( 'thermo_base' )

    if ( this%initialized ) call logger%warn( "Try to re-initialize thermo object" )

    this%kB = 1.38064852d-23 .u. meter**2 * kg / ( sec**2 * kel ) &
      .to. units%rho * units%length**5 / ( units%time**2 * kel )
    call logger%log( 'kB', this%kB%v, '[ '//trim( this%kB%u%p() )//' ]' )

    this%initialized = .true.

    call logger%set_section( '' )
  end subroutine rhyme_thermo_base_init
end submodule rhyme_thermo_base_init_smod
