submodule ( rhyme_chemistry ) rhyme_chemistry_init_smod
contains
  module subroutine rhyme_chemistry_init ( chemistry, units, logger )
    implicit none

    class ( chemistry_t ), intent ( inout ) :: chemistry
    type ( rhyme_units_t ), intent ( in ) :: units
    type ( log_t ), intent ( inout ) :: logger

    call logger%set_section( 'chemistry' )

    if ( chemistry%initialized ) call logger%warn( 'Try to re-initialize chemistry object')

    chemistry%molar%e = 5.48580d-7 .u. kg / mol .to. units%rho * units%length**3 / mol
    chemistry%molar%H = 1.00794d-3 .u. kg / mol .to. units%rho * units%length**3 / mol
    chemistry%molar%He = 4.002602d-3 .u. kg / mol .to. units%rho * units%length**3 / mol
    call logger%log( 'e ( molar mass )', chemistry%molar%e%v, '[ '//trim(chemistry%molar%e%u%p())//' ]' )
    call logger%log( 'H ( molar mass )', chemistry%molar%H%v, '[ '//trim(chemistry%molar%H%u%p())//' ]' )
    call logger%log( 'He ( molar mass )', chemistry%molar%He%v, '[ '//trim(chemistry%molar%He%u%p())//' ]' )

    chemistry%atomic%e = 9.1093835d-31 .u. kg .to. units%rho * units%length**3
    chemistry%atomic%H = 1.6737236d-27 .u. kg .to. units%rho * units%length**3
    chemistry%atomic%He = 6.6464764d-27 .u. kg .to. units%rho * units%length**3
    call logger%log( 'e ( atomic mass )', chemistry%atomic%e%v, '[ '//trim(chemistry%atomic%e%u%p())//' ]' )
    call logger%log( 'H ( atomic mass )', chemistry%atomic%H%v, '[ '//trim(chemistry%atomic%H%u%p())//' ]' )
    call logger%log( 'He ( atomic mass )', chemistry%atomic%He%v, '[ '//trim(chemistry%atomic%He%u%p())//' ]' )

    chemistry%amu%one = 1.66054d-27 .u. kg .to. units%rho * units%length**3
    call logger%log( '1 amu', chemistry%amu%one%v, '[ '//trim(chemistry%amu%one%u%p())//' ]' )

    chemistry%amu%e = 5.48580d-4
    chemistry%amu%H = 1.00794d0
    chemistry%amu%He = 4.002602d0
    call logger%log( 'e [ amu ]', chemistry%amu%e )
    call logger%log( 'H [ amu ]', chemistry%amu%H )
    call logger%log( 'He [ amu ]', chemistry%amu%He )

    chemistry%initialized = .true.

    call logger%set_section( '' )
  end subroutine rhyme_chemistry_init
end submodule rhyme_chemistry_init_smod
