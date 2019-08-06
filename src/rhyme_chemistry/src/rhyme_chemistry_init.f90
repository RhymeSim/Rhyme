submodule ( rhyme_chemistry ) init_smod
contains
  module subroutine rhyme_chemistry_init ( chemistry, physics, logger )
    implicit none

    class ( chemistry_t ), intent ( inout ) :: chemistry
    type ( physics_t ), intent ( in ) :: physics
    type ( logger_t ), intent ( inout ) :: logger

    call logger%begin_section( 'chemistry' )

    chemistry%molar%e = 5.48580d-7 .u. kg / mol .to. physics%rho * physics%length**3 / mol
    chemistry%molar%H = 1.00794d-3 .u. kg / mol .to. physics%rho * physics%length**3 / mol
    chemistry%molar%He = 4.002602d-3 .u. kg / mol .to. physics%rho * physics%length**3 / mol
    call logger%log( 'e ( molar mass )', chemistry%molar%e%v, '[ '//trim( .printUnit. chemistry%molar%e%u )//' ]' )
    call logger%log( 'H ( molar mass )', chemistry%molar%H%v, '[ '//trim( .printUnit. chemistry%molar%H%u )//' ]' )
    call logger%log( 'He ( molar mass )', chemistry%molar%He%v, '[ '//trim( .printUnit. chemistry%molar%He%u )//' ]' )

    chemistry%atomic%e = 9.1093835d-31 .u. kg .to. physics%rho * physics%length**3
    chemistry%atomic%H = 1.6737236d-27 .u. kg .to. physics%rho * physics%length**3
    chemistry%atomic%He = 6.6464764d-27 .u. kg .to. physics%rho * physics%length**3
    call logger%log( 'e ( atomic mass )', chemistry%atomic%e%v, '[ '//trim( .printUnit. chemistry%atomic%e%u )//' ]' )
    call logger%log( 'H ( atomic mass )', chemistry%atomic%H%v, '[ '//trim( .printUnit. chemistry%atomic%H%u )//' ]' )
    call logger%log( 'He ( atomic mass )', chemistry%atomic%He%v, '[ '//trim( .printUnit. chemistry%atomic%He%u )//' ]' )

    chemistry%amu%e = 5.48580d-4
    chemistry%amu%H = 1.00794d0
    chemistry%amu%He = 4.002602d0
    call logger%log( 'e [ amu ]', chemistry%amu%e )
    call logger%log( 'H [ amu ]', chemistry%amu%H )
    call logger%log( 'He [ amu ]', chemistry%amu%He )

    call logger%end_section
  end subroutine rhyme_chemistry_init
end submodule init_smod
