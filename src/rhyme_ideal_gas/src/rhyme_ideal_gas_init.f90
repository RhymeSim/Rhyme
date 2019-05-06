submodule ( rhyme_ideal_gas ) rhyme_ideal_gas_init_smod
contains

  module subroutine rhyme_ideal_gas_init ( this, chemi, thermo, units, logger )
    implicit none

    class ( ideal_gas_t ), intent ( inout ) :: this
    type ( chemistry_t ), intent ( in ) :: chemi
    type ( thermo_base_t ), intent ( in ) :: thermo
    type ( rhyme_units_t ), intent ( in ) :: units
    type ( log_t ), intent ( inout ) :: logger

    call logger%set_section( 'ideal_gas' )

    if ( this%initialized ) call logger%warn( 'Try to re-initialize ideal gas object' )

    this%R = nombre_t( 8.314d0, kg * (meter / sec)**2 / mol / Kel )
    this%R = this%R .to. units%rho * units%length**5 / ( units%time**2 * mol * units%temperature )
    call logger%log( 'R', this%R%v, '[ '//trim( this%R%u%p() )//' ]' )

    this%kB__amu = thermo%kB%v / chemi%amu%one%v
    call logger%log( 'kB / amu', this%KB__amu )

    if ( this%type .eq. igid%monatomic ) then
      this%Cv = 3.d0 / 2.d0 * this%R
      this%Cp = 5.d0 / 2.d0 * this%R
      this%beta = 3 ! Degrees of freedom
    else if ( this%type .eq. igid%diatomic ) then
      this%Cv = 5.d0 / 2.d0 * this%R
      this%Cp = 7.d0 / 2.d0 * this%R
      this%beta = 5
    else if ( this%type .eq. igid%polyatomic ) then
      this%Cv = 3.d0 * this%R
      this%Cp = 4.d0 * this%R
      this%beta = 6
    end if

    call logger%log( 'Cv', this%Cv%v, '[ '//trim( this%Cv%u%p() )//' ]' )
    call logger%log( 'Cp', this%Cp%v, '[ '//trim( this%Cp%u%p() )//' ]' )
    call logger%log( 'Beta', this%beta )

    this%gamma = this%Cp%v / this%Cv%v
    call logger%log( 'gamma', this%gamma )

    this%gm1 = this%gamma - 1.d0
    this%gp1 = this%gamma + 1.d0
    this%gm1_gp1 = ( this%gamma - 1.d0 ) / ( this%gamma + 1.d0 )
    this%gm1_2g = ( this%gamma - 1.d0 ) / ( 2 * this%gamma )
    this%gp1_2g = ( this%gamma + 1.d0 ) / ( 2 * this%gamma )
    this%g_inv = 1.d0 / this%gamma

    this%initialized = .true.

    call logger%set_section( '' )
  end subroutine rhyme_ideal_gas_init
end submodule rhyme_ideal_gas_init_smod
