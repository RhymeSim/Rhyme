module rhyme_thermo_base
  use rhyme_physics
  use rhyme_ideal_gas
  use rhyme_log

  implicit none

  type, private :: thermo_base_indices_t
    integer :: unset = -1
    ! TODO: integer :: on_the_fly = 0
    integer :: monatomic = 1
    integer :: diatomic = 2
    integer :: polyatomic = 3
  end type thermo_base_indices_t

  type ( thermo_base_indices_t ), parameter :: thid = thermo_base_indices_t()


  ! TODO: Make ig_gamma a private parameter
  real ( kind=8 ), parameter :: ig_gamma( thid%monatomic:thid%polyatomic ) = &
    [ 5.d0 / 3.d0, 7.d0 / 5.d0, 4.d0 / 3.d0 ]

  ! :(
  integer, private :: rhyme_thermo_base_state_of_matter = thid%unset
  real ( kind=8 ), private :: rhyme_thermo_base_kb_amu = 0.d0


  type thermo_base_t
    integer :: state_of_matter = thid%unset
  end type thermo_base_t


  interface
    pure module function rhyme_thermo_base_get_gamma () result ( g )
      real ( kind=8 ) :: g
    end function rhyme_thermo_base_get_gamma

    pure module function rhyme_thermo_base_temperature_per_mu ( u ) result ( t_mu )
      real ( kind=8 ), intent ( in ) :: u( cid%rho:cid%e_tot )
      real ( kind=8 ) :: t_mu
    end function rhyme_thermo_base_temperature_per_mu

    pure module function rhyme_thermo_base_speed_of_sound ( u ) result ( cs )
      real ( kind=8 ), intent ( in ) :: u( cid%rho:cid%e_tot )
      real ( kind=8 ) :: cs
    end function rhyme_thermo_base_speed_of_sound

    pure module function rhyme_thermo_base_pressure ( u ) result ( p )
      real ( kind=8 ), intent ( in ) :: u( cid%rho:cid%e_tot )
      real ( kind=8 ) :: p
    end function rhyme_thermo_base_pressure

    pure module function rhyme_thermo_base_flux ( u, axis ) result ( f )
      real ( kind=8 ), intent ( in ) :: u( cid%rho:cid%e_tot )
      integer, intent ( in ) :: axis
      real ( kind=8 ) :: f( cid%rho:cid%e_tot )
    end function rhyme_thermo_base_flux

    pure module subroutine rhyme_thermo_base_primitive_to_conserved ( w, u )
      real ( kind=8 ), intent ( in ) :: w( cid%rho:cid%p )
      real ( kind=8 ), intent ( out ) :: u( cid%rho:cid%e_tot )
    end subroutine rhyme_thermo_base_primitive_to_conserved

    pure module subroutine rhyme_thermo_base_primitive_vars_to_conserved ( rho, v, p, u )
      real ( kind=8 ), intent ( in ) :: rho
      real ( kind=8 ), intent ( in ) :: v( NDIM )
      real ( kind=8 ), intent ( in ) :: p
      real ( kind=8 ), intent ( out ) :: u( cid%rho:cid%e_tot )
    end subroutine rhyme_thermo_base_primitive_vars_to_conserved

    pure module function rhyme_thermo_base_specific_internal_energy ( u ) result ( sp_int_e )
      real ( kind=8 ), intent ( in ) :: u( cid%rho:cid%e_tot )
      real ( kind=8 ) :: sp_int_e
    end function rhyme_thermo_base_specific_internal_energy
  end interface


  interface calc_t_mu
    module procedure :: rhyme_thermo_base_temperature_per_mu
  end interface calc_t_mu

  interface calc_cs
    module procedure :: rhyme_thermo_base_speed_of_sound
  end interface calc_cs

  interface calc_p
    module procedure :: rhyme_thermo_base_pressure
  end interface calc_p

  interface calc_flux
    module procedure :: rhyme_thermo_base_flux
  end interface calc_flux

  interface conv_prim_to_cons
    module procedure :: rhyme_thermo_base_primitive_to_conserved
  end interface conv_prim_to_cons

  interface conv_prim_vars_to_cons
    module procedure :: rhyme_thermo_base_primitive_vars_to_conserved
  end interface conv_prim_vars_to_cons

  interface get_gamma
    module procedure :: rhyme_thermo_base_get_gamma
  end interface get_gamma

  interface calc_sp_int_e
    module procedure :: rhyme_thermo_base_specific_internal_energy
  end interface calc_sp_int_e

contains
  module subroutine rhyme_thermo_base_init ( thermo, physics, logger )
    implicit none

    type ( thermo_base_t ), intent ( in ) :: thermo
    type ( physics_t ), intent ( in ) :: physics
    type ( log_t ), intent ( inout ) :: logger

    call logger%begin_section( 'thermo_base' )

    rhyme_thermo_base_state_of_matter = thermo%state_of_matter
    call logger%log( '', 'state_of_matter', '=', [ rhyme_thermo_base_state_of_matter ] )

    rhyme_thermo_base_kb_amu = physics%kb%v / physics%amu%v
    call logger%log( '', 'kB / 1 amu', '=', [ rhyme_thermo_base_kb_amu ] )

    call logger%end_section
  end subroutine rhyme_thermo_base_init
end module rhyme_thermo_base
