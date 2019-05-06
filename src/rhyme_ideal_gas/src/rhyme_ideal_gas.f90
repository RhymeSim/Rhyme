module rhyme_ideal_gas
  use rhyme_units
  use rhyme_hydro_base
  use rhyme_chemistry
  use rhyme_thermo_base

  implicit none

  type ideal_gas_indices_t
    integer :: monatomic = 1
    integer :: diatomic = 2
    integer :: polyatomic = 3
  end type ideal_gas_indices_t

  type ( ideal_gas_indices_t ), parameter :: igid = ideal_gas_indices_t ()


  type ideal_gas_t
    integer :: type
    integer :: beta
    type ( nombre_t ) :: R, Cv, Cp
    real ( kind=8 ) :: gamma
    real ( kind=8 ) :: gm1, gp1, gm1_gp1, gm1_2g, gp1_2g, g_inv
    real ( kind=8 ) :: kB__amu
    logical :: initialized = .false.
  contains
    procedure :: T_per_mu => rhyme_ideal_gas_T_per_mu
    procedure :: T => rhyme_ideal_gas_T
    procedure :: Cs => rhyme_ideal_gas_sound_speed
    procedure :: p => rhyme_ideal_gas_pressure
    procedure :: B => rhyme_ideal_gas_bulk_modulus ! resistant to compression
    procedure :: e_int_sp => rhyme_ideal_gas_pecific_internal_energy
    procedure :: e_kin_sp => rhyme_ideal_gas_pecific_kinetic_energy
    procedure :: prim_vars_to_cons => rhyme_ideal_gas_primitive_vars_to_conserved
    procedure :: prim_to_cons => rhyme_ideal_gas_primitive_to_conserved
    procedure :: cons_to_prim => rhyme_ideal_gas_conserved_to_primitive
    procedure :: flux_at => rhyme_ideal_gas_flux_at
    procedure :: half_step_extrapolation => rhyme_ideal_gas_half_step_extrapolation
  end type ideal_gas_t

  interface
    module subroutine rhyme_ideal_gas_init ( this, chemi, thermo, units, logger )
      class ( ideal_gas_t ), intent ( inout ) :: this
      type ( chemistry_t ), intent ( in ) :: chemi
      type ( thermo_base_t ), intent ( in ) :: thermo
      type ( rhyme_units_t ), intent ( in ) :: units
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_ideal_gas_init
  end interface

contains

  pure function rhyme_ideal_gas_T_per_mu ( this, U ) result ( T_per_mu )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_conserved_t ), intent ( in ) :: U

    real ( kind=8 ) :: T_per_mu

    T_per_mu = hy_sp_internal_e(U) * ( this%gamma - 1.d0 ) / this%kB__amu
  end function rhyme_ideal_gas_T_per_mu


  pure function rhyme_ideal_gas_T ( this, U, mean_atomic_weight ) result ( T )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_conserved_t ), intent ( in ) :: U
    real ( kind=8 ), intent ( in ) :: mean_atomic_weight

    real ( kind=8 ) :: T

    T = this%T_per_mu(U) * mean_atomic_weight
  end function rhyme_ideal_gas_T


  pure function rhyme_ideal_gas_sound_speed ( this, U ) result ( Cs )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_conserved_t ), intent ( in ) :: U

    real ( kind=8 ) :: Cs

    Cs = sqrt( this%gamma * this%kB__amu * this%T_per_mu(U) )
  end function rhyme_ideal_gas_sound_speed


  pure function rhyme_ideal_gas_pressure ( this, U ) result ( p )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_conserved_t ), intent ( in ) :: U

    real ( kind=8 ) :: p

    p = U%u( hyid%rho ) * this%kB__amu * this%T_per_mu(U)
  end function rhyme_ideal_gas_pressure


  pure function rhyme_ideal_gas_bulk_modulus ( this, U ) result ( B )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_conserved_t ), intent ( in ) :: U

    real ( kind=8 ) :: B

    B = this%gamma * this%p(U)
  end function rhyme_ideal_gas_bulk_modulus


  pure function rhyme_ideal_gas_pecific_internal_energy ( this, U ) result ( e_int_sp )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_conserved_t ), intent ( in ) :: u

    real ( kind=8 ) :: e_int_sp

    e_int_sp = this%kB__amu * this%T_per_mu(U) / ( this%gamma - 1.d0 )
  end function rhyme_ideal_gas_pecific_internal_energy


  pure function rhyme_ideal_gas_pecific_kinetic_energy ( this, U ) result ( e_kin_sp )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_conserved_t ), intent ( in ) :: U

    real ( kind=8 ) :: e_kin_sp

    e_kin_sp = U%u( hyid%e_tot ) / U%u( hyid%rho ) - this%e_int_sp(U)
  end function rhyme_ideal_gas_pecific_kinetic_energy


  pure subroutine rhyme_ideal_gas_primitive_to_conserved ( this, prim, cons )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_primitive_t ), intent ( in ) :: prim
    type ( hydro_conserved_t ), intent ( out ) :: cons

    real ( kind=8 ) :: rho, u, v, w, p

    rho = prim%w ( hyid%rho )
    u = prim%w( hyid%u )
    v = prim%w( hyid%v )
    w = prim%w( hyid%w )
    p = prim%w( hyid%p )

    call this%prim_vars_to_cons(rho, u, v, w, p, cons)
  end subroutine rhyme_ideal_gas_primitive_to_conserved


  pure subroutine rhyme_ideal_gas_primitive_vars_to_conserved ( &
    this, rho, u, v, w, p, cons )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    real ( kind=8 ), intent ( in ) :: rho, u, v, w, p
    type ( hydro_conserved_t ), intent ( out ) :: cons

    cons%u( hyid%rho ) = rho
    cons%u( hyid%rho_u ) = cons%u( hyid%rho ) * u
    cons%u( hyid%rho_v ) = cons%u( hyid%rho ) * v
    cons%u( hyid%rho_w ) = cons%u( hyid%rho ) * w
    cons%u( hyid%e_tot ) = 0.5d0 * cons%u( hyid%rho ) * ( u**2 + v**2 + w**2 ) &
      + p / ( this%gamma - 1.d0 )
  end subroutine rhyme_ideal_gas_primitive_vars_to_conserved


  pure subroutine rhyme_ideal_gas_conserved_to_primitive ( this, cons, prim )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_conserved_t ), intent ( in ) :: cons
    type ( hydro_primitive_t ), intent ( out ) :: prim

    prim%w( hyid%rho ) = cons%u( hyid%rho )
    prim%w( hyid%u ) = cons%u( hyid%rho_u ) / prim%w( hyid%rho )
    prim%w( hyid%v ) = cons%u( hyid%rho_v ) / prim%w( hyid%rho )
    prim%w( hyid%w ) = cons%u( hyid%rho_w ) / prim%w( hyid%rho )
    prim%w( hyid%p ) = this%p( cons )
  end subroutine rhyme_ideal_gas_conserved_to_primitive


  pure subroutine rhyme_ideal_gas_flux_at ( this, U, dir, F )
    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_conserved_t ), intent ( in ) :: U
    integer, intent ( in ) :: dir
    type ( hydro_flux_t ), intent ( out ) :: F

    if ( U%u( hyid%rho ) < tiny(0.d0) ) then
      F%f = 0.d0
    else
      F%f( hyid%rho ) = U%u( hyid%rho_vel(dir) )

      F%f( hyid%rho_u:hyid%rho_w ) = U%u( hyid%rho_vel(dir) ) &
        * U%u( hyid%rho_u:hyid%rho_w ) / U%u( hyid%rho )
      F%f( hyid%rho_vel(dir) ) = F%f( hyid%rho_vel(dir) ) + this%p(U)

      F%f( hyid%e_tot ) = U%u( hyid%rho_vel(dir) ) / U%u( hyid%rho ) &
        * ( U%u( hyid%e_tot ) + this%p(U) )
    end if
  end subroutine rhyme_ideal_gas_flux_at


  pure subroutine rhyme_ideal_gas_half_step_extrapolation ( &
    this, U, Delta, dir, dx, dt, L, R )
    ! TODO: Move this procedure to MH module

    implicit none

    class ( ideal_gas_t ), intent ( in ) :: this
    type ( hydro_conserved_t ), intent ( in ) :: U, Delta
    integer, intent ( in ) :: dir
    real ( kind=8 ), intent ( in ) :: dx, dt
    type ( hydro_conserved_t ), intent ( inout ) :: L, R

    type ( hydro_flux_t ) :: FL, FR, dF

    L%u = U%u - .5d0 * Delta%u
    R%u = U%u + .5d0 * Delta%u

    call this%flux_at( L, dir, FL )
    call this%flux_at( R, dir, FR )

    dF%f = FL%f - FR%f

    L%u = L%u + .5d0 * dt / dx * dF%f
    R%u = R%u + .5d0 * dt / dx * dF%f
  end subroutine rhyme_ideal_gas_half_step_extrapolation
end module rhyme_ideal_gas
