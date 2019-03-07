module rhyme_ideal_gas
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
    type ( nombre_t ) :: R, Cv, Cp
    real ( kind=8 ) :: gamma
    real ( kind=8 ) :: gm1, gp1, gm1_gp1, gm1_2g, gp1_2g, g_inv
    integer :: type
    integer :: beta
    real ( kind=8 ) :: kB_per_amu ! kB / 1 amu
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_ideal_gas_init
    procedure :: init_with => rhyme_ideal_gas_init_with
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

contains

  subroutine rhyme_ideal_gas_init_with ( this, chemi, thermo, gastype, log )
    implicit none

    class ( ideal_gas_t ), intent ( inout ) :: this
    type ( chemistry_t ), intent ( in ) :: chemi
    type ( thermo_base_t ), intent ( in ) :: thermo
    integer, intent ( in ) :: gastype
    type ( log_t ), intent ( inout ) :: log


    if ( this%initialized ) then
      call log%warn( 'Try to re-initialize ideal gas object')
      return
    end if

    this%type = gastype

    call rhyme_ideal_gas_init ( this, chemi, thermo, log )
  end subroutine rhyme_ideal_gas_init_with


  subroutine rhyme_ideal_gas_init ( this, chemi, thermo, log )
    implicit none

    class ( ideal_gas_t ), intent ( inout ) :: this
    type ( chemistry_t ), intent ( in ) :: chemi
    type ( thermo_base_t ), intent ( in ) :: thermo
    type ( log_t ), intent ( inout ) :: log


    if ( this%initialized ) then
      call log%warn( 'Try to re-initialize ideal gas object' )
      return
    end if

    this%R = nombre_t(8.314d0, kg * (m / s)**2 / mol / Kel)
    this%kB_per_amu = thermo%kB%v / chemi%amu%one%v

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

    this%gamma = this%Cp%v / this%Cv%v

    this%gm1 = this%gamma - 1.d0
    this%gp1 = this%gamma + 1.d0
    this%gm1_gp1 = ( this%gamma - 1.d0 ) / ( this%gamma + 1.d0 )
    this%gm1_2g = ( this%gamma - 1.d0 ) / ( 2 * this%gamma )
    this%gp1_2g = ( this%gamma + 1.d0 ) / ( 2 * this%gamma )
    this%g_inv = 1.d0 / this%gamma

    this%initialized = .true.
  end subroutine rhyme_ideal_gas_init


  pure function rhyme_ideal_gas_T_per_mu (this, U) result (T_per_mu)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    type(hydro_conserved_t), intent(in) :: U

    real(kind=8) :: T_per_mu

    T_per_mu = hy_sp_internal_e(U) * ( this%gamma - 1.d0 ) / this%kB_per_amu
  end function rhyme_ideal_gas_T_per_mu


  pure function rhyme_ideal_gas_T (this, U, mean_atomic_weight) result (T)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    type(hydro_conserved_t), intent(in) :: U
    real(kind=8), intent(in) :: mean_atomic_weight

    real(kind=8) :: T

    T = this%T_per_mu(U) * mean_atomic_weight
  end function rhyme_ideal_gas_T


  pure function rhyme_ideal_gas_sound_speed (this, U) result (Cs)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    type(hydro_conserved_t), intent(in) :: U

    real(kind=8) :: Cs

    Cs = sqrt(this%gamma * this%kB_per_amu * this%T_per_mu(U))
  end function rhyme_ideal_gas_sound_speed


  pure function rhyme_ideal_gas_pressure (this, U) result (p)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    type(hydro_conserved_t), intent(in) :: U

    real(kind=8) :: p

    p = U%u(hyid%rho) * this%kB_per_amu * this%T_per_mu(U)
  end function rhyme_ideal_gas_pressure


  pure function rhyme_ideal_gas_bulk_modulus (this, U) result (B)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    type(hydro_conserved_t), intent(in) :: U

    real(kind=8) :: B

    B = this%gamma * this%p(U)
  end function rhyme_ideal_gas_bulk_modulus


  pure function rhyme_ideal_gas_pecific_internal_energy (this, U) result (e_int_sp)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    type(hydro_conserved_t), intent(in) :: u

    real(kind=8) :: e_int_sp

    e_int_sp = this%kB_per_amu * this%T_per_mu(U) / ( this%gamma - 1.d0 )
  end function rhyme_ideal_gas_pecific_internal_energy


  pure function rhyme_ideal_gas_pecific_kinetic_energy (this, U) result (e_kin_sp)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    type(hydro_conserved_t), intent(in) :: U

    real(kind=8) :: e_kin_sp

    e_kin_sp = U%u(hyid%e_tot) / U%u(hyid%rho) - this%e_int_sp(U)
  end function rhyme_ideal_gas_pecific_kinetic_energy


  pure subroutine rhyme_ideal_gas_primitive_to_conserved (this, prim, cons)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    type(hydro_primitive_t), intent(in) :: prim
    type(hydro_conserved_t), intent(out) :: cons

    real(kind=8) :: rho, u, v, w, p

    rho = max ( prim%w(hyid%rho), epsilon(0.d0) )
    u = prim%w(hyid%u)
    v = prim%w(hyid%v)
    w = prim%w(hyid%w)
    p = max ( prim%w(hyid%p), epsilon(0.d0) )

    call this%prim_vars_to_cons(rho, u, v, w, p, cons)
  end subroutine rhyme_ideal_gas_primitive_to_conserved


  pure subroutine rhyme_ideal_gas_primitive_vars_to_conserved (this, rho, u, v, w, p, cons)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    real(kind=8), intent(in) :: rho, u, v, w, p
    type(hydro_conserved_t), intent(out) :: cons

    cons%u(hyid%rho) = max ( rho, epsilon(0.d0) )
    cons%u(hyid%rho_u) = cons%u(hyid%rho) * u
    cons%u(hyid%rho_v) = cons%u(hyid%rho) * v
    cons%u(hyid%rho_w) = cons%u(hyid%rho) * w
    cons%u(hyid%e_tot) = 0.5d0 * cons%u(hyid%rho) * (u**2 + v**2 + w**2) + p / ( this%gamma - 1.d0 )
  end subroutine rhyme_ideal_gas_primitive_vars_to_conserved


  pure subroutine rhyme_ideal_gas_conserved_to_primitive (this, cons, prim)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    type(hydro_conserved_t), intent(in) :: cons
    type(hydro_primitive_t), intent(out) :: prim

    prim%w(hyid%rho) = max ( cons%u(hyid%rho), epsilon(0.d0) )
    prim%w(hyid%u) = cons%u(hyid%rho_u) / prim%w(hyid%rho)
    prim%w(hyid%v) = cons%u(hyid%rho_v) / prim%w(hyid%rho)
    prim%w(hyid%w) = cons%u(hyid%rho_w) / prim%w(hyid%rho)
    prim%w(hyid%p) = max ( this%p(cons), epsilon(0.d0) )
  end subroutine rhyme_ideal_gas_conserved_to_primitive


  pure subroutine rhyme_ideal_gas_flux_at (this, U, dir, F)
    implicit none

    class(ideal_gas_t), intent(in) :: this
    type(hydro_conserved_t), intent(in) :: U
    integer, intent(in) :: dir
    type(hydro_flux_t), intent(out) :: F

    F%f(hyid%rho) = U%u(hyid%rho_vel(dir))

    F%f(hyid%rho_u:hyid%rho_w) = U%u(hyid%rho_vel(dir)) * U%u(hyid%rho_u:hyid%rho_w) / U%u(hyid%rho)
    F%f(hyid%vel(dir)) = F%f(hyid%vel(dir)) + this%p(U)

    F%f(hyid%e_tot) = U%u(hyid%rho_vel(dir)) / U%u(hyid%rho) * ( U%u(hyid%e_tot) + this%p(U) )
  end subroutine rhyme_ideal_gas_flux_at


  pure subroutine rhyme_ideal_gas_half_step_extrapolation (this, U, Delta, dir, dx, dt, L, R)
    implicit none

    class ( ideal_gas_t ), intent(in) :: this
    type ( hydro_conserved_t ), intent(in) :: U, Delta
    integer, intent(in) :: dir
    real(kind=8), intent(in) :: dx, dt
    type ( hydro_conserved_t ), intent(out) :: L, R

    type ( hydro_flux_t ) :: FL, FR, dF

    L%u = U%u - .5d0 * Delta%u
    R%u = U%u + .5d0 * Delta%u

    call this%flux_at ( L, dir, FL )
    call this%flux_at ( R, dir, FR )

    dF%f = FL%f - FR%f

    L%u = L%u + .5d0 * dt / dx * dF%f
    R%u = R%u + .5d0 * dt / dx * dF%f

    ! Avoiding absolute vacuum
    L%u(hyid%rho) = max ( L%u(hyid%rho), epsilon(0.d0) )
    R%u(hyid%rho) = max ( R%u(hyid%rho), epsilon(0.d0) )
  end subroutine rhyme_ideal_gas_half_step_extrapolation
end module rhyme_ideal_gas
