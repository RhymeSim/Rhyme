module rhyme_iterative_riemann_solver
  use rhyme_hydro_base
  use rhyme_ideal_gas
  use rhyme_riemann_problem

  implicit none

  type iterative_riemann_solver_t
    logical :: initialized = .false.
    integer :: n_iteration = 100
    real(kind=8) :: pressure_floor = 1.d-10
    real(kind=8) :: tolerance = 1.d-6
    type ( ideal_gas_t ) :: ig
  contains
    procedure :: init_with => rhyme_iterative_riemann_solver_init_with
    procedure :: solve => rhyme_iterative_riemann_solver_solve
    procedure :: nonlinear_waves => rhyme_iterative_riemann_solver_nonlinear_waves
    procedure :: sampling => rhyme_iterative_riemann_solver_sampling
  end type iterative_riemann_solver_t

contains

  subroutine rhyme_iterative_riemann_solver_init_with ( this, ig, niter, tol, pfloor )
    implicit none

    class ( iterative_riemann_solver_t ), intent ( inout ) :: this
    type ( ideal_gas_t ), intent ( in ) :: ig
    integer, intent ( in ) :: niter
    real ( kind=8 ) :: tol, pfloor

    if ( this%initialized ) return

    this%ig = ig
    this%n_iteration = niter
    this%tolerance = tol
    this%pressure_floor = pfloor

    this%initialized = .true.
  end subroutine rhyme_iterative_riemann_solver_init_with

  !> calculate star region variables based on Newton-Rhapson iteration method
  !! @param[in] L, R <= Left and right conserved hydro states
  !! @param[in] dir <= wave direection
  !! @param[out] star => star region variables (to be filled)
  pure subroutine rhyme_iterative_riemann_solver_solve ( this, L, R, dir, star )
    implicit none

    class ( iterative_riemann_solver_t ), intent ( inout ) :: this
    type(hydro_conserved_t), intent(in) :: L, R
    integer, intent(in) :: dir
    type(rp_star_region_t), intent(out) :: star

    real(kind=8) :: pL, pR, vL, vR, csL, csR, fL, fR, fprimeL, fprimeR
    real(kind=8) :: p_star__pL, p_star__pR
    real(kind=8) :: g_m_1__g_p_1, g_p_1__2_g, g_m_1__2_g, gamma_inv
    real(kind=8) :: p_star, p_star_prev

    integer :: i

    pL = max ( this%ig%p(L), this%pressure_floor )
    pR = max ( this%ig%p(R), this%pressure_floor )

    csL = this%ig%Cs(L)
    csR = this%ig%Cs(R)

    p_star = ( R%u(hyid%rho) * csR * pL + L%u(hyid%rho) * csL * pR &
      + csR * csL * (L%u(hyid%vel(dir)) - R%u(hyid%vel(dir))) ) &
      / (R%u(hyid%rho) * csR + L%u(hyid%rho) * csL)

    p_star = max( p_star, this%pressure_floor )

    vL = L%u(hyid%rho_vel(dir)) / L%u(hyid%rho)
    vR = R%u(hyid%rho_vel(dir)) / R%u(hyid%rho)

    p_star_prev = this%pressure_floor

    do i = 1, this%n_iteration
      call this%nonlinear_waves ( L%u(hyid%rho), pL, p_star, fL, fprimeL )
      call this%nonlinear_waves ( R%u(hyid%rho), pR, p_star, fR, fprimeR )

      p_star = p_star - (fL + fR + (vR - vL)) / (fprimeL + fprimeR)

      if ( abs(p_star - p_star_prev) / (0.5d0 * (p_star + p_star_prev)) < this%tolerance) exit
      p_star_prev = p_star
    end do

    p_star = max (p_star, this%pressure_floor)

    star%p = p_star
    star%u = 0.5d0 * ((vR + vL) + (fR - fL))

    p_star__pL = star%p / pL
    p_star__pR = star%p / pR

    g_m_1__g_p_1 = (this%ig%gamma - 1.d0) / (this%ig%gamma + 1.d0)
    g_p_1__2_g = (this%ig%gamma + 1.d0) / (2.d0 * this%ig%gamma)
    g_m_1__2_g = (this%ig%gamma - 1.d0) / (2.d0 * this%ig%gamma)
    gamma_inv = 1.d0 / this%ig%gamma

    if (p_star > pL) then
      star%left%is_shock = .true.
      star%left%shock%rho = L%u(hyid%rho) * (g_m_1__g_p_1 + p_star__pL) / (g_m_1__g_p_1 * p_star__pL + 1.0)
      star%left%shock%speed = vL - csL * sqrt(g_p_1__2_g * p_star__pL + g_m_1__2_g)
    else
      star%left%is_shock = .false.
      star%left%fan%rho = L%u(hyid%rho) * p_star__pL**real(gamma_inv, kind=8)
      star%left%fan%cs = csL * p_star__pL**real(g_m_1__2_g, kind=8)
      star%left%fan%speedH = vL - csL
      star%left%fan%speedT = star%u - star%left%fan%cs
    end if

    if (p_star > pR) then
      star%right%is_shock = .true.
      star%right%shock%rho = R%u(hyid%rho) * (g_m_1__g_p_1 + p_star__pR) / (g_m_1__g_p_1 * p_star__pR + 1.0)
      star%right%shock%speed = vR + csR * sqrt(g_p_1__2_g * p_star__pR + g_m_1__2_g)
    else
      star%right%is_shock = .false.
      star%right%fan%rho = R%u(hyid%rho) * p_star__pR**real(gamma_inv, kind=8)
      star%right%fan%cs = csR * p_star__pR**real(g_m_1__2_g, kind=8)
      star%right%fan%speedH = vR + csR
      star%right%fan%speedT = star%u + star%right%fan%cs
    end if
  end subroutine rhyme_iterative_riemann_solver_solve


  pure subroutine rhyme_iterative_riemann_solver_nonlinear_waves ( this, rho, p_k, p, f, fprime)
    implicit none

    class ( iterative_riemann_solver_t ), intent ( in ) :: this
    real(kind=8), intent(in) :: rho, p_k, p
    real(kind=8), intent(out) :: f, fprime

    real(kind=8) :: factor, cs
    real(kind=8) :: Ak, Bk

    Ak = 2.d0 / ((this%ig%gamma + 1.d0) * rho)
    Bk = (this%ig%gamma - 1.d0) * p_k / (this%ig%gamma + 1.d0)

    if (p > p_k) then
      factor = sqrt(AK / (Bk + p))
      f = (p - p_k) * factor
      fprime = factor * (1.d0 - (p - p_k) / (2.d0 * (Bk + p)))
    else
      cs = sqrt(this%ig%gamma * p_k / rho)

      f = 2.d0 * cs / (this%ig%gamma - 1.d0) * ((p / p_k)**real((this%ig%gamma - 1.d0) / (2.d0 * this%ig%gamma), kind=8) - 1.d0)
      fprime =  1.d0 / (rho * cs) * ((p / p_k)**real(-(this%ig%gamma + 1.d0) / (2.d0 * this%ig%gamma), kind=8))
    end if
  end subroutine rhyme_iterative_riemann_solver_nonlinear_waves


  subroutine rhyme_iterative_riemann_solver_sampling ( this, L, R, star, dir, dx, dt, U )
    implicit none

    class ( iterative_riemann_solver_t ), intent ( in ) :: this
    type(hydro_conserved_t), intent(in) :: L, R
    type(rp_star_region_t), intent(in) :: star
    integer, intent(in) :: dir
    real(kind=8), intent(in) :: dx, dt
    type(hydro_conserved_t), intent(out) :: U

    real(kind=8) :: g_m_1__g_p_1, g_p_1__2_g, g_m_1__2_g, g_m_1, g_p_1, gamma
    real(kind=8) :: S

    gamma = this%ig%gamma
    g_m_1 = (this%ig%gamma - 1.d0)
    g_p_1 = (this%ig%gamma + 1.d0)
    g_m_1__g_p_1 = (this%ig%gamma - 1.d0) / (this%ig%gamma + 1.d0)
    g_p_1__2_g = (this%ig%gamma + 1.d0) / (2.d0 * this%ig%gamma)
    g_m_1__2_g = (this%ig%gamma - 1.d0) / (2.d0 * this%ig%gamma)

    S = dx / dt

    if ( S <= star%u ) then
      call irs_sampling_left
    else
      call irs_sampling_right
    end if

  contains

    subroutine irs_sampling_right ()
      implicit none

      real(kind=8) :: factor, vel(3)

      vel = R%u(hyid%rho_u:hyid%rho_w) / R%u(hyid%rho)

      if ( star%right%is_shock ) then ! Shock
        if ( star%u <= S .and. S <= star%right%shock%speed ) then
          vel(dir) = star%u
          call this%ig%prim_vars_to_cons (star%right%shock%rho, vel(1), vel(2), vel(3), star%p, U)
        else if ( S >= star%right%shock%speed ) then
          call hy_copy (R, U)
        end if
      else ! Fan
        if ( star%u <= S .and. S <= star%right%fan%speedT ) then
          vel(dir) = star%u
          call this%ig%prim_vars_to_cons (star%right%fan%rho, vel(1), vel(2), vel(3), star%p, U)
        else if ( star%right%fan%speedT <= S .and. S <= star%right%fan%speedH ) then
          factor = 2.0 / g_p_1 - g_m_1__g_p_1 / this%ig%Cs(R) * (R%u(hyid%vel(dir)) / R%u(hyid%rho) - S)
          vel(dir) = 2.0 / g_p_1 * (-this%ig%Cs(R) + g_m_1 / 2.0 * U%u(hyid%vel(dir)) / R%u(hyid%rho) + S)
          call this%ig%prim_vars_to_cons (R%u(hyid%rho) * factor**(2.0 / g_m_1), &
          vel(1), vel(2), vel(3), this%ig%p(R) * factor**(2.0 * gamma / g_m_1), U)
        else if ( S >= star%right%fan%speedH ) then
          call hy_copy(R, U)
        end if
      end if
    end subroutine irs_sampling_right


    subroutine irs_sampling_left ()
      implicit none

      real(kind=8) :: factor, vel(3)

      vel = L%u(hyid%rho_u:hyid%rho_w) / L%u(hyid%rho)

      if ( star%left%is_shock ) then ! Shock
        if ( star%left%shock%speed <= S .and. S < star%u) then
          vel(dir) = star%u
          call this%ig%prim_vars_to_cons (star%left%shock%rho, vel(1), vel(2), vel(3), star%p, U)
        else if ( S < star%left%shock%speed ) then
          call hy_copy(L, U)
        end if
      else ! Fan
        if ( S <= star%left%fan%speedH ) then
          call hy_copy(L, U)
        else if ( star%left%fan%speedH <= S .and. S <= star%left%fan%speedT ) then
          factor = 2.0 / g_p_1 + g_m_1__g_p_1 / this%ig%Cs(L) * (L%u(hyid%vel(dir)) / L%u(hyid%rho) - S)
          vel(dir) = 2.0 / g_p_1 * ( this%ig%Cs(L) + g_m_1 / 2.0 * L%u(hyid%vel(dir)) / L%u(hyid%rho) + S)
          call this%ig%prim_vars_to_cons (L%u(hyid%rho) * factor**(2.0 / g_m_1), &
          vel(1), vel(2), vel(3), this%ig%p(L) * factor**(2.0 * gamma / g_m_1), U)
        else if ( star%left%fan%speedT <= S .and. S <= star%u ) then
          vel(dir) = star%u
          call this%ig%prim_vars_to_cons (star%left%fan%rho, vel(1), vel(2), vel(3), star%p, U)
        end if
      end if
    end subroutine irs_sampling_left
  end subroutine rhyme_iterative_riemann_solver_sampling

end module rhyme_iterative_riemann_solver
