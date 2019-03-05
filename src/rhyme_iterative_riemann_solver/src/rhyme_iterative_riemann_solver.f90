module rhyme_iterative_riemann_solver
  use rhyme_hydro_base
  use rhyme_ideal_gas
  use rhyme_riemann_problem

  implicit none

  type iterative_riemann_solver_t
    logical :: initialized = .false.
    integer :: n_iteration = 100
    real ( kind=8 ) :: pressure_floor = 1.d-10
    real ( kind=8 ) :: tolerance = 1.d-6
  contains
    procedure :: init => rhyme_iterative_riemann_solver_init
    procedure :: init_with => rhyme_iterative_riemann_solver_init_with
    procedure :: solve => rhyme_iterative_riemann_solver_solve
    procedure :: sampling => rhyme_iterative_riemann_solver_sampling
  end type iterative_riemann_solver_t

  interface nonlinear_waves
    procedure rhyme_iterative_riemann_solver_nonlinear_waves
  end interface nonlinear_waves

contains

  pure subroutine rhyme_iterative_riemann_solver_init_with ( this, niter, tol, pfloor )
    implicit none

    class ( iterative_riemann_solver_t ), intent ( inout ) :: this
    integer, intent ( in ) :: niter
    real ( kind=8 ), intent ( in ) :: tol, pfloor

    if ( this%initialized ) return

    this%n_iteration = niter
    this%tolerance = tol
    this%pressure_floor = pfloor

    call this%init
  end subroutine rhyme_iterative_riemann_solver_init_with


  pure subroutine rhyme_iterative_riemann_solver_init ( this )
    implicit none

    class ( iterative_riemann_solver_t ), intent ( inout ) :: this

    if ( this%initialized ) return

    this%initialized = .true.
  end subroutine rhyme_iterative_riemann_solver_init

  !> calculate star region variables based on Newton-Rhapson iteration method
  !! @param[in] L, R <= Left and right conserved hydro states
  !! @param[in] dir <= wave direection
  !! @param[out] star => star region variables (to be filled)
  pure subroutine rhyme_iterative_riemann_solver_solve ( this, ig, L, R, dir, star )
    implicit none

    class ( iterative_riemann_solver_t ), intent ( inout ) :: this
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( in ) :: L, R
    integer, intent(in) :: dir
    type ( rp_star_region_t ), intent ( out ) :: star

    real ( kind=8 ) :: pL, pR, vL, vR, csL, csR, fL, fR, fprimeL, fprimeR
    real ( kind=8 ) :: p_star__pL, p_star__pR
    real ( kind=8 ) :: p_star, p_star_prev

    integer :: i

    if ( .not. this%initialized ) return

    pL = max ( ig%p(L), this%pressure_floor )
    pR = max ( ig%p(R), this%pressure_floor )

    csL = ig%Cs(L)
    csR = ig%Cs(R)

    p_star = ( R%u(hyid%rho) * csR * pL + L%u(hyid%rho) * csL * pR &
      + csR * csL * (L%u(hyid%vel(dir)) - R%u(hyid%vel(dir))) ) &
      / (R%u(hyid%rho) * csR + L%u(hyid%rho) * csL)

    p_star = max( p_star, this%pressure_floor )

    vL = L%u(hyid%rho_vel(dir)) / L%u(hyid%rho)
    vR = R%u(hyid%rho_vel(dir)) / R%u(hyid%rho)

    p_star_prev = this%pressure_floor

    do i = 1, this%n_iteration
      call nonlinear_waves( ig, L%u(hyid%rho), pL, p_star, fL, fprimeL )
      call nonlinear_waves( ig, R%u(hyid%rho), pR, p_star, fR, fprimeR )

      p_star = p_star - (fL + fR + (vR - vL)) / (fprimeL + fprimeR)

      if ( abs(p_star - p_star_prev) / (0.5d0 * (p_star + p_star_prev)) < this%tolerance) exit
      p_star_prev = p_star
    end do

    p_star = max (p_star, this%pressure_floor)

    star%p = p_star
    star%u = 0.5d0 * ((vR + vL) + (fR - fL))

    p_star__pL = star%p / pL
    p_star__pR = star%p / pR

    if ( p_star > pL ) then
      star%left%is_shock = .true.
      star%left%shock%rho = L%u(hyid%rho) * (ig%gm1_gp1 + p_star__pL) / (ig%gm1_gp1 * p_star__pL + 1.0)
      star%left%shock%speed = vL - csL * sqrt(ig%gp1_2g * p_star__pL + ig%gm1_2g)
    else
      star%left%is_shock = .false.
      star%left%fan%rho = L%u(hyid%rho) * p_star__pL**real(ig%g_inv, kind=8)
      star%left%fan%cs = csL * p_star__pL**real(ig%gm1_2g, kind=8)
      star%left%fan%speedH = vL - csL
      star%left%fan%speedT = star%u - star%left%fan%cs
    end if

    if ( p_star > pR ) then
      star%right%is_shock = .true.
      star%right%shock%rho = R%u(hyid%rho) * (ig%gm1_gp1 + p_star__pR) / (ig%gm1_gp1 * p_star__pR + 1.0)
      star%right%shock%speed = vR + csR * sqrt(ig%gp1_2g * p_star__pR + ig%gm1_2g)
    else
      star%right%is_shock = .false.
      star%right%fan%rho = R%u(hyid%rho) * p_star__pR**real(ig%g_inv, kind=8)
      star%right%fan%cs = csR * p_star__pR**real(ig%gm1_2g, kind=8)
      star%right%fan%speedH = vR + csR
      star%right%fan%speedT = star%u + star%right%fan%cs
    end if
  end subroutine rhyme_iterative_riemann_solver_solve


  pure subroutine rhyme_iterative_riemann_solver_nonlinear_waves ( ig, rho, p_k, p, f, fprime)
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    real ( kind=8 ), intent ( in ) :: rho, p_k, p
    real ( kind=8 ), intent ( out ) :: f, fprime

    real(kind=8) :: factor, cs
    real(kind=8) :: Ak, Bk

    Ak = 2.d0 / ((ig%gamma + 1.d0) * rho)
    Bk = (ig%gamma - 1.d0) * p_k / (ig%gamma + 1.d0)

    if (p > p_k) then
      factor = sqrt(AK / (Bk + p))
      f = (p - p_k) * factor
      fprime = factor * (1.d0 - (p - p_k) / (2.d0 * (Bk + p)))
    else
      cs = sqrt(ig%gamma * p_k / rho)

      f = 2.d0 * cs / (ig%gamma - 1.d0) * ((p / p_k)**real((ig%gamma - 1.d0) / (2.d0 * ig%gamma), kind=8) - 1.d0)
      fprime =  1.d0 / (rho * cs) * ((p / p_k)**real(-(ig%gamma + 1.d0) / (2.d0 * ig%gamma), kind=8))
    end if
  end subroutine rhyme_iterative_riemann_solver_nonlinear_waves


  pure subroutine rhyme_iterative_riemann_solver_sampling ( this, ig, L, R, star, dir, dx, dt, U )
    implicit none

    class ( iterative_riemann_solver_t ), intent ( in ) :: this
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( in ) :: L, R
    type ( rp_star_region_t ), intent ( in ) :: star
    integer, intent ( in ) :: dir
    real ( kind=8 ), intent ( in ) :: dx, dt
    type ( hydro_conserved_t ), intent ( out ) :: U

    if ( .not. this%initialized ) return

    if ( dx/dt <= star%u ) then
      call irs_sampling_left( L, star, dx/dt, U )
    else
      call irs_sampling_right( R, star, dx/dt, U )
    end if

  contains

    pure subroutine irs_sampling_right ( right, star_region, dxdt, state )
      implicit none

      type ( hydro_conserved_t ), intent ( in ) :: right
      type ( rp_star_region_t ), intent ( in ) :: star_region
      real ( kind=8 ), intent ( in ) :: dxdt
      type ( hydro_conserved_t ), intent ( out ) :: state
      real(kind=8) :: factor, vel(3)

      vel = right%u(hyid%rho_u:hyid%rho_w) / right%u(hyid%rho)

      if ( star_region%right%is_shock ) then ! Shock

        if ( star_region%u <= dxdt &
          .and. dxdt <= star_region%right%shock%speed ) then
          vel(dir) = star_region%u
          call ig%prim_vars_to_cons( &
            star_region%right%shock%rho, &
            vel(1), vel(2), vel(3), &
            star_region%p, state &
          )
        else if ( dxdt >= star_region%right%shock%speed ) then
          call hy_copy (right, state)
        end if

      else ! Fan
        if ( star_region%u <= dxdt &
          .and. dxdt <= star_region%right%fan%speedT ) then
          vel(dir) = star_region%u
          call ig%prim_vars_to_cons ( &
            star_region%right%fan%rho, &
            vel(1), vel(2), vel(3), &
            star_region%p, state &
          )
        else if ( star_region%right%fan%speedT <= dxdt &
          .and. dxdt <= star_region%right%fan%speedH ) then
          factor = 2.0 / ig%gp1 - ig%gm1_gp1 / ig%Cs(right) &
            * ( right%u(hyid%vel(dir)) / right%u(hyid%rho) - dxdt )
          vel(dir) = 2.0 / ig%gp1 &
            * ( -ig%Cs(right) + ig%gm1 / 2.0 * state%u(hyid%vel(dir)) / right%u(hyid%rho) + dxdt )
          call ig%prim_vars_to_cons ( &
            right%u(hyid%rho) * factor**(2.0 / ig%gm1), &
            vel(1), vel(2), vel(3), &
            ig%p(right) * factor**(2.0 * ig%gamma / ig%gm1), state &
          )
        else if ( dxdt >= star_region%right%fan%speedH ) then
          call hy_copy(right, state)
        end if
      end if
    end subroutine irs_sampling_right


    pure subroutine irs_sampling_left ( left, star_region, dxdt, state )
      implicit none

      type ( hydro_conserved_t ), intent ( in ) :: left
      type ( rp_star_region_t ), intent ( in ) :: star_region
      real ( kind=8 ), intent ( in ) :: dxdt
      type ( hydro_conserved_t ), intent ( out ) :: state
      real(kind=8) :: factor, vel(3)

      vel = left%u(hyid%rho_u:hyid%rho_w) / left%u(hyid%rho)

      if ( star_region%left%is_shock ) then ! Shock
        if ( star_region%left%shock%speed <= dxdt &
          .and. dxdt < star_region%u) then
          vel(dir) = star_region%u
          call ig%prim_vars_to_cons( &
            star_region%left%shock%rho, &
            vel(1), vel(2), vel(3), &
            star_region%p, state &
          )
        else if ( dxdt < star_region%left%shock%speed ) then
          call hy_copy(left, state)
        end if
      else ! Fan
        if ( dxdt <= star_region%left%fan%speedH ) then
          call hy_copy(left, state)
        else if ( star_region%left%fan%speedH <= dxdt &
          .and. dxdt <= star_region%left%fan%speedT ) then
          factor = 2.0 / ig%gp1 + ig%gm1_gp1 / ig%Cs(left) &
            * (left%u(hyid%vel(dir)) / left%u(hyid%rho) - dxdt)
          vel(dir) = 2.0 / ig%gp1 &
            * ( ig%Cs(left) + ig%gm1 / 2.0 * left%u(hyid%vel(dir)) / left%u(hyid%rho) + dxdt)
          call ig%prim_vars_to_cons( &
            left%u(hyid%rho) * factor**(2.0 / ig%gm1), &
            vel(1), vel(2), vel(3), &
            ig%p(left) * factor**(2.0 * ig%gamma / ig%gm1), state &
          )
        else if ( star_region%left%fan%speedT <= dxdt &
          .and. dxdt <= star_region%u ) then
          vel(dir) = star_region%u
          call ig%prim_vars_to_cons( &
            star_region%left%fan%rho, &
            vel(1), vel(2), vel(3), &
            star_region%p, state &
          )
        end if
      end if
    end subroutine irs_sampling_left
  end subroutine rhyme_iterative_riemann_solver_sampling
end module rhyme_iterative_riemann_solver
