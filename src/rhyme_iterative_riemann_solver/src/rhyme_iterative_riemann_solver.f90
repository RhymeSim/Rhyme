module rhyme_iterative_riemann_solver
  use rhyme_hydro_base
  use rhyme_ideal_gas
  use rhyme_riemann_problem
  use rhyme_log

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

contains

  subroutine rhyme_iterative_riemann_solver_init_with ( &
    this, niter, tol, pfloor, log )
    implicit none

    class ( iterative_riemann_solver_t ), intent ( inout ) :: this
    integer, intent ( in ) :: niter
    real ( kind=8 ), intent ( in ) :: tol, pfloor
    type ( log_t ), intent ( inout ) :: log

    if ( this%initialized ) then
      call log%warn( 'Try to re-initialize irs object')
      return
    end if

    this%n_iteration = niter
    this%tolerance = tol
    this%pressure_floor = pfloor

    call this%init( log )
  end subroutine rhyme_iterative_riemann_solver_init_with


  subroutine rhyme_iterative_riemann_solver_init ( this, log )
    implicit none

    class ( iterative_riemann_solver_t ), intent ( inout ) :: this
    type ( log_t ), intent ( inout ) :: log

    if ( this%initialized ) then
      call log%warn( 'Try to re-initialize irs object')
      return
    end if

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
    integer, intent ( in ) :: dir
    type ( rp_star_region_t ), intent ( out ) :: star

    real ( kind=8 ) :: pL, pR, vL, vR, csL, csR, fL, fR, fprimeL, fprimeR
    real ( kind=8 ) :: ps_pL, ps_pR
    real ( kind=8 ) :: p_star, p_star_prev

    integer :: i

    pL = ig%p(L)
    pR = ig%p(R)

    csL = ig%Cs(L)
    csR = ig%Cs(R)

    vL = L%u( hyid%rho_vel(dir) ) / L%u(hyid%rho)
    vR = R%u( hyid%rho_vel(dir) ) / R%u(hyid%rho)

    p_star = max( &
      rhyme_iterative_riemann_solver_guess_p_star( &
        R%u(hyid%rho), csR, vR, pR, L%u(hyid%rho), csL, vL, pL &
      ), this%tolerance )

    p_star_prev = p_star

    do i = 1, this%n_iteration
      call rhyme_iterative_riemann_solver_nonlinear_waves( ig, L%u(hyid%rho), pL, p_star, fL, fprimeL )
      call rhyme_iterative_riemann_solver_nonlinear_waves( ig, R%u(hyid%rho), pR, p_star, fR, fprimeR )

      p_star = p_star - (fL + fR + (vR - vL)) / (fprimeL + fprimeR)

      if ( abs(p_star - p_star_prev) / (0.5d0 * (p_star + p_star_prev)) < this%tolerance) exit
      p_star_prev = p_star
    end do

    star%p = p_star
    star%u = 0.5d0 * ( (vR + vL) + (fR - fL) )

    ps_pL = star%p / pL
    ps_pR = star%p / pR


    if ( p_star > pL ) then
      star%left%is_shock = .true.
      star%left%shock%rho = L%u(hyid%rho) * (ig%gm1_gp1 + ps_pL) / (ig%gm1_gp1 * ps_pL + 1.0)
      star%left%shock%speed = vL - csL * sqrt(ig%gp1_2g * ps_pL + ig%gm1_2g)
    else
      star%left%is_shock = .false.
      star%left%fan%rho = L%u(hyid%rho) * ps_pL**real(ig%g_inv, kind=8)
      star%left%fan%cs = csL * ps_pL**real(ig%gm1_2g, kind=8)
      star%left%fan%speedH = vL - csL
      star%left%fan%speedT = star%u - star%left%fan%cs
    end if

    if ( p_star > pR ) then
      star%right%is_shock = .true.
      star%right%shock%rho = R%u(hyid%rho) * (ig%gm1_gp1 + ps_pR) / (ig%gm1_gp1 * ps_pR + 1.0)
      star%right%shock%speed = vR + csR * sqrt(ig%gp1_2g * ps_pR + ig%gm1_2g)
    else
      star%right%is_shock = .false.
      star%right%fan%rho = R%u(hyid%rho) * ps_pR**real(ig%g_inv, kind=8)
      star%right%fan%cs = csR * ps_pR**real(ig%gm1_2g, kind=8)
      star%right%fan%speedH = vR + csR
      star%right%fan%speedT = star%u + star%right%fan%cs
    end if
  end subroutine rhyme_iterative_riemann_solver_solve


  pure function rhyme_iterative_riemann_solver_guess_p_star ( &
    rhor, csr, ur, pr, rhol, csl, ul, pl ) result ( p_star )
    implicit none

    real ( kind=8 ), intent ( in ) :: rhor, csr, ur, pr, rhol, csl, ul, pl
    real ( kind=8 ) :: p_star

    p_star = ( &
      rhor * csr * pl + rhol * csl * pr + csr * csl * ( rhol * ul - rhor * ur ) &
    ) / ( rhor * csr + rhol * csl )
  end function rhyme_iterative_riemann_solver_guess_p_star


  pure subroutine rhyme_iterative_riemann_solver_nonlinear_waves ( ig, rho, p_k, p, f, fprime)
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    real ( kind=8 ), intent ( in ) :: rho, p_k, p
    real ( kind=8 ), intent ( out ) :: f, fprime

    real(kind=8) :: factor, cs
    real(kind=8) :: Ak, Bk

    Ak = 2.d0 / (ig%gp1 * rho)
    Bk = ig%gm1 * p_k / ig%gp1

    if (p > p_k) then
      factor = sqrt(AK / (Bk + p))
      f = (p - p_k) * factor
      fprime = factor * (1.d0 - (p - p_k) / (2.d0 * (Bk + p)))
    else
      cs = sqrt(ig%gamma * p_k / rho)

      f = 2.d0 * cs / ig%gm1 * ((p / p_k)**ig%gm1_2g - 1.d0)
      fprime =  1.d0 / (rho * cs) * (p / p_k)**(-ig%gp1_2g)
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

    real ( kind=8 ) :: dxdt

    dxdt = dx / dt

    if ( dxdt <= star%u ) then
      call irs_sampling_left( U )
    else
      call irs_sampling_right( U )
    end if

  contains

    pure subroutine irs_sampling_right ( state )
      implicit none

      type ( hydro_conserved_t ), intent ( out ) :: state

      real(kind=8) :: factor, vel(3)

      vel = R%u(hyid%rho_u:hyid%rho_w) / R%u(hyid%rho)

      if ( star%right%is_shock ) then ! Shock

        if ( star%u <= dxdt &
          .and. dxdt <= star%right%shock%speed ) then
          vel(dir) = star%u
          call ig%prim_vars_to_cons( &
            star%right%shock%rho, &
            vel(1), vel(2), vel(3), &
            star%p, state &
          )
        else if ( dxdt >= star%right%shock%speed ) then
          call hy_copy( R, state )
        end if

      else ! Fan
        if ( star%u <= dxdt &
          .and. dxdt <= star%right%fan%speedT ) then
          vel(dir) = star%u
          call ig%prim_vars_to_cons ( &
            star%right%fan%rho, &
            vel(1), vel(2), vel(3), &
            star%p, state &
          )
        else if ( star%right%fan%speedT <= dxdt &
          .and. dxdt <= star%right%fan%speedH ) then
          factor = 2.0 / ig%gp1 - ig%gm1_gp1 / ig%Cs(R) &
            * ( R%u(hyid%vel(dir)) / R%u(hyid%rho) - dxdt )
          vel(dir) = 2.0 / ig%gp1 &
            * ( -ig%Cs(R) + ig%gm1 / 2.0 * state%u(hyid%vel(dir)) / R%u(hyid%rho) + dxdt )
          call ig%prim_vars_to_cons ( &
            R%u(hyid%rho) * factor**(2.0 / ig%gm1), &
            vel(1), vel(2), vel(3), &
            ig%p(R) * factor**(1.d0 / ig%gm1_2g), state &
          )
        else if ( dxdt >= star%right%fan%speedH ) then
          call hy_copy( R, state )
        end if
      end if
    end subroutine irs_sampling_right


    pure subroutine irs_sampling_left ( state )
      implicit none

      type ( hydro_conserved_t ), intent ( out ) :: state
      real(kind=8) :: factor, vel(3)

      vel = L%u(hyid%rho_u:hyid%rho_w) / L%u(hyid%rho)

      if ( star%left%is_shock ) then ! Shock
        if ( star%left%shock%speed <= dxdt &
          .and. dxdt < star%u) then
          vel(dir) = star%u
          call ig%prim_vars_to_cons( &
            star%left%shock%rho, &
            vel(1), vel(2), vel(3), &
            star%p, state &
          )
        else if ( dxdt < star%left%shock%speed ) then
          call hy_copy( L, state )
        end if
      else ! Fan
        if ( dxdt <= star%left%fan%speedH ) then
          call hy_copy( L, state )
        else if ( star%left%fan%speedH <= dxdt &
          .and. dxdt <= star%left%fan%speedT ) then
          factor = 2.0 / ig%gp1 + ig%gm1_gp1 / ig%Cs(L) &
            * (L%u(hyid%vel(dir)) / L%u(hyid%rho) - dxdt)
          vel(dir) = 2.0 / ig%gp1 &
            * ( ig%Cs(L) + ig%gm1 / 2.0 * L%u(hyid%vel(dir)) / L%u(hyid%rho) + dxdt)
          call ig%prim_vars_to_cons( &
            L%u(hyid%rho) * factor**(2.0 / ig%gm1), &
            vel(1), vel(2), vel(3), &
            ig%p(L) * factor**(1.d0 / ig%gm1_2g), state &
          )
        else if ( star%left%fan%speedT <= dxdt &
          .and. dxdt <= star%u ) then
          vel(dir) = star%u
          call ig%prim_vars_to_cons( &
            star%left%fan%rho, &
            vel(1), vel(2), vel(3), &
            star%p, state &
          )
        end if
      end if
    end subroutine irs_sampling_left
  end subroutine rhyme_iterative_riemann_solver_sampling
end module rhyme_iterative_riemann_solver
