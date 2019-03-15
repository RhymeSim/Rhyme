module rhyme_irs
  use rhyme_hydro_base
  use rhyme_ideal_gas
  use rhyme_riemann_problem
  use rhyme_log

  implicit none

  type irs_t
    logical :: initialized = .false.
    integer :: n_iteration = 100
    real ( kind=8 ) :: pressure_floor = 1.d-10
    real ( kind=8 ) :: tolerance = 1.d-6
  contains
    procedure :: init => rhyme_irs_init
    procedure :: init_with => rhyme_irs_init_with
  end type irs_t

  interface
    pure module subroutine rhyme_irs_sampling ( ig, L, R, solution, dir, dx, dt, U )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( hydro_conserved_t ), intent ( in ) :: L, R
      type ( riemann_problem_solution_t ), intent ( in ) :: solution
      integer, intent ( in ) :: dir
      real ( kind=8 ), intent ( in ) :: dx, dt
      type ( hydro_conserved_t ), intent ( out ) :: U
    end subroutine rhyme_irs_sampling

    pure module subroutine rhyme_irs_solve ( cfg, ig, L, R, dir, solution )
      class ( irs_t ), intent ( in ) :: cfg
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( hydro_conserved_t ), intent ( in ) :: L, R
      integer, intent ( in ) :: dir
      type ( riemann_problem_solution_t ), intent ( out ) :: solution
    end subroutine rhyme_irs_solve

    pure module subroutine rhyme_irs_nonlinear_waves ( ig, rho, p_k, p, f, fprime)
      type ( ideal_gas_t ), intent ( in ) :: ig
      real ( kind=8 ), intent ( in ) :: rho, p_k, p
      real ( kind=8 ), intent ( out ) :: f, fprime
    end subroutine rhyme_irs_nonlinear_waves

    pure module function rhyme_irs_guess_p_star ( &
      rhor, csr, ur, pr, rhol, csl, ul, pl ) result ( p_star )
      real ( kind=8 ), intent ( in ) :: rhor, csr, ur, pr, rhol, csl, ul, pl
      real ( kind=8 ) :: p_star
    end function rhyme_irs_guess_p_star
  end interface

contains

  subroutine rhyme_irs_init_with ( &
    this, niter, tol, pfloor, log )
    implicit none

    class ( irs_t ), intent ( inout ) :: this
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
  end subroutine rhyme_irs_init_with


  subroutine rhyme_irs_init ( this, log )
    implicit none

    class ( irs_t ), intent ( inout ) :: this
    type ( log_t ), intent ( inout ) :: log

    if ( this%initialized ) then
      call log%warn( 'Try to re-initialize irs object')
      return
    end if

    this%initialized = .true.
  end subroutine rhyme_irs_init

end module rhyme_irs
