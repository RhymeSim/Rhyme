module rhyme_irs
  use rhyme_riemann_problem
  use rhyme_physics
  use rhyme_hydro_base
  use rhyme_thermo_base
  use rhyme_logger

  implicit none

  real ( kind=8 ), private :: gm1 = 0.d0
  real ( kind=8 ), private :: gp1 = 0.d0
  real ( kind=8 ), private :: gm1_gp1 = 0.d0
  real ( kind=8 ), private :: gm1_2g = 0.d0
  real ( kind=8 ), private :: gp1_2g = 0.d0
  real ( kind=8 ), private :: g_inv = 0.d0

  type irs_t
    integer :: n_iteration = 100
    real ( kind=8 ) :: pressure_floor = 1.d-10
    real ( kind=8 ) :: tolerance = 1.d-6
  end type irs_t

  interface
    pure module subroutine rhyme_irs_solve ( irs, l, r, dx, dt, axis, u )
      type ( irs_t ), intent ( in ) :: irs
      real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( in ) :: l, r
      real ( kind=8 ), intent ( in ) :: dx, dt
      integer, intent ( in ) :: axis
      real ( kind=8 ), intent ( inout ) :: u( cid%rho:cid%e_tot )
    end subroutine rhyme_irs_solve

    pure module subroutine rhyme_irs_sampling ( solution, axis, dx, dt, u )
      type ( riemann_problem_solution_t ), intent ( in ) :: solution
      integer, intent ( in ) :: axis
      real ( kind=8 ), intent ( in ) :: dx, dt
      real ( kind=8 ), intent ( out ) :: u( cid%rho:cid%e_tot )
    end subroutine rhyme_irs_sampling

    pure module subroutine rhyme_irs_iterate ( irs, solution, axis )
      type ( irs_t ), intent ( in ) :: irs
      type ( riemann_problem_solution_t ), intent ( inout ) :: solution
      integer, intent ( in ) :: axis
    end subroutine rhyme_irs_iterate

    pure module subroutine rhyme_irs_nonlinear_wave_function ( state, p, star )
      type ( rp_side_t ), intent ( in ) :: state
      real ( kind=8 ), intent ( in ) :: p
      type ( rp_star_side_t ), intent ( inout ) :: star
    end subroutine rhyme_irs_nonlinear_wave_function

    pure module function rhyme_irs_guess_p_star ( l, r, axis, tol ) result ( p_star )
      type ( rp_side_t ), intent ( in ) :: l, r
      integer, intent ( in ) :: axis
      real ( kind=8 ), intent ( in ) :: tol
      real ( kind=8 ) :: p_star(5)
    end function rhyme_irs_guess_p_star

    pure module function irs_w_k ( s ) result ( u )
      type ( rp_side_t ), intent ( in ) :: s
      real ( kind=8 ) :: u( cid%rho:cid%e_tot )
    end function irs_w_k

    pure module function irs_w_starl_sho ( s, axis ) result ( u )
      type ( riemann_problem_solution_t ), intent ( in ) :: s
      integer, intent ( in ) :: axis
      real ( kind=8 ) :: u( cid%rho:cid%e_tot )
    end function irs_w_starl_sho

    pure module function irs_w_starr_sho ( s, axis ) result ( u )
      type ( riemann_problem_solution_t ), intent ( in ) :: s
      integer, intent ( in ) :: axis
      real ( kind=8 ) :: u( cid%rho:cid%e_tot )
    end function irs_w_starr_sho

    pure module function irs_w_kfan ( s, dxdt, axis, is_right ) result ( u )
      type ( rp_side_t ), intent ( in ) :: s
      real ( kind=8 ), intent ( in ) :: dxdt
      integer, intent ( in ) :: axis
      logical, intent ( in ) :: is_right
      real ( kind=8 ) :: u( cid%rho:cid%e_tot )
    end function irs_w_kfan

    pure module function irs_w_starl_fan ( s, axis ) result ( u )
      type ( riemann_problem_solution_t ), intent ( in ) :: s
      integer, intent ( in ) :: axis
      real ( kind=8 ) :: u( cid%rho:cid%e_tot )
    end function irs_w_starl_fan

    pure module function irs_w_starr_fan ( s, axis ) result ( u )
      type ( riemann_problem_solution_t ), intent ( in ) :: s
      integer, intent ( in ) :: axis
      real ( kind=8 ) :: u( cid%rho:cid%e_tot )
    end function irs_w_starr_fan

    pure module function irs_rp_side_to_cons ( s ) result ( u )
      type ( rp_side_t ), intent ( in ) :: s
      real ( kind=8 ) :: u( cid%rho:cid%e_tot )
    end function irs_rp_side_to_cons
  end interface

contains

  module subroutine rhyme_irs_init ( irs, logger )
    implicit none

    type ( irs_t ), intent ( inout ) :: irs
    type ( logger_t ), intent ( inout ) :: logger

    real ( kind=8 ) :: g

    call logger%begin_section( 'irs' )

    g = get_gamma()

    call logger%log( '', 'n_iteration', '=', [ irs%n_iteration ] )
    call logger%log( '', 'pressure_floor', '=', [ irs%pressure_floor ] )
    call logger%log( '', 'tolerance', '=', [ irs%tolerance ] )

    call logger%log( 'Setting up ideal gas coefficients...' )

    gm1 = g - 1.d0
    gp1 = g + 1.d0
    gm1_gp1 = gm1 / gp1
    gm1_2g = gm1 / ( 2.d0 * g )
    gp1_2g = gp1 / ( 2.d0 * g )
    g_inv = 1.d0 / g

    call logger%end_section
  end subroutine rhyme_irs_init
end module rhyme_irs
