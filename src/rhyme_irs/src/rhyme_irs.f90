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
  end type irs_t

  interface
    pure module subroutine rhyme_irs_solve ( cfg, ig, L, R, dx, dt, dir, U )
      type ( irs_t ), intent ( in ) :: cfg
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( hydro_conserved_t ), intent ( in ) :: L, R
      real ( kind=8 ), intent ( in ) :: dx, dt
      integer, intent ( in ) :: dir
      type ( hydro_conserved_t ), intent ( inout ) :: U
    end subroutine rhyme_irs_solve

    pure module subroutine rhyme_irs_sampling ( ig, solution, dir, dx, dt, U )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( riemann_problem_solution_t ), intent ( in ) :: solution
      integer, intent ( in ) :: dir
      real ( kind=8 ), intent ( in ) :: dx, dt
      type ( hydro_conserved_t ), intent ( out ) :: U
    end subroutine rhyme_irs_sampling

    pure module subroutine rhyme_irs_iterate ( cfg, ig, solution, dir )
      class ( irs_t ), intent ( in ) :: cfg
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( riemann_problem_solution_t ), intent ( inout ) :: solution
      integer, intent ( in ) :: dir
    end subroutine rhyme_irs_iterate

    pure module subroutine rhyme_irs_nonlinear_wave_function ( ig, state, p, star )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( rp_side_t ), intent ( in ) :: state
      real ( kind=8 ), intent ( in ) :: p
      type ( rp_star_side_t ), intent ( inout ) :: star
    end subroutine rhyme_irs_nonlinear_wave_function

    pure module function rhyme_irs_guess_p_star ( ig, L, R, dir, tol ) result ( p_star )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( rp_side_t ), intent ( in ) :: L, R
      integer, intent ( in ) :: dir
      real ( kind=8 ), intent ( in ) :: tol
      real ( kind=8 ) :: p_star(5)
    end function rhyme_irs_guess_p_star

    type ( hydro_conserved_t ) pure module function irs_w_k ( ig, s ) result ( U )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( rp_side_t ), intent ( in ) :: s
    end function irs_w_k

    type ( hydro_conserved_t ) pure module function irs_w_starL_sho ( ig, s, dir ) result ( U )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( riemann_problem_solution_t ), intent ( in ) :: s
      integer, intent ( in ) :: dir
    end function irs_w_starL_sho

    type ( hydro_conserved_t ) pure module function irs_w_starR_sho ( ig, s, dir ) result ( U )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( riemann_problem_solution_t ), intent ( in ) :: s
      integer, intent ( in ) :: dir
    end function irs_w_starR_sho

    type ( hydro_conserved_t ) pure module function irs_w_kfan ( ig, s, dxdt, dir, is_right ) result ( U )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( rp_side_t ), intent ( in ) :: s
      real ( kind=8 ), intent ( in ) :: dxdt
      integer, intent ( in ) :: dir
      logical, intent ( in ) :: is_right
    end function irs_w_kfan

    type ( hydro_conserved_t ) pure module function irs_w_starL_fan ( ig, s, dir ) result ( U )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( riemann_problem_solution_t ), intent ( in ) :: s
      integer, intent ( in ) :: dir
    end function irs_w_starL_fan

    type ( hydro_conserved_t ) pure module function irs_w_starR_fan ( ig, s, dir ) result ( U )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( riemann_problem_solution_t ), intent ( in ) :: s
      integer, intent ( in ) :: dir
    end function irs_w_starR_fan

    type ( hydro_conserved_t ) pure module function irs_rp_side_to_cons ( ig, s ) result ( U )
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( rp_side_t ), intent ( in ) :: s
    end function irs_rp_side_to_cons
  end interface

contains

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
