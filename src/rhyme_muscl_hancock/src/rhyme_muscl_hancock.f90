module rhyme_muscl_hancock
  use rhyme_mh_workspace
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_slope_limiter
  use rhyme_irs
  use rhyme_log

  implicit none

  type rhyme_muscl_hancock_indices_t
    integer :: cpu_intensive = mhwsid%cpu_intensive
    integer :: memory_intensive = mhwsid%memory_intensive
  end type rhyme_muscl_hancock_indices_t

  type ( rhyme_muscl_hancock_indices_t ), parameter :: mhid = rhyme_muscl_hancock_indices_t ()


  type muscl_hancock_t
    logical :: active_axis(3) = .false.
    integer :: active_flux(3) = 0
    integer :: solver_type = mhid%memory_intensive
    logical :: initialized = .false.
  end type muscl_hancock_t

  interface
    module subroutine rhyme_muscl_hancock_init ( mh, samr, ws, logger )
      class ( muscl_hancock_t ), intent ( inout ) :: mh
      type ( samr_t ), intent ( in ) :: samr
      type ( mh_workspace_t ), intent ( inout ) :: ws
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_muscl_hancock_init

    pure module subroutine rhyme_muscl_hancock_solve_cpu_intensive ( &
      cfg, box, dx, dt, cfl, ig, irs, sl, ws )
      class ( muscl_hancock_t ), intent ( inout ) :: cfg
      type ( samr_box_t ), intent ( inout ) :: box
      real ( kind=8 ), intent ( in ) :: dx(3), dt
      type ( cfl_t ), intent ( in ) :: cfl
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( irs_t ), intent ( inout ) :: irs
      type ( slope_limiter_t ), intent ( in ) :: sl
      type ( mh_workspace_t ), intent ( inout ) :: ws
    end subroutine rhyme_muscl_hancock_solve_cpu_intensive

    pure module subroutine rhyme_muscl_hancock_solve_memory_intensive ( &
      cfg, box, dx, dt, cfl, ig, irs, sl, ws )
      class ( muscl_hancock_t ), intent ( inout ) :: cfg
      type ( samr_box_t ), intent ( inout ) :: box
      real ( kind=8 ), intent ( in ) :: dx(3), dt
      type ( cfl_t ), intent ( in ) :: cfl
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( irs_t ), intent ( inout ) :: irs
      type ( slope_limiter_t ), intent ( in ) :: sl
      type ( mh_workspace_t ), intent ( inout ) :: ws
    end subroutine rhyme_muscl_hancock_solve_memory_intensive

    module subroutine rhyme_muscl_hancock_solve ( &
      cfg, box, dx, dt, cfl, ig, irs, sl, ws, log )
      class ( muscl_hancock_t ), intent ( inout ) :: cfg
      type ( samr_box_t ), intent ( inout ) :: box
      real ( kind=8 ), intent ( in ) :: dx(3), dt
      type ( cfl_t ), intent ( in ) :: cfl
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( irs_t ), intent ( inout ) :: irs
      type ( slope_limiter_t ), intent ( in ) :: sl
      type ( mh_workspace_t ), intent ( inout ) :: ws
      type ( log_t ), intent ( inout ) :: log
    end subroutine rhyme_muscl_hancock_solve
  end interface

contains


end module rhyme_muscl_hancock
