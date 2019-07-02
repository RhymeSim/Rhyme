module rhyme_muscl_hancock
  use rhyme_mh_workspace
  use rhyme_ideal_gas
  use rhyme_slope_limiter
  use rhyme_irs
  use rhyme_log

  implicit none

  type, private :: rhyme_muscl_hancock_indices_t
    integer :: cpu_intensive = mhwsid%cpu_intensive
    integer :: memory_intensive = mhwsid%memory_intensive
  end type rhyme_muscl_hancock_indices_t

  type ( rhyme_muscl_hancock_indices_t ), parameter :: mhid = rhyme_muscl_hancock_indices_t ()


  type muscl_hancock_t
    integer :: solver_type = mhid%memory_intensive
  end type muscl_hancock_t

  interface
    module subroutine rhyme_muscl_hancock_init ( mh, samr, ws, logger )
      type ( muscl_hancock_t ), intent ( inout ) :: mh
      type ( samr_t ), intent ( in ) :: samr
      type ( mh_workspace_t ), intent ( inout ) :: ws
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_muscl_hancock_init

    pure module subroutine rhyme_muscl_hancock_solve_cpu_intensive ( &
      box, dx, dt, irs, sl, ws )
      type ( samr_box_t ), intent ( inout ) :: box
      real ( kind=8 ), intent ( in ) :: dx( NDIM ), dt
      type ( irs_t ), intent ( inout ) :: irs
      type ( slope_limiter_t ), intent ( in ) :: sl
      type ( mh_workspace_t ), intent ( inout ) :: ws
    end subroutine rhyme_muscl_hancock_solve_cpu_intensive

    pure module subroutine rhyme_muscl_hancock_solve_memory_intensive ( &
      box, dx, dt, irs, sl, ws )
      type ( samr_box_t ), intent ( inout ) :: box
      real ( kind=8 ), intent ( in ) :: dx( NDIM ), dt
      type ( irs_t ), intent ( inout ) :: irs
      type ( slope_limiter_t ), intent ( in ) :: sl
      type ( mh_workspace_t ), intent ( inout ) :: ws
    end subroutine rhyme_muscl_hancock_solve_memory_intensive

    module subroutine rhyme_muscl_hancock_solve ( &
      mh, box, dx, dt, irs, sl, ws, logger )
      type ( muscl_hancock_t ), intent ( inout ) :: mh
      type ( samr_box_t ), intent ( inout ) :: box
      real ( kind=8 ), intent ( in ) :: dx( NDIM ), dt
      type ( irs_t ), intent ( inout ) :: irs
      type ( slope_limiter_t ), intent ( in ) :: sl
      type ( mh_workspace_t ), intent ( inout ) :: ws
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_muscl_hancock_solve

    pure module subroutine rhyme_muscl_hancock_half_step_extrapolation ( &
      u, delta, axis, dx, dt, l, r )
      real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( in ) :: u, delta
      integer, intent ( in ) :: axis
      real ( kind=8 ), intent ( in ) :: dx, dt
      real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( out ) :: l, r
    end subroutine rhyme_muscl_hancock_half_step_extrapolation
  end interface
end module rhyme_muscl_hancock
