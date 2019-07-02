submodule ( rhyme_muscl_hancock ) rhyme_mh_solve_submodule
contains
  module subroutine rhyme_muscl_hancock_solve ( &
    mh, box, dx, dt, irs, sl, ws, logger )
    implicit none

    type ( muscl_hancock_t ), intent ( inout ) :: mh
    type ( samr_box_t ), intent ( inout ) :: box
    real ( kind=8 ), intent ( in ) :: dx( NDIM ), dt
    type ( irs_t ), intent ( inout ) :: irs
    type ( slope_limiter_t ), intent ( in ) :: sl
    type ( mh_workspace_t ), intent ( inout ) :: ws
    type ( log_t ), intent ( inout ) :: logger

    call logger%begin_section( 'MH_solver' )

    select case ( mh%solver_type )
    case ( mhid%cpu_intensive )
      call logger%log( 'using cpu_intensive solver' )
      call rhyme_muscl_hancock_solve_cpu_intensive( box, dx, dt, irs, sl, ws )
    case ( mhid%memory_intensive )
      call logger%log( 'using memory_intensive solver' )
      call rhyme_muscl_hancock_solve_memory_intensive( box, dx, dt, irs, sl, ws )
    case DEFAULT
      call logger%err( 'Unknown solver type' )
      return
    end select

    call logger%end_section
  end subroutine rhyme_muscl_hancock_solve
end submodule rhyme_mh_solve_submodule
