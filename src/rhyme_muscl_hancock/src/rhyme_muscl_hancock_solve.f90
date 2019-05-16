submodule ( rhyme_muscl_hancock ) rhyme_mh_solve_submodule
contains
  module subroutine rhyme_muscl_hancock_solve ( &
    cfg, box, dx, dt, ig, irs, sl, ws, log )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: cfg
    type ( samr_box_t ), intent ( inout ) :: box
    real ( kind=8 ), intent ( in ) :: dx(3), dt
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( irs_t ), intent ( inout ) :: irs
    type ( slope_limiter_t ), intent ( in ) :: sl
    type ( mh_workspace_t ), intent ( inout ) :: ws
    type ( log_t ), intent ( inout ) :: log

    select case ( cfg%solver_type )
    case ( mhid%cpu_intensive )
      call log%log( 'using cpu_intensive solver' )
      call rhyme_muscl_hancock_solve_cpu_intensive( &
        cfg, box, dx, dt, ig, irs, sl, ws )
    case ( mhid%memory_intensive )
      call log%log( 'using memory_intensive solver' )
      call rhyme_muscl_hancock_solve_memory_intensive( &
        cfg, box, dx, dt, ig, irs, sl, ws )
    case DEFAULT
      call log%err( 'Unknown solver type' )
      return
    end select
  end subroutine rhyme_muscl_hancock_solve

end submodule rhyme_mh_solve_submodule
