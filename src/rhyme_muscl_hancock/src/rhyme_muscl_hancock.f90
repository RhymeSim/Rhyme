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
  contains
    procedure :: init => rhyme_muscl_hancock_init
  end type muscl_hancock_t

  interface
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

  subroutine rhyme_muscl_hancock_init ( this, samr, ws, log )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr
    type ( mh_workspace_t ), intent ( inout ) :: ws
    type ( log_t ), intent ( inout ) :: log

    call ws%init( samr, log )

    if ( this%initialized ) then
      call log%warn( 'Trying to re-initialize muscl_hancock object' )
      return
    end if

    this%active_axis = samr%base_grid > 1
    this%active_flux = merge( 1, 0, this%active_axis )

    call log%log( 'muscl_hancock object has been initialized', &
      'solver_type', '=', [ this%solver_type ] )

    this%initialized = .true.
  end subroutine rhyme_muscl_hancock_init

end module rhyme_muscl_hancock
