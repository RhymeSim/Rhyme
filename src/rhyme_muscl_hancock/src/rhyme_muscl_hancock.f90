module rhyme_muscl_hancock
  use rhyme_mh_workspace
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_slope_limiter
  use rhyme_irs
  use rhyme_log

  implicit none


  type rhyme_muscl_hancock_indices_t
  end type rhyme_muscl_hancock_indices_t

  type ( rhyme_muscl_hancock_indices_t ), parameter :: mhid = rhyme_muscl_hancock_indices_t ()


  type muscl_hancock_t
    logical :: initialized = .false.
    type ( mh_workspace_t ) :: ws
    logical :: active_axis(3) = .false.
    integer :: active_flux(3) = 0
  contains
    procedure :: init => rhyme_muscl_hancock_init
    procedure :: solve => rhyme_muscl_hancock_solve_memory_intensive
    procedure :: solve_memory_intensive => rhyme_muscl_hancock_solve_memory_intensive
    procedure :: solve_cpu_intensive => rhyme_muscl_hancock_solve_cpu_intensive
  end type muscl_hancock_t

  interface
    pure module subroutine rhyme_muscl_hancock_solve_cpu_intensive ( &
      this, box, dx, dt, cfl, ig, irs, sl )
      class ( muscl_hancock_t ), intent ( inout ) :: this
      type ( samr_box_t ), intent ( inout ) :: box
      real ( kind=8 ), intent ( in ) :: dx(3), dt
      type ( cfl_t ), intent ( in ) :: cfl
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( irs_t ), intent ( inout ) :: irs
      type ( slope_limiter_t ), intent ( in ) :: sl
    end subroutine rhyme_muscl_hancock_solve_cpu_intensive

    pure module subroutine rhyme_muscl_hancock_solve_memory_intensive ( &
      this, box, dx, dt, cfl, ig, irs, sl )
      class ( muscl_hancock_t ), intent ( inout ) :: this
      type ( samr_box_t ), intent ( inout ) :: box
      real ( kind=8 ), intent ( in ) :: dx(3), dt
      type ( cfl_t ), intent ( in ) :: cfl
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( irs_t ), intent ( inout ) :: irs
      type ( slope_limiter_t ), intent ( in ) :: sl
    end subroutine rhyme_muscl_hancock_solve_memory_intensive
  end interface

contains

  subroutine rhyme_muscl_hancock_init ( this, samr, log )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr
    type ( log_t ), intent ( inout ) :: log

    if ( this%initialized ) then
      call log%warn( 'Try to re-initialize muscl_hancock object' )
      return
    end if

    call this%ws%init( samr, log )

    this%active_axis = samr%base_grid > 1
    this%active_flux = merge( 1, 0, this%active_axis )

    this%initialized = .true.
  end subroutine rhyme_muscl_hancock_init

end module rhyme_muscl_hancock
