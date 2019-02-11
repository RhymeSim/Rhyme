module rhyme_muscl_hancock
  use rhyme_mh_workspace
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_slope_limiter
  use rhyme_iterative_riemann_solver

  implicit none


  type rhyme_muscl_hancock_indices_t
  end type rhyme_muscl_hancock_indices_t

  type ( rhyme_muscl_hancock_indices_t ), parameter :: mhid = rhyme_muscl_hancock_indices_t ()


  type muscl_hancock_t
    type ( cfl_t ) :: cfl
    type ( ideal_gas_t ) :: ig
    type ( slope_limiter_t ) :: sl
    type ( mh_workspace_t ) :: ws
    logical :: initialized = .false.
  contains
    procedure :: init_with => rhyme_muscl_hancock_init_with
    procedure :: init => rhyme_muscl_hancock_init
  end type muscl_hancock_t

contains

  subroutine rhyme_muscl_hancock_init_with ( this, cfl, ig, sl, samr )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( cfl_t ), intent ( in ) :: cfl
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( slope_limiter_t ), intent ( in ) :: sl
    type ( samr_t ), intent ( in ) :: samr


    if ( this%initialized ) return

    this%cfl = cfl
    this%ig = ig
    this%sl = sl


    call this%init ( samr )
  end subroutine rhyme_muscl_hancock_init_with


  subroutine rhyme_muscl_hancock_init ( this, samr )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr

    integer :: ndim

    if ( this%initialized ) return

    ndim = sum ( merge ( 1, 0, samr%base_grid > 1 ) )

    if ( ndim < 3 ) then
      this%ws%type = wsid%memory_intensive
    else
      ! TODO: Not implemented yet
      this%ws%type = wsid%cpu_intensive
    end if

    call this%ws%init ( samr )

    this%initialized = .true.
  end subroutine rhyme_muscl_hancock_init
end module rhyme_muscl_hancock
