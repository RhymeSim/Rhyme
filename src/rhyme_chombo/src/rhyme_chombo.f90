module rhyme_chombo
  use rhyme_hdf5_util
  use rhyme_samr

  implicit none

  type rhyme_chombo_indices_t
    integer :: unset = -1
  end type rhyme_chombo_indices_t

  type ( rhyme_chombo_indices_t ), parameter :: chid = rhyme_chombo_indices_t ()

  type, extends ( rhyme_hdf5_util_t ) :: rhyme_chombo_t
    integer :: num_levels = chid%unset
    integer :: num_components = chid%unset
    integer ( hid_t ) :: chombo_global_id
    integer ( hid_t ) :: level_ids(0:23) = chid%unset
  contains
    procedure :: write_samr => rhyme_chombo_write_samr
  end type rhyme_chombo_t

contains

  subroutine rhyme_chombo_write_samr ( this, filename, samr )
    implicit none

    class ( rhyme_chombo_t ), intent (inout) :: this
    character ( len=* ), intent (in) :: filename
    type ( samr_t ), intent (in) :: samr

    integer :: i, ndims
    character ( len=16 ) :: level_name


    if ( this%initialized ) return

    call this%create ( filename )

    do i = 0, samr%nlevels - 1
      write ( level_name, '(A6,I1)') "/level_", i
      call this%create_group ( level_name, this%level_ids(i) )
    end do

    ndims = size ( samr%base_grid ) - sum ( samr%base_grid * merge ( 1, 0, samr%base_grid <= 1 ) )

    call this%add_group_1d_array_attr ( "/", "ProblemDomain", samr%base_grid )
    call this%add_group_attr ( "/", "num_levels", samr%nlevels )
    call this%add_group_attr ( "/", "num_components", 5 )
    call this%add_group_attr ( "/", "component_0", "rho" )
    call this%add_group_attr ( "/", "component_1", "rho_u" )
    call this%add_group_attr ( "/", "component_2", "rho_v" )
    call this%add_group_attr ( "/", "component_3", "rho_w" )
    call this%add_group_attr ( "/", "component_4", "E" )
    ! TODO: not implemented: call this%add_group_attr ( "/", "iteration", samr%levels(0)%iteration )

    call this%create_group ( "/chombo_global", this%chombo_global_id )
    call this%add_group_attr ( "/chombo_global", "SpaceDim", ndims )

  end subroutine rhyme_chombo_write_samr
end module rhyme_chombo
