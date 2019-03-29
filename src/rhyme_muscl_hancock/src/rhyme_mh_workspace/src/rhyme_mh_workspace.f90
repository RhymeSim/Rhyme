module rhyme_mh_workspace
  use rhyme_hydro_base
  use rhyme_samr
  use rhyme_log

  implicit none

  type mh_workspace_indices_t
    integer :: cpu_intensive = 1, memory_intensive = 2
  end type mh_workspace_indices_t

  type ( mh_workspace_indices_t ), parameter :: mhwsid = mh_workspace_indices_t()


  type mh_workspace_box_t
    type ( hydro_conserved_t ), allocatable :: U ( :, :, : ) ! i, j, k
    type ( hydro_conserved_t ), allocatable :: UL ( :, :, :, : ) ! i, j, k, dir
    type ( hydro_conserved_t ), allocatable :: UR ( :, :, :, : ) ! i, j, k, dir
    type ( hydro_flux_t ), allocatable :: FR ( :, :, :, : ) ! i, j, k, dir
  end type mh_workspace_box_t


  type mh_workspace_level_t
    integer :: max_nboxes
    type ( mh_workspace_box_t ), allocatable :: boxes(:)
  end type mh_workspace_level_t


  type mh_workspace_t
    integer :: nlevels
    integer :: type = mhwsid%memory_intensive
    logical :: initialized = .false.
    type ( mh_workspace_level_t ) :: levels( 0:samrid%max_nlevels )
    type ( samr_t ), pointer :: samr
  contains
    procedure :: init => rhyme_mh_workspace_init
  end type mh_workspace_t

  interface
    pure module subroutine rhyme_mh_workspace_check ( mhws, box )
      class ( mh_workspace_t ), intent ( inout ) :: mhws
      type ( samr_box_t ), intent ( in ) :: box
    end subroutine rhyme_mh_workspace_check
  end interface

contains

  subroutine rhyme_mh_workspace_init ( this, samr, log )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr
    type ( log_t ), intent ( inout ) :: log

    integer :: l

    if ( this%initialized ) then
      call log%warn( 'Trying to re-initialize mh_workspace object' )
      return
    end if

    this%nlevels = samr%nlevels

    do l = 0, samr%nlevels - 1
      allocate( this%levels(l)%boxes( samr%levels(l)%max_nboxes ) )
      this%levels(l)%max_nboxes = samr%levels(l)%max_nboxes
    end do

    call log%log( 'mh_workspace object has been initialized', &
      'type', '=', [ this%type ] )

    this%initialized = .true.
  end subroutine rhyme_mh_workspace_init

end module rhyme_mh_workspace
