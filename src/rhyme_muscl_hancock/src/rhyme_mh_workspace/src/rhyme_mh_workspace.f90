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
  end type mh_workspace_t

  interface
    module subroutine rhyme_mh_workspace_init ( mhws, samr, logger )
      class ( mh_workspace_t ), intent ( inout ) :: mhws
      type ( samr_t ), intent ( in ) :: samr
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_mh_workspace_init

    pure module subroutine rhyme_mh_workspace_check ( mhws, box )
      class ( mh_workspace_t ), intent ( inout ) :: mhws
      type ( samr_box_t ), intent ( in ) :: box
    end subroutine rhyme_mh_workspace_check
  end interface

end module rhyme_mh_workspace
