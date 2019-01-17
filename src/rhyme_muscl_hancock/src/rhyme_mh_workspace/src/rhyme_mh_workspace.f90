module rhyme_mh_workspace
  use rhyme_hydro_base
  use rhyme_riemann_problem
  use rhyme_samr
  implicit none

  type rhyme_mh_workspace_indices_t
    integer :: memory_intensive = 10, cpu_intensive = 11
  end type rhyme_mh_workspace_indices_t

  type ( rhyme_mh_workspace_indices_t ), parameter :: wsid = rhyme_mh_workspace_indices_t ()

  type mh_workspace_box_t
    type ( hydro_conserved_t ), allocatable :: U_sl ( :, :, :, : )
    type ( hydro_conserved_t ), allocatable :: U_side ( :, :, :, : )
    type ( hydro_flux_t ), allocatable :: F_dir ( :, :, :, : )
    type ( hydro_conserved_t ) :: phi
    type ( rp_star_region_t ) :: star
  end type mh_workspace_box_t

  type mh_workspace_level_t
    integer :: nboxes, tot_nboxes
    type ( mh_workspace_box_t ), allocatable :: boxes(:)
  end type mh_workspace_level_t

  type mh_workspace_t
    integer :: nlevels, type = wsid%memory_intensive
    logical :: initialized
    type ( mh_workspace_level_t ) :: levels(0:23)
  contains
    procedure :: init => rhyme_mh_workspace_init
    procedure :: setup_workspace => rhyme_mh_workspace_setup_workspace
  end type mh_workspace_t

  type ( mh_workspace_t ), save :: mhws

contains

  subroutine rhyme_mh_workspace_init ( this )
    implicit none

    class ( mh_workspace_t ), intent ( in ) :: this

  end subroutine rhyme_mh_workspace_init


  subroutine rhyme_mh_workspace_setup_workspace ( this, samr )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr

    if ( this%initialized ) return

    if ( this%type .eq. wsid%memory_intensive ) then
      call setup_memory_intensive_workspace
    else if ( this%type .eq. wsid%memory_intensive ) then
      call setup_cpu_intensive_workspace
    end if

    this%initialized = .true.

  contains

    subroutine setup_memory_intensive_workspace ()
      implicit none

      integer :: i, l, b, d(3,2)

      mhws%nlevels = samr%nlevels

      do l = 0, samr%nlevels - 1
        allocate ( mhws%levels(l)%boxes ( samr%levels(l)%tot_nboxes ) )

        mhws%levels(l)%tot_nboxes = samr%levels(l)%tot_nboxes
        mhws%levels(l)%nboxes = 0

        do b = 1, samr%levels(l)%nboxes

          do i = 1, 3
            if ( samr%base_grid(i) .le. 1 ) then
              d(i, :) = 1
            else
              d(i, 1) = -samr%ghost_cells(i) + 2
              d(i, 2) = samr%levels(l)%boxes(b)%dims(i) + samr%ghost_cells(i) - 1
            endif
          end do

          allocate ( mhws%levels(l)%boxes(b)%U_sl ( d(1,1):d(1,2), d(2,1):d(2,2), d(3,1):d(3,2), 3 ) )
          allocate ( mhws%levels(l)%boxes(b)%U_side ( d(1,1):d(1,2), d(2,1):d(2,2), d(3,1):d(3,2), 6 ) )
          allocate ( mhws%levels(l)%boxes(b)%F_dir ( d(1,1):d(1,2), d(2,1):d(2,2), d(3,1):d(3,2), 3 ) )

          mhws%levels(l)%nboxes = mhws%levels(l)%nboxes + 1
        end do
      end do
    end subroutine setup_memory_intensive_workspace

    subroutine setup_cpu_intensive_workspace ()
      implicit none

      ! TODO: Need to be implemented
      print *, "Not implemented yet!"
      stop
    end subroutine setup_cpu_intensive_workspace
  end subroutine rhyme_mh_workspace_setup_workspace
end module rhyme_mh_workspace
