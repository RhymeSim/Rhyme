module rhyme_mh_workspace
  use rhyme_hydro_base
  use rhyme_riemann_problem
  use rhyme_samr

  implicit none

  type rhyme_mh_workspace_indices_t
    integer :: sl = 1, lsides = 2, rsides = 3, fluxes = 4 ! box ws indices
    integer :: memory_intensive = 10, cpu_intensive = 11
  end type rhyme_mh_workspace_indices_t

  type ( rhyme_mh_workspace_indices_t ), parameter :: wsid = rhyme_mh_workspace_indices_t ()


  type mh_workspace_box_t
    type ( hydro_conserved_t ), allocatable :: U( :, :, :, :, : ) ! i, j, k, dir, type
    type ( hydro_conserved_t ) :: phi
    type ( rp_star_region_t ) :: star
  end type mh_workspace_box_t


  type mh_workspace_level_t
    integer :: tot_nboxes
    type ( mh_workspace_box_t ), allocatable :: boxes(:)
  end type mh_workspace_level_t


  type mh_workspace_t
    integer :: nlevels
    integer :: type = wsid%memory_intensive
    logical :: initialized = .false.
    type ( mh_workspace_level_t ) :: levels(0:samrid%max_nlevels)
  contains
    procedure :: init => rhyme_mh_workspace_init
    procedure :: check => rhyme_mh_workspace_check
  end type mh_workspace_t

contains

  subroutine rhyme_mh_workspace_init ( this, samr )
  ! pure subroutine rhyme_mh_workspace_init ( this, samr )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr
    integer :: l


    if ( this%initialized ) return

    this%nlevels = samr%nlevels

    if ( this%type .eq. wsid%memory_intensive ) then

      do l = 0, samr%nlevels - 1
        allocate ( this%levels(l)%boxes ( samr%levels(l)%tot_nboxes ) )
        this%levels(l)%tot_nboxes = samr%levels(l)%tot_nboxes
      end do

    else if ( this%type .eq. wsid%cpu_intensive ) then
      ! TODO: Implement cpu_intensive
    end if

    this%initialized = .true.
  end subroutine rhyme_mh_workspace_init


  pure subroutine rhyme_mh_workspace_check ( this, box, l, b )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( in ) :: box
    integer, intent ( in ) :: l, b

    integer :: lb(3), ub(3), wslb(5), wsub(5)

    if ( this%type .eq. wsid%cpu_intensive ) return

    lb = lbound ( box%hydro )
    ub = ubound ( box%hydro )

    if ( allocated( this%levels(l)%boxes(b)%U ) ) then
      wslb = lbound(this%levels(l)%boxes(b)%U)
      wsub = ubound(this%levels(l)%boxes(b)%U)

      if ( any(lb .ne. wslb(1:3)) .or. any(ub .ne. wsub(1:3)) ) then
        deallocate ( this%levels(l)%boxes(b)%U )

        allocate ( this%levels(l)%boxes(b)%U ( &
          lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3, 4 &
        ))
      end if
    else
      allocate ( this%levels(l)%boxes(b)%U ( &
        lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3, 4 &
      ))
    end if
  end subroutine rhyme_mh_workspace_check

end module rhyme_mh_workspace
