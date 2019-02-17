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
    logical :: initialized = .false.
    integer :: type = wsid%memory_intensive
    type ( mh_workspace_level_t ) :: levels ( 0:samrid%max_nlevels )
    type ( samr_t ), pointer :: samr
  contains
    procedure :: init => rhyme_mh_workspace_init
    procedure :: check => rhyme_mh_workspace_check
  end type mh_workspace_t

contains

  pure subroutine rhyme_mh_workspace_init ( this, samr )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr
    integer :: l


    if ( this%initialized ) return

    this%nlevels = samr%nlevels

    if ( this%type .eq. wsid%memory_intensive ) then

      do l = 0, samr%nlevels - 1
        allocate ( this%levels(l)%boxes ( samr%levels(l)%max_nboxes ) )
        this%levels(l)%max_nboxes = samr%levels(l)%max_nboxes
      end do

    else if ( this%type .eq. wsid%cpu_intensive ) then
      ! TODO: Implement cpu_intensive
    end if

    this%initialized = .true.
  end subroutine rhyme_mh_workspace_init


  pure subroutine rhyme_mh_workspace_check ( this, l, b, hydro_box )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: this
    integer, intent ( in ) :: l, b
    type ( samr_box_t ), intent ( in ) :: hydro_box

    integer :: lb(3), ub(3), wslb(4), wsub(4), stat


    if ( this%type .eq. wsid%cpu_intensive ) return


    lb = lbound ( hydro_box%hydro )
    ub = ubound ( hydro_box%hydro )

    if ( allocated( this%levels(l)%boxes(b)%UL ) ) then

      wslb = lbound ( this%levels(l)%boxes(b)%UL )
      wsub = ubound ( this%levels(l)%boxes(b)%UL )

      if ( any( lb .ne. wslb(:3) ) .or. any( ub .ne. wsub(:3) ) ) then
        deallocate ( this%levels(l)%boxes(b)%UL, stat=stat )
        deallocate ( this%levels(l)%boxes(b)%UR, stat=stat )
        deallocate ( this%levels(l)%boxes(b)%FR, stat=stat )

        if ( stat == 0 ) then
          allocate ( this%levels(l)%boxes(b)%UL ( &
            lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
          ))
          allocate ( this%levels(l)%boxes(b)%UR ( &
            lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
          ))
          allocate ( this%levels(l)%boxes(b)%FR ( &
            lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
          ))
        end if
      end if
    else
      allocate ( this%levels(l)%boxes(b)%UL ( &
        lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
      ))
      allocate ( this%levels(l)%boxes(b)%UR ( &
        lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
      ))
      allocate ( this%levels(l)%boxes(b)%FR ( &
        lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
      ))
    end if
  end subroutine rhyme_mh_workspace_check

end module rhyme_mh_workspace