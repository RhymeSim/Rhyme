module rhyme_mh_workspace
  use rhyme_hydro_base
  use rhyme_riemann_problem
  use rhyme_samr
  use rhyme_log

  implicit none

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
    type ( mh_workspace_level_t ) :: levels ( 0:samrid%max_nlevels )
    type ( samr_t ), pointer :: samr
  contains
    procedure :: init => rhyme_mh_workspace_init
    procedure :: check => rhyme_mh_workspace_check
  end type mh_workspace_t

contains

  subroutine rhyme_mh_workspace_init ( this, samr, log )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr
    type ( log_t ), intent ( inout ) :: log

    integer :: l

    if ( this%initialized ) then
      call log%warn( 'Try to re-initialize workspace' )
      return
    end if

    this%nlevels = samr%nlevels

    do l = 0, samr%nlevels - 1
      allocate( this%levels(l)%boxes( samr%levels(l)%max_nboxes ) )
      this%levels(l)%max_nboxes = samr%levels(l)%max_nboxes
    end do

    this%initialized = .true.
  end subroutine rhyme_mh_workspace_init


  pure subroutine rhyme_mh_workspace_check ( this, hydro_box )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( in ) :: hydro_box

    integer :: l, b
    integer :: lb(3), ub(3), wslb(4), wsub(4), stat

    l = hydro_box%level
    b = hydro_box%number

    lb = lbound ( hydro_box%hydro )
    ub = ubound ( hydro_box%hydro )

    if ( allocated( this%levels(l)%boxes(b)%UL ) ) then

      wslb = lbound ( this%levels(l)%boxes(b)%UL )
      wsub = ubound ( this%levels(l)%boxes(b)%UL )

      if ( any( lb .ne. wslb(:3) ) .or. any( ub .ne. wsub(:3) ) ) then
        deallocate( this%levels(l)%boxes(b)%UL, stat=stat )
        deallocate( this%levels(l)%boxes(b)%UR, stat=stat )
        deallocate( this%levels(l)%boxes(b)%FR, stat=stat )

        if ( stat == 0 ) then
          allocate( this%levels(l)%boxes(b)%UL ( &
            lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
          ))
          allocate( this%levels(l)%boxes(b)%UR ( &
            lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
          ))
          allocate( this%levels(l)%boxes(b)%FR ( &
            lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
          ))
        end if
      end if
    else
      allocate( this%levels(l)%boxes(b)%UL ( &
        lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
      ))
      allocate( this%levels(l)%boxes(b)%UR ( &
        lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
      ))
      allocate( this%levels(l)%boxes(b)%FR ( &
        lb(1):ub(1), lb(2):ub(2), lb(3):ub(3), 3 &
      ))
    end if
  end subroutine rhyme_mh_workspace_check
end module rhyme_mh_workspace
