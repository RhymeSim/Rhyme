module rhyme_samr_bc
  use rhyme_samr
  use rhyme_log

  implicit none


  type samr_bc_indices_t
    integer :: unset = -1
    integer :: reflective = 1, outflow = 2, periodic = 3
    integer :: left = samrid%left, right = samrid%right
    integer :: bottom = samrid%bottom, top = samrid%top
    integer :: back = samrid%back, front = samrid%front
  end type samr_bc_indices_t


  type ( samr_bc_indices_t ), parameter :: bcid = samr_bc_indices_t ()


  type samr_bc_t
    integer :: types(6) = bcid%reflective
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_samr_bc_init
    procedure :: set_base_grid_boundaries => rhyme_samr_bc_set_base_grid_boundaries
    procedure :: set_base_grid_left_boundary => rhyme_samr_bc_set_base_grid_left_boundary
    procedure :: set_base_grid_right_boundary => rhyme_samr_bc_set_base_grid_right_boundary
    procedure :: set_base_grid_bottom_boundary => rhyme_samr_bc_set_base_grid_bottom_boundary
    procedure :: set_base_grid_top_boundary => rhyme_samr_bc_set_base_grid_top_boundary
    procedure :: set_base_grid_back_boundary => rhyme_samr_bc_set_base_grid_back_boundary
    procedure :: set_base_grid_front_boundary => rhyme_samr_bc_set_base_grid_front_boundary
  end type samr_bc_t


  interface
    pure module subroutine rhyme_samr_bc_set_base_grid_boundaries ( this, samr )
      class ( samr_bc_t ), intent ( in ) :: this
      type ( samr_t ), intent ( inout ) :: samr
    end subroutine rhyme_samr_bc_set_base_grid_boundaries
  end interface

contains

  subroutine rhyme_samr_bc_init ( this, samr, log )
    implicit none

    class ( samr_bc_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( inout ) :: samr
    type ( log_t ), intent ( inout ) :: log

    integer :: i, j, k, lb(3), ub(3)

    if ( .not. samr%initialized ) then
      call log%err( 'SAMR object need to be initialized before initializing SAMR_BC' )
      return
    end if

    if ( this%initialized ) then
      call log%warn( 'Trying to re-initialize SAMR_BC object' )
      return
    end if

    lb = - samr%ghost_cells + 1
    ub = samr%base_grid + samr%ghost_cells

    do k = lb(3), ub(3)
      do j = lb(2), ub(2)
        do i = lb(1), ub(1)
          if ( &
            i < 1 .or. j < 1 .or. k < 1 &
            .or. i > samr%levels(0)%boxes(1)%dims(1) &
            .or. j > samr%levels(0)%boxes(1)%dims(2) &
            .or. k > samr%levels(0)%boxes(1)%dims(3) &
          ) then
            samr%levels(0)%boxes(1)%flags(i, j, k) = samrid%ghost
            samr%levels(0)%boxes(1)%hydro(i, j, k)%u = 0.d0
          end if
        end do
      end do
    end do

    this%initialized = .true.
  end subroutine rhyme_samr_bc_init


  pure subroutine rhyme_samr_bc_set_base_grid_left_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_box_t ), intent ( inout ) :: box

    integer :: j, k

    select case ( this%types( bcid%left ) )
    case ( bcid%reflective )
      do k = 1, box%dims(3)
        do j = 1, box%dims(2)
          box%hydro(  0, j, k )%u = box%hydro( 1, j, k )%u
          box%hydro(  0, j, k )%u( hyid%rho_u ) = -box%hydro( 1, j, k )%u( hyid%rho_u )
          box%hydro( -1, j, k )%u = box%hydro( 2, j, k )%u
          box%hydro( -1, j, k )%u( hyid%rho_u ) = -box%hydro( 2, j, k )%u( hyid%rho_u )
        end do
      end do
    case ( bcid%outflow )
      do k = 1, box%dims(3)
        do j = 1, box%dims(2)
          box%hydro(  0, j, k )%u = box%hydro( 1, j, k )%u
          box%hydro( -1, j, k )%u = box%hydro( 2, j, k )%u
        end do
      end do
    case ( bcid%periodic )
      do k = 1, box%dims(3)
        do j = 1, box%dims(2)
          box%hydro(  0, j, k )%u = box%hydro( box%dims(1)-0, j, k )%u
          box%hydro( -1, j, k )%u = box%hydro( box%dims(1)-1, j, k )%u
        end do
      end do
    end select
  end subroutine rhyme_samr_bc_set_base_grid_left_boundary


  pure subroutine rhyme_samr_bc_set_base_grid_right_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type(samr_box_t), intent ( inout ) :: box

    integer :: j, k

    select case ( this%types( bcid%right ) )
    case ( bcid%reflective )
      do k = 1, box%dims(3)
        do j = 1, box%dims(2)
          box%hydro( box%dims(1)+1, j, k )%u = box%hydro( box%dims(1)-0, j, k )%u
          box%hydro( box%dims(1)+1, j, k )%u( hyid%rho_u ) = -box%hydro( box%dims(1)-0, j, k )%u( hyid%rho_u )
          box%hydro( box%dims(1)+2, j, k )%u = box%hydro( box%dims(1)-1, j, k )%u
          box%hydro( box%dims(1)+2, j, k )%u( hyid%rho_u ) = -box%hydro( box%dims(1)-1, j, k )%u( hyid%rho_u )
        end do
      end do
    case ( bcid%outflow )
      do k = 1, box%dims(3)
        do j = 1, box%dims(2)
          box%hydro( box%dims(1)+1, j, k )%u = box%hydro( box%dims(1)-0, j, k )%u
          box%hydro( box%dims(1)+2, j, k )%u = box%hydro( box%dims(1)-1, j, k )%u
        end do
      end do
    case ( bcid%periodic )
      do k = 1, box%dims(3)
        do j = 1, box%dims(2)
          box%hydro( box%dims(1)+1, j, k )%u = box%hydro( 1, j, k )%u
          box%hydro( box%dims(1)+2, j, k )%u = box%hydro( 2, j, k )%u
        end do
      end do
    end select
  end subroutine rhyme_samr_bc_set_base_grid_right_boundary


  pure subroutine rhyme_samr_bc_set_base_grid_bottom_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_box_t ), intent ( inout ) :: box

    integer :: i, k

    select case ( this%types ( bcid%bottom ) )
    case ( bcid%reflective )
      do k = 1, box%dims(3)
        do i = 1, box%dims(1)
          box%hydro( i,  0, k )%u = box%hydro( i, 1, k )%u
          box%hydro( i,  0, k )%u( hyid%rho_v ) = -box%hydro( i, 1, k )%u( hyid%rho_v )
          box%hydro( i, -1, k )%u = box%hydro( i, 2, k )%u
          box%hydro( i, -1, k )%u( hyid%rho_v ) = -box%hydro( i, 2, k )%u( hyid%rho_v )
        end do
      end do
    case ( bcid%outflow )
      do k = 1, box%dims(3)
        do i = 1, box%dims(1)
          box%hydro( i,  0, k )%u = box%hydro( i, 1, k )%u
          box%hydro( i, -1, k )%u = box%hydro( i, 2, k )%u
        end do
      end do
    case ( bcid%periodic )
      do k = 1, box%dims(3)
        do i = 1, box%dims(1)
          box%hydro( i,  0, k )%u = box%hydro( i, box%dims(2)-0, k )%u
          box%hydro( i, -1, k )%u = box%hydro( i, box%dims(2)-1, k )%u
        end do
      end do
    end select
  end subroutine rhyme_samr_bc_set_base_grid_bottom_boundary


  pure subroutine rhyme_samr_bc_set_base_grid_top_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_box_t ), intent ( inout ) :: box

    integer :: i, k

    select case ( this%types( bcid%top ) )
    case ( bcid%reflective )
      do k = 1, box%dims(3)
        do i = 1, box%dims(1)
          box%hydro( i, box%dims(2)+1, k )%u = box%hydro( i, box%dims(2)-0, k )%u
          box%hydro( i, box%dims(2)+1, k )%u( hyid%rho_v ) = -box%hydro( i, box%dims(2)-0, k )%u( hyid%rho_v )
          box%hydro( i, box%dims(2)+2, k )%u = box%hydro( i, box%dims(2)-1, k )%u
          box%hydro( i, box%dims(2)+2, k )%u( hyid%rho_v ) = -box%hydro( i, box%dims(2)-1, k )%u( hyid%rho_v )
        end do
      end do
    case ( bcid%outflow )
      do k = 1, box%dims(3)
        do i = 1, box%dims(1)
          box%hydro( i, box%dims(2)+1, k )%u = box%hydro( i, box%dims(2)-0, k )%u
          box%hydro( i, box%dims(2)+2, k )%u = box%hydro( i, box%dims(2)-1, k )%u
        end do
      end do
    case ( bcid%periodic )
      do k = 1, box%dims(3)
        do i = 1, box%dims(1)
          box%hydro( i, box%dims(2)+1, k )%u = box%hydro( i, 1, k )%u
          box%hydro( i, box%dims(2)+2, k )%u = box%hydro( i, 2, k )%u
        end do
      end do
    end select
  end subroutine rhyme_samr_bc_set_base_grid_top_boundary


  pure subroutine rhyme_samr_bc_set_base_grid_back_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_box_t ), intent ( inout ) :: box

    integer :: i, j

    select case ( this%types( bcid%back ) )
    case ( bcid%reflective )
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)
          box%hydro( i, j,  0 )%u = box%hydro( i, j, 1 )%u
          box%hydro( i, j,  0 )%u( hyid%rho_w ) = -box%hydro( i, j, 1 )%u( hyid%rho_w )
          box%hydro( i, j, -1 )%u = box%hydro( i, j, 2 )%u
          box%hydro( i, j, -1 )%u( hyid%rho_w ) = -box%hydro( i, j, 2 )%u( hyid%rho_w )
        end do
      end do
    case ( bcid%outflow )
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)
          box%hydro( i, j,  0 )%u = box%hydro( i, j, 1 )%u
          box%hydro( i, j, -1 )%u = box%hydro( i, j, 2 )%u
        end do
      end do
    case ( bcid%periodic )
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)
          box%hydro( i, j,  0 )%u = box%hydro( i, j, box%dims(3)-0 )%u
          box%hydro( i, j, -1 )%u = box%hydro( i, j, box%dims(3)-1 )%u
        end do
      end do
    end select
  end subroutine rhyme_samr_bc_set_base_grid_back_boundary


  pure subroutine rhyme_samr_bc_set_base_grid_front_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_box_t ), intent ( inout ) :: box

    integer :: i, j

    select case ( this%types( bcid%front ) )
    case ( bcid%reflective )
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)
          box%hydro( i, j, box%dims(3)+1 )%u = box%hydro( i, j, box%dims(3)-0 )%u
          box%hydro( i, j, box%dims(3)+1 )%u( hyid%rho_w ) = -box%hydro( i, j, box%dims(3)-0 )%u( hyid%rho_w )
          box%hydro( i, j, box%dims(3)+2 )%u = box%hydro( i, j, box%dims(3)-1 )%u
          box%hydro( i, j, box%dims(3)+2 )%u( hyid%rho_w ) = -box%hydro( i, j, box%dims(3)-1 )%u( hyid%rho_w )
        end do
      end do
    case ( bcid%outflow )
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)
          box%hydro( i, j, box%dims(3)+1 )%u = box%hydro( i, j, box%dims(3)-0 )%u
          box%hydro( i, j, box%dims(3)+2 )%u = box%hydro( i, j, box%dims(3)-1 )%u
        end do
      end do
    case ( bcid%periodic )
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)
          box%hydro( i, j, box%dims(3)+1 )%u = box%hydro( i, j, 1 )%u
          box%hydro( i, j, box%dims(3)+2 )%u = box%hydro( i, j, 2 )%u
        end do
      end do
    end select
  end subroutine rhyme_samr_bc_set_base_grid_front_boundary
end module rhyme_samr_bc
