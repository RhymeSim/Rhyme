module rhyme_samr_bc
  use rhyme_samr

  implicit none


  type samr_bc_indices_t
    integer :: reflective = 1, outflow = 2, periodic = 3
    integer :: left = 1, right = 2, bottom = 3, top = 4, back = 5, front = 6
  end type samr_bc_indices_t


  type ( samr_bc_indices_t ), parameter :: bcid = samr_bc_indices_t ()


  type samr_bc_t
    integer :: types(6) = bcid%reflective
    integer :: ghost_cells(3)
    logical :: initialized = .false.
  contains
    procedure :: init_with => rhyme_samr_bc_init_with
    procedure :: init => rhyme_samr_bc_init
    procedure :: set_base_grid_boundaries => rhyme_samr_bc_set_base_grid_boundaries
    procedure :: set_base_grid_left_boundary => rhyme_samr_bc_set_base_grid_left_boundary
    procedure :: set_base_grid_right_boundary => rhyme_samr_bc_set_base_grid_right_boundary
    procedure :: set_base_grid_bottom_boundary => rhyme_samr_bc_set_base_grid_bottom_boundary
    procedure :: set_base_grid_top_boundary => rhyme_samr_bc_set_base_grid_top_boundary
    procedure :: set_base_grid_back_boundary => rhyme_samr_bc_set_base_grid_back_boundary
    procedure :: set_base_grid_front_boundary => rhyme_samr_bc_set_base_grid_front_boundary
  end type samr_bc_t

contains

  ! @param[in] bc_types Boundary conditions types (left, right, bottom, top, back, front)
  subroutine rhyme_samr_bc_init_with ( this, samr, bc_types )
    implicit none

    class (samr_bc_t), intent(inout) :: this
    type (samr_t), intent(inout) :: samr
    integer, intent(in) :: bc_types(6)

    if ( .not. samr%initialized ) return

    this%types(:) = bc_types(:)
    this%ghost_cells(:) = samr%ghost_cells(:)

    call rhyme_samr_bc_init ( this, samr )

  end subroutine rhyme_samr_bc_init_with

  ! Initializing boundary condition object
  ! @param[in] samr Initialized structred AMR object
  subroutine rhyme_samr_bc_init ( this, samr )
    implicit none

    class (samr_bc_t), intent(inout) :: this
    type (samr_t), intent(inout) :: samr

    integer :: i, j, k, lb(3), ub(3)

    if ( .not. samr%initialized ) return

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
          end if
        end do
      end do
    end do

    this%initialized = .true.
  end subroutine rhyme_samr_bc_init


  subroutine rhyme_samr_bc_set_base_grid_boundaries (this, samr)
    implicit none

    class (samr_bc_t), intent(in) :: this
    type (samr_t), intent(inout) :: samr

    if ( .not. this%initialized ) return
    if ( .not. samr%initialized ) return


    if ( samr%ghost_cells(1) .ne. 0 ) then
      call this%set_base_grid_left_boundary ( samr%levels(0)%boxes(1) )
      call this%set_base_grid_right_boundary ( samr%levels(0)%boxes(1) )
    end if

    if ( samr%ghost_cells(2) .ne. 0 ) then
      call this%set_base_grid_bottom_boundary ( samr%levels(0)%boxes(1) )
      call this%set_base_grid_top_boundary ( samr%levels(0)%boxes(1) )
    end if

    if ( samr%ghost_cells(3) .ne. 0 ) then
      call this%set_base_grid_back_boundary ( samr%levels(0)%boxes(1) )
      call this%set_base_grid_front_boundary ( samr%levels(0)%boxes(1) )
    end if
  end subroutine rhyme_samr_bc_set_base_grid_boundaries


  pure subroutine rhyme_samr_bc_set_base_grid_left_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_box_t ), intent ( inout ) :: box

    integer :: i, d_ref, lb, dim1

    select case ( this%types( bcid%left ) )
    case ( bcid%reflective )
      do i = 1, this%ghost_cells(1)
        d_ref = i - int( 2.d0 * (real(i) - .5) )
        box%hydro(d_ref,:,:) = box%hydro(i,:,:)
        box%hydro(d_ref,:,:)%u(hyid%rho_u) = -box%hydro(d_ref,:,:)%u(hyid%rho_u)
      end do

    case ( bcid%outflow )
      do i = 1, this%ghost_cells(1)
        d_ref = i - int( 2.d0 * (real(i) - .5) )
        box%hydro(d_ref,:,:) = box%hydro(i,:,:)
      end do

    case ( bcid%periodic )
      lb = - this%ghost_cells(1) + 1
      dim1 = box%dims(1)
      box%hydro( lb:0,:,: ) = box%hydro( dim1+lb:dim1,:,: )
    end select

  end subroutine rhyme_samr_bc_set_base_grid_left_boundary


  pure subroutine rhyme_samr_bc_set_base_grid_right_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type(samr_box_t), intent ( inout ) :: box

    integer :: i, d_ref, dim1, gcell1

    dim1 = box%dims(1)
    gcell1 = this%ghost_cells(1)

    select case ( this%types( bcid%right ) )
    case ( bcid%reflective )
      do i = dim1 - gcell1 + 1, dim1
        d_ref = i + int( 2.d0 * ( (real(dim1) + .5) - real(i) ) )
        box%hydro( d_ref,:,: ) = box%hydro( i,:,: )
        box%hydro( d_ref,:,: )%u(hyid%rho_u) = -box%hydro( d_ref,:,: )%u(hyid%rho_u)
      end do

    case ( bcid%outflow )
      do i = dim1 - gcell1 + 1, dim1
        d_ref = i + int( 2.d0 * ( (real(dim1) + .5) - real(i) ) )
        box%hydro( d_ref,:,: ) = box%hydro( i,:,: )
      end do

    case ( bcid%periodic )
      box%hydro( dim1+1:dim1+gcell1,:,: ) = box%hydro( 1:gcell1,:,: )
    end select

  end subroutine rhyme_samr_bc_set_base_grid_right_boundary


  pure subroutine rhyme_samr_bc_set_base_grid_bottom_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_box_t ), intent ( inout ) :: box

    integer :: i, d_ref, dim2, gcell2

    select case ( this%types ( bcid%bottom ) )
    case ( bcid%reflective )
      do i = 1, this%ghost_cells(2)
        d_ref = i - int( 2.d0 * (real(i) - .5) )
        box%hydro( :,d_ref,: ) = box%hydro( :, i,: )
        box%hydro( :,d_ref,: )%u(hyid%rho_v) = -box%hydro( :,d_ref,: )%u(hyid%rho_v)
      end do

    case ( bcid%outflow )
      do i = 1, this%ghost_cells(2)
        d_ref = i - int( 2.d0 * (real(i) - .5) )
        box%hydro( :,d_ref,: ) = box%hydro( :,i,: )
      end do

    case ( bcid%periodic )
      dim2 = box%dims(2)
      gcell2 = this%ghost_cells(2)
      box%hydro( :,-gcell2+1:0,: ) = box%hydro( :,dim2-gcell2+1:dim2,:)
    end select

  end subroutine rhyme_samr_bc_set_base_grid_bottom_boundary


  subroutine rhyme_samr_bc_set_base_grid_top_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_box_t ), intent ( inout ) :: box

    integer :: i, d_ref, dim2, gcell2

    dim2 = box%dims(2)
    gcell2 = this%ghost_cells(2)

    select case ( this%types( bcid%top ) )
    case ( bcid%reflective )
      do i = dim2 - gcell2 + 1, dim2
        d_ref = i + int( 2.d0 * ( (real(dim2) + .5) - real(i) ) )
        box%hydro( :,d_ref,: ) = box%hydro( :,i,: )
        box%hydro( :,d_ref,: )%u(hyid%rho_v) = -box%hydro( :,d_ref,: )%u(hyid%rho_v)
      end do

    case ( bcid%outflow )
      do i = dim2 - gcell2 + 1, dim2
        d_ref = i + int( 2.d0 * ( (real(dim2) + .5) - real(i) ) )
        box%hydro( :,d_ref,: ) = box%hydro( :,i,: )
      end do

    case ( bcid%periodic )
      box%hydro( :,dim2+1:dim2+gcell2,: ) = box%hydro( :,1:gcell2,: )
    end select

  end subroutine rhyme_samr_bc_set_base_grid_top_boundary


  subroutine rhyme_samr_bc_set_base_grid_back_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_box_t ), intent ( inout ) :: box

    integer :: i, d_ref, dim3, gcell3

    dim3 = box%dims(3)
    gcell3 = this%ghost_cells(3)

    select case ( this%types( bcid%back ) )
    case ( bcid%reflective )
      do i = 1, gcell3
        d_ref = i - int( 2.d0 * (real(i) - .5) )
        box%hydro( :,:,d_ref ) = box%hydro( :,:, i )
        box%hydro( :,:,d_ref )%u(hyid%rho_w) = -box%hydro( :,:,d_ref )%u(hyid%rho_w)
      end do

    case ( bcid%outflow )
      do i = 1, gcell3
        d_ref = i - int( 2.d0 * (real(i) - .5) )
        box%hydro( :,:,d_ref ) = box%hydro( :,:,i )
      end do

    case ( bcid%periodic )
      box%hydro( :,:,-gcell3+1:0 ) = box%hydro( :,:,dim3-gcell3+1:dim3 )
    end select

  end subroutine rhyme_samr_bc_set_base_grid_back_boundary


  subroutine rhyme_samr_bc_set_base_grid_front_boundary ( this, box )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_box_t ), intent ( inout ) :: box

    integer :: i, d_ref, dim3, gcell3

    dim3 = box%dims(3)
    gcell3 = this%ghost_cells(3)

    select case ( this%types( bcid%front ) )
    case ( bcid%reflective )
      do i = dim3 - gcell3 + 1, dim3
        d_ref = i + int( 2.d0 * ( (real(dim3) + .5) - real(i) ) )
        box%hydro( :,:,d_ref ) = box%hydro( :,:,i )
        box%hydro( :,:,d_ref )%u(hyid%rho_w) = -box%hydro( :,:,d_ref )%u(hyid%rho_w)
      end do

    case ( bcid%outflow )
      do i = dim3 - gcell3 + 1, dim3
        d_ref = i + int( 2.d0 * ( (real(dim3) + .5) - real(i) ) )
        box%hydro( :,:,d_ref ) = box%hydro( :,:,i )
      end do

    case ( bcid%periodic )
      box%hydro( :,:,dim3+1:dim3+gcell3 ) = box%hydro( :,:,1:gcell3 )
    end select

  end subroutine rhyme_samr_bc_set_base_grid_front_boundary
end module rhyme_samr_bc