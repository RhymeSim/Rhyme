module rhyme_samr_bc
  use rhyme_samr

  implicit none


  type samr_bc_indices_t
    integer :: reflective = 1, outflow = 2, periodic = 3
    integer :: left = 1, right = 2, bottom = 3, top = 4, back = 5, front = 6
  end type samr_bc_indices_t


  type (samr_bc_indices_t), parameter :: bcid = samr_bc_indices_t ()


  type samr_bc_t
    integer :: types(6) = bcid%reflective
    logical :: initialized
  contains
    procedure :: init_with => init_samr_bc_with
    procedure :: init => init_samr_bc
    procedure :: set => set_samr_bc
  end type samr_bc_t

contains

  ! @param[in] bc_types Boundary conditions types (left, right, bottom, top, back, front)
  subroutine init_samr_bc_with ( this, samr, bc_types )
    implicit none

    class (samr_bc_t), intent(inout) :: this
    type (samr_t), intent(inout) :: samr
    integer, intent(in) :: bc_types(6)

    if ( .not. samr%initialized ) return

    this%types = bc_types

    call init_samr_bc ( this, samr )

  end subroutine init_samr_bc_with

  ! Initializing boundary condition object
  ! @param[in] samr Initialized structred AMR object
  subroutine init_samr_bc ( this, samr )
    implicit none

    class (samr_bc_t), intent(inout) :: this
    type (samr_t), intent(inout) :: samr

    integer :: i, j, k

    if ( .not. samr%initialized ) return

    do k = -samr%ghost_cells(3) + 1, samr%levels(0)%boxes(1)%dims(3) + samr%ghost_cells(3)
      do j = -samr%ghost_cells(2) + 1, samr%levels(0)%boxes(1)%dims(2) + samr%ghost_cells(2)
        do i = -samr%ghost_cells(1) + 1, samr%levels(0)%boxes(1)%dims(1) + samr%ghost_cells(1)
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
  end subroutine init_samr_bc


  logical function set_samr_bc (this, samr) result (succ)
    implicit none

    class (samr_bc_t), intent(in) :: this
    type (samr_t), intent(inout) :: samr


    succ = .false.

    if ( .not. this%initialized ) return
    if ( .not. samr%initialized ) return


    if ( samr%ghost_cells(1) .ne. 0 ) then
      call set_left_bc ( this%types(bcid%left), samr%ghost_cells(1), samr%levels(0)%boxes(1) )
      call set_right_bc ( this%types(bcid%right), samr%ghost_cells(1), samr%levels(0)%boxes(1) )
    end if

    if ( samr%ghost_cells(2) .ne. 0 ) then
      call set_bottom_bc ( this%types(bcid%bottom), samr%ghost_cells(2), samr%levels(0)%boxes(1) )
      call set_top_bc ( this%types(bcid%top), samr%ghost_cells(2), samr%levels(0)%boxes(1) )
    end if

    if ( samr%ghost_cells(3) .ne. 0 ) then
      call set_back_bc ( this%types(bcid%back), samr%ghost_cells(3), samr%levels(0)%boxes(1) )
      call set_front_bc ( this%types(bcid%front), samr%ghost_cells(3), samr%levels(0)%boxes(1) )
    end if

    succ = .true.

  contains

    subroutine set_left_bc ( t, d, b )
      implicit none

      integer, intent(in) :: t, d
      type(samr_box_t) :: b

      integer :: i, d_ref

      select case ( t )
      case ( bcid%reflective )
        do i = 1, d
          d_ref = i - int( 2.d0 * (real(i, kind=8) - .5d0) )
          b%hydro(d_ref,:,:) = b%hydro(i,:,:)
          b%hydro(d_ref,:,:)%u(hyid%rho_u) = -b%hydro(d_ref,:,:)%u(hyid%rho_u)
        end do

      case ( bcid%outflow )
        do i = 1, d
          d_ref = i - int( 2.d0 * (real(i, kind=8) - .5d0) )
          b%hydro(d_ref,:,:) = b%hydro(i,:,:)
        end do

      case ( bcid%periodic )
        b%hydro(-d+1:0,:,:) = b%hydro(b%dims(1)-d+1:b%dims(1),:,:)
      end select

    end subroutine set_left_bc


    subroutine set_right_bc ( t, d, b )
      implicit none

      integer, intent(in) :: t, d
      type(samr_box_t) :: b

      integer :: i, d_ref

      select case ( t )
      case ( bcid%reflective )
        do i = b%dims(1) - d + 1, b%dims(1)
          d_ref = i + int( 2.d0 * ( (real(b%dims(1)) + .5d0) - real(i, kind=8) ) )
          b%hydro(d_ref,:,:) = b%hydro(i,:,:)
          b%hydro(d_ref,:,:)%u(hyid%rho_u) = -b%hydro(d_ref,:,:)%u(hyid%rho_u)
        end do

      case ( bcid%outflow )
        do i = b%dims(1) - d + 1, b%dims(1)
          d_ref = i + int( 2.d0 * ( (real(b%dims(1)) + .5d0) - real(i, kind=8) ) )
          b%hydro(d_ref,:,:) = b%hydro(i,:,:)
        end do

      case ( bcid%periodic )
        b%hydro(b%dims(1)+1:b%dims(1)+d,:,:) = b%hydro(1:d,:,:)
      end select

    end subroutine set_right_bc


    subroutine set_bottom_bc ( t, d, b )
      implicit none

      integer, intent(in) :: t, d
      type(samr_box_t) :: b

      integer :: i, d_ref

      select case ( t )
      case ( bcid%reflective )
        do i = 1, d
          d_ref = i - int( 2.d0 * (real(i, kind=8) - .5d0) )
          b%hydro(:,d_ref,:) = b%hydro(:, i,:)
          b%hydro(:,d_ref,:)%u(hyid%rho_v) = -b%hydro(:,d_ref,:)%u(hyid%rho_v)
        end do

      case ( bcid%outflow )
        do i = 1, d
          d_ref = i - int( 2.d0 * (real(i, kind=8) - .5d0) )
          b%hydro(:,d_ref,:) = b%hydro(:,i,:)
        end do

      case ( bcid%periodic )
        b%hydro(:,-d+1:0,:) = b%hydro(:,b%dims(2)-d+1:b%dims(2),:)
      end select

    end subroutine set_bottom_bc


    subroutine set_top_bc ( t, d, b )
      implicit none

      integer, intent(in) :: t, d
      type(samr_box_t) :: b

      integer :: i, d_ref

      select case ( t )
      case ( bcid%reflective )
        do i = b%dims(2) - d + 1, b%dims(2)
          d_ref = i + int( 2.d0 * ( (real(b%dims(2)) + .5d0) - real(i, kind=8) ) )
          b%hydro(:,d_ref,:) = b%hydro(:,i,:)
          b%hydro(:,d_ref,:)%u(hyid%rho_v) = -b%hydro(:,d_ref,:)%u(hyid%rho_v)
        end do

      case ( bcid%outflow )
        do i = b%dims(2) - d + 1, b%dims(2)
          d_ref = i + int( 2.d0 * ( (real(b%dims(2)) + .5d0) - real(i, kind=8) ) )
          b%hydro(:,d_ref,:) = b%hydro(:,i,:)
        end do

      case ( bcid%periodic )
        b%hydro(:,b%dims(2)+1:b%dims(2)+d,:) = b%hydro(:,1:d,:)
      end select

    end subroutine set_top_bc


    subroutine set_back_bc ( t, d, b )
      implicit none

      integer, intent(in) :: t, d
      type(samr_box_t) :: b

      integer :: i, d_ref

      select case ( t )
      case ( bcid%reflective )
        do i = 1, d
          d_ref = i - int( 2.d0 * (real(i, kind=8) - .5d0) )
          b%hydro(:,:,d_ref) = b%hydro(:,:, i)
          b%hydro(:,:,d_ref)%u(hyid%rho_w) = -b%hydro(:,:,d_ref)%u(hyid%rho_w)
        end do

      case ( bcid%outflow )
        do i = 1, d
          d_ref = i - int( 2.d0 * (real(i, kind=8) - .5d0) )
          b%hydro(:,:,d_ref) = b%hydro(:,:,i)
        end do

      case ( bcid%periodic )
        b%hydro(:,:,-d+1:0) = b%hydro(:,:,b%dims(3)-d+1:b%dims(3))
      end select

    end subroutine set_back_bc


    subroutine set_front_bc ( t, d, b )
      implicit none

      integer, intent(in) :: t, d
      type(samr_box_t) :: b

      integer :: i, d_ref

      select case ( t )
      case ( bcid%reflective )
        do i = b%dims(3) - d + 1, b%dims(3)
          d_ref = i + int( 2.d0 * ( (real(b%dims(3)) + .5d0) - real(i, kind=8) ) )
          b%hydro(:,:,d_ref) = b%hydro(:,:,i)
          b%hydro(:,:,d_ref)%u(hyid%rho_w) = -b%hydro(:,:,d_ref)%u(hyid%rho_w)
        end do

      case ( bcid%outflow )
        do i = b%dims(3) - d + 1, b%dims(3)
          d_ref = i + int( 2.d0 * ( (real(b%dims(3)) + .5d0) - real(i, kind=8) ) )
          b%hydro(:,:,d_ref) = b%hydro(:,:,i)
        end do

      case ( bcid%periodic )
        b%hydro(:,:,b%dims(3)+1:b%dims(3)+d) = b%hydro(:,:,1:d)
      end select

    end subroutine set_front_bc

  end function set_samr_bc
end module rhyme_samr_bc
