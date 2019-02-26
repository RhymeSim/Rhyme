module rhyme_drawing
  use rhyme_samr
  use rhyme_hydro_base
  use rhyme_ideal_gas

  implicit none

  type drawing_indices_t
    integer :: grad_x = -1, grad_y = -2, grad_z = -3, grad_r = -4 ! Filling types
    integer :: uniform = 0
    integer :: linear = 1, cubic = 2 ! Gradient/transition types
    integer :: rect = 10, circle = 11 ! Shapes
    integer :: uniform_bg = 20, transparent_bg = 21
    integer :: unset = -1234 ! Unset
  end type drawing_indices_t

  type ( drawing_indices_t ), parameter :: drid = drawing_indices_t ()


  type shape_transition_t
    integer :: type
    real(kind=8) :: width_px
  end type shape_transition_t


  type shape_filling_t
    integer :: type
    type ( hydro_primitive_t ) :: states(2)
  end type shape_filling_t


  type shape_t
    integer :: type ! Shape
    integer :: xl(3) = 0, length(3) = 0 ! Rectangle (in pixel unit)
    real(kind=8) :: x0(3) = 0.d0, r = 0.d0 ! Circle (in pixel unit)
    type ( shape_filling_t ) :: fill
    type ( shape_transition_t ) :: trans
    type ( shape_t ), pointer :: next => null()
  end type shape_t


  type drawing_t
    integer :: type = drid%uniform_bg
    type ( hydro_primitive_t ) :: background
    type ( shape_t ), pointer :: shapes => null()
    logical :: initialized
  contains
    procedure :: new_shape => rhyme_drawing_new_shape
    procedure :: apply => rhyme_drawing_apply
  end type drawing_t

contains


  function rhyme_drawing_new_shape ( this, shape_type ) result ( shape )
    implicit none

    class ( drawing_t ), intent(inout) :: this
    integer, intent(in) :: shape_type

    type ( shape_t ), pointer :: shape

    shape => this%shapes

    if ( associated ( shape ) ) then
      do while ( associated ( shape%next ) )
        shape => shape%next
      end do

      allocate ( shape%next )
      shape => shape%next
    else
      allocate ( this%shapes )
      shape => this%shapes
    end if

    shape%type = shape_type
    shape%fill%type = drid%unset
    shape%trans%type = drid%unset
  end function rhyme_drawing_new_shape


  subroutine rhyme_drawing_apply ( this, ig, samr )
    implicit none

    class ( drawing_t ), intent(in) :: this
    type ( ideal_gas_t ), intent(in) :: ig
    type ( samr_t ), intent(inout) :: samr

    integer :: i, j, k
    type ( hydro_conserved_t ) :: bg, h(2)
    type ( shape_t ), pointer :: shape


    ! No action for transparent
    if ( this%type .eq. drid%uniform_bg ) then
      call ig%prim_to_cons ( this%background, bg )

      do k = 1, samr%levels(0)%boxes(1)%dims(3)
        do j = 1, samr%levels(0)%boxes(1)%dims(2)
          do i = 1, samr%levels(0)%boxes(1)%dims(1)
            samr%levels(0)%boxes(1)%hydro(i,j,k)%u = bg%u
          end do
        end do
      end do
    end if

    shape => this%shapes

    do while ( associated(shape) )
      call ig%prim_to_cons ( shape%fill%states(1), h(1))
      call ig%prim_to_cons ( shape%fill%states(2), h(2))

      if ( shape%type .eq. drid%rect ) then
        if ( shape%fill%type .eq. drid%uniform) then

          do k = shape%xl(3), shape%xl(3) + shape%length(3) - 1
            do j = shape%xl(2), shape%xl(2) + shape%length(2) - 1
              do i = shape%xl(1), shape%xl(1) + shape%length(1) - 1
                samr%levels(0)%boxes(1)%hydro(i,j,k)%u = h(1)%u
              end do
            end do
          end do

        end if
      end if

      shape => shape%next
    end do
  end subroutine rhyme_drawing_apply
end module rhyme_drawing
