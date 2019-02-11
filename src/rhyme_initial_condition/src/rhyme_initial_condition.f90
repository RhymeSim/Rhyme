module rhyme_initial_condition
  use rhyme_samr
  use rhyme_samr_bc
  use rhyme_hydro_base
  use rhyme_ideal_gas

  implicit none

  type ic_indices_t
    integer :: rect = 10, circle = 11 ! Shapes
    integer :: linear = 1, cubic = 2 ! Gradient/transition types
    integer :: uniform = 0, grad_x = -1, grad_y = -2, grad_z = -3, grad_r = -4 ! Filling types
    integer :: unset = -1234 ! Unset
  end type ic_indices_t

  type ( ic_indices_t ), parameter :: icid = ic_indices_t ()


  type ic_transition_t
    integer :: type
    real(kind=8) :: width_px
  end type ic_transition_t


  type ic_filling_t
    integer :: type
    type ( hydro_primitive_t ) :: states(2)
  end type ic_filling_t


  type ic_shape_t
    integer :: type ! Shape
    integer :: xl(3) = 0, length(3) = 0 ! Rectangle (in pixel unit)
    real(kind=8) :: x0(3) = 0.d0, r = 0.d0 ! Circle (in pixel unit)
    type ( ic_filling_t ) :: fill
    type ( ic_transition_t ) :: trans
    type ( ic_shape_t ), pointer :: next => null()
  end type ic_shape_t


  type initial_condition_t
    type ( hydro_primitive_t ) :: background
    type ( ic_shape_t ), pointer :: shapes => null()
    logical :: initialized
  contains
    procedure :: new_shape => ic_new_shape
    procedure :: apply => ic_apply
  end type initial_condition_t

contains


  function ic_new_shape ( this, shape_type ) result ( shape )
    implicit none

    class ( initial_condition_t ), intent(inout) :: this
    integer, intent(in) :: shape_type

    type ( ic_shape_t ), pointer :: shape

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
    shape%fill%type = icid%unset
    shape%trans%type = icid%unset
  end function ic_new_shape


  subroutine ic_apply ( this, ig, samr, bc )
    implicit none

    class ( initial_condition_t ), intent(in) :: this
    type ( ideal_gas_t ), intent(in) :: ig
    type ( samr_t ), intent(inout) :: samr
    type ( samr_bc_t ), intent(inout) :: bc

    integer :: i, j, k
    type ( hydro_conserved_t ) :: bg, h(2)
    type ( ic_shape_t ), pointer :: shape

    call ig%prim_to_cons ( this%background, bg )

    do k = 1, samr%levels(0)%boxes(1)%dims(3)
      do j = 1, samr%levels(0)%boxes(1)%dims(2)
        do i = 1, samr%levels(0)%boxes(1)%dims(1)
          samr%levels(0)%boxes(1)%hydro(i,j,k)%u = bg%u
        end do
      end do
    end do

    shape => this%shapes

    do while ( associated(shape) )
      call ig%prim_to_cons ( shape%fill%states(1), h(1))
      call ig%prim_to_cons ( shape%fill%states(2), h(2))

      if ( shape%type .eq. icid%rect ) then
        if ( shape%fill%type .eq. icid%uniform) then

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
  end subroutine ic_apply
end module rhyme_initial_condition
