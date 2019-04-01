module rhyme_drawing
  use rhyme_samr
  use rhyme_hydro_base
  use rhyme_ideal_gas

  implicit none

  type drawing_indices_t
    integer :: grad_x = -1, grad_y = -2, grad_z = -3, grad_r = -4 ! Filling types
    integer :: uniform = 0
    integer :: linear = 1, cubic = 2 ! Gradient/transition types
    integer :: rect = 10, sphere = 11, triangle = 12 ! Shapes
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
    real ( kind=8 ) :: x0(3) = 0.d0, r = 0.d0 ! Circle (in pixel unit)
    real ( kind=8 ) :: vertices( 3, 3 ), thickness ! Triangle (in pixel unit)
    type ( shape_filling_t ) :: fill
    type ( shape_transition_t ) :: trans
    type ( shape_t ), pointer :: next => null()
  end type shape_t


  type drawing_t
    integer :: type = drid%uniform_bg
    type ( hydro_primitive_t ) :: canvas
    type ( shape_t ), pointer :: shapes => null()
    logical :: initialized
  contains
    procedure :: new_shape => rhyme_drawing_new_shape
    procedure :: apply => rhyme_drawing_apply
  end type drawing_t

  interface
    pure module subroutine rhyme_drawing_uniform_bg ( samr, ig, bg_prim )
      type ( samr_t ), intent ( inout ) :: samr
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( hydro_primitive_t ), intent ( in ) :: bg_prim
    end subroutine rhyme_drawing_uniform_bg

    pure module subroutine rhyme_drawing_uniform_rectangle ( samr, ig, rect )
      type ( samr_t ), intent ( inout ) :: samr
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( shape_t ), intent ( in ) :: rect
    end subroutine rhyme_drawing_uniform_rectangle

    pure module subroutine rhyme_drawing_uniform_sphere ( samr, ig, sphere )
      type ( samr_t ), intent ( inout ) :: samr
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( shape_t ), intent ( in ) :: sphere
    end subroutine rhyme_drawing_uniform_sphere

    module subroutine rhyme_drawing_uniform_triangle ( samr, ig, tri )
      type ( samr_t ), intent ( inout ) :: samr
      type ( ideal_gas_t ), intent ( in ) :: ig
      type ( shape_t ), intent ( in ) :: tri
    end subroutine rhyme_drawing_uniform_triangle
  end interface

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

    type ( shape_t ), pointer :: shape


    ! No action for transparent
    if ( this%type .eq. drid%uniform_bg ) then
      call rhyme_drawing_uniform_bg( samr, ig, this%canvas )
    end if

    shape => this%shapes

    do while ( associated(shape) )
      select case ( shape%type )
      case ( drid%rect )
        if ( shape%fill%type .eq. drid%uniform) then
          call rhyme_drawing_uniform_rectangle( samr, ig, shape )
        end if

      case ( drid%sphere )
        if ( shape%fill%type .eq. drid%uniform ) then
          call rhyme_drawing_uniform_sphere( samr, ig, shape )
        end if

      case ( drid%triangle )
        if ( shape%fill%type .eq. drid%uniform) then
          call rhyme_drawing_uniform_triangle( samr, ig, shape )
        end if

      end select

      shape => shape%next
    end do

  end subroutine rhyme_drawing_apply
end module rhyme_drawing
