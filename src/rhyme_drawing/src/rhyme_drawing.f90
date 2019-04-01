module rhyme_drawing
  use rhyme_samr
  use rhyme_hydro_base
  use rhyme_ideal_gas

  implicit none

  type drawing_indices_t
    integer :: grad_x = -1, grad_y = -2, grad_z = -3, grad_r = -4 ! Filling types
    integer :: uniform = 0
    integer :: linear = 1, cubic = 2 ! Gradient/transition types
    integer :: rect = 10, circle = 11, triangle = 12 ! Shapes
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
      call ig%prim_to_cons ( this%canvas, bg )

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
      else if ( shape%type .eq. drid%triangle ) then
        if ( shape%fill%type .eq. drid%uniform) then

          do k = 1, samr%base_grid(3)
            do j = 1, samr%base_grid(2)
              do i = 1, samr%base_grid(1)
                if ( is_inside_triangle( [i, j, k], shape ) ) then
                  samr%levels(0)%boxes(1)%hydro(i,j,k)%u = h(1)%u
                end if
              end do
            end do
          end do

        end if
      end if

      shape => shape%next
    end do
  contains
    logical function is_inside_triangle ( point, trian ) result ( is_inside )
      implicit none

      integer, intent ( in ) :: point(3)
      type ( shape_t ), intent ( in ) :: trian

      real ( kind=8 ) :: p0(3), vec_p0(3), len_vec_p0
      real ( kind=8 ) :: proj_p(3), vec_proj_p(3), len_vec_proj_p
      real ( kind=8 ) :: a(3), b(3), n(3), len_n
      real ( kind=8 ) :: p_1(3), len_p_1, p_2(3), len_p_2, p_3(3), len_p_3
      real ( kind=8 ) :: theta, dis
      real ( kind=8 ), parameter :: pi = 3.1415926535897932_8

      p0 = real( point, kind=8 )
      vec_p0 = p0 - trian%vertices(1, :)
      len_vec_p0 = sqrt( sum( vec_p0**2 ) )

      a = trian%vertices(2, :) - trian%vertices(1, :)
      b = trian%vertices(3, :) - trian%vertices(1, :)

      n(1) = a(2) * b(3) - a(3) * b(2)
      n(2) = a(3) * b(1) - a(1) * b(3)
      n(3) = a(1) * b(2) - a(2) * b(1)
      len_n = sqrt( sum( n**2 ) )

      theta = acos( sum( n * vec_p0 ) / ( len_n * len_vec_p0 ) )
      dis = len_vec_p0 * sin( theta - pi / 2 )

      if ( dis > trian%thickness / 2 ) then
        is_inside = .false.
        return
      end if

      proj_p = p0 - dis * n / len_n
      vec_proj_p = proj_p - trian%vertices(1, :)
      len_vec_proj_p = sqrt( sum( vec_proj_p**2 ) )

      theta = 0
      p_1 = proj_p - trian%vertices(1, :)
      len_p_1 = sqrt( sum( p_1**2 ) )
      p_2 = proj_p - trian%vertices(2, :)
      len_p_2 = sqrt( sum( p_2**2 ) )
      p_3 = proj_p - trian%vertices(3, :)
      len_p_3 = sqrt( sum( p_3**2 ) )

      theta = theta + acos( sum( p_1 * p_2 ) / ( len_p_1 * len_p_2 ) )
      theta = theta + acos( sum( p_1 * p_3 ) / ( len_p_1 * len_p_3 ) )
      theta = theta + acos( sum( p_2 * p_3 ) / ( len_p_2 * len_p_3 ) )

      if ( abs( theta - 2 * pi ) > epsilon(0.e0) ) then
        is_inside = .false.
      else
        is_inside = .true.
      end if
    end function is_inside_triangle
  end subroutine rhyme_drawing_apply
end module rhyme_drawing
