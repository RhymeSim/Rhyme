logical function rhyme_drawing_uniform_triangle_test () result ( failed )
  use rhyme_drawing_factory

  implicit none

  type ( drawing_t ) :: draw
  type ( shape_t ), pointer :: shape
  type ( samr_t ) :: samr

  integer :: l, b, k, j, i

  integer, parameter :: nlevels = 4
  integer, parameter :: base_grid(3) = [ 16, 16, 16 ]
  integer, parameter :: ghost_cells(3) = [ 2, 2, 2 ]
  integer, parameter :: max_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: init_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 2, 4, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  real ( kind=8 ) :: vertices(3,3)
  real ( kind=8 ), parameter :: thickness = 2

  vertices = reshape( [ 1, 1, 1, 5, 12, 4, 13, 8, 10 ], [3, 3], order=[2, 1] )

  call rhyme_drawing_factory_init

  call rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

  draw%type = drid%transparent_bg

  shape => draw%new_shape( drid%triangle )

  shape%vertices = vertices
  shape%thickness = thickness
  shape%fill%type = drid%uniform
  shape%fill%states(1)%w = hy%prim%w

  call rhyme_drawing_uniform_triangle( samr, draw_fac_ig_mon, shape )

  do l = 0, samr%nlevels - 1
    do b = 1, samr%levels(l)%nboxes
      do k = 1, samr%levels(l)%boxes(b)%dims(3)
        do j = 1, samr%levels(l)%boxes(b)%dims(2)
          do i = 1, samr%levels(l)%boxes(b)%dims(1)
            if ( is_inside_triangle( [i, j, k], samr%levels(l)%boxes(b), shape ) ) then

              failed = any( abs( &
                samr%levels(l)%boxes(b)%hydro(i,j,k)%u - hy%cons%u &
              ) > epsilon(0.d0) )
              if ( failed ) then
                print *, l, b, i, j, k
                return
              end if

            end if
          end do
        end do
      end do
    end do
  end do

contains

  pure logical function is_inside_triangle ( point, box, trian ) result ( is_inside )
    implicit none

    integer, intent ( in ) :: point(3)
    type ( samr_box_t ), intent ( in ) :: box
    type ( shape_t ), intent ( in ) :: trian

    real ( kind=8 ), parameter :: pi = 3.1415926535897932_8

    real ( kind=8 ) :: p0(3), vec_p0(3), len_vec_p0
    real ( kind=8 ) :: proj_p(3)
    real ( kind=8 ) :: a(3), b(3), n(3), len_n
    real ( kind=8 ) :: p_1(3), len_p_1, p_2(3), len_p_2, p_3(3), len_p_3
    real ( kind=8 ) :: theta, dis

    p0 = real( point, kind=8 ) / 2**box%level
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

end function rhyme_drawing_uniform_triangle_test
