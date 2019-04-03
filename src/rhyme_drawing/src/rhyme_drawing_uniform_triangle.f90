submodule ( rhyme_drawing ) rhyme_drawing_uniform_triangle_submodule
contains
  module subroutine rhyme_drawing_uniform_triangle ( samr, ig, tri )
    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( shape_t ), intent ( in ) :: tri

    type ( hydro_conserved_t ) :: color
    integer :: l, b, k, j, i, ax
    integer :: shift(3), lb(3), ub(3)

    call ig%prim_to_cons ( tri%fill%colors(1), color )

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        shift = samr%levels(l)%boxes(b)%left_edge

        do ax = 1, 3
          lb(ax) = int( minval( tri%vertices(:,ax) ) * 2**l - tri%thickness - shift(ax) ) - 1
          ub(ax) = int( maxval( tri%vertices(:,ax) ) * 2**l + tri%thickness - shift(ax) ) + 1
        end do

        lb = merge( 1, lb, lb < 1 )
        lb = merge( samr%levels(l)%boxes(b)%dims + 1, lb, lb > samr%levels(l)%boxes(b)%dims )

        ub = merge( 0, ub, ub < 1 )
        ub = merge( samr%levels(l)%boxes(b)%dims, ub, ub > samr%levels(l)%boxes(b)%dims )

        do k = lb(3), ub(3)
          do j = lb(2), ub(2)
            do i = lb(1), ub(1)

              if ( is_inside_triangle( [i, j, k], samr%levels(l)%boxes(b), tri ) ) then
                samr%levels(l)%boxes(b)%hydro(i,j,k)%u = color%u
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

  end subroutine rhyme_drawing_uniform_triangle
end submodule rhyme_drawing_uniform_triangle_submodule
