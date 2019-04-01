submodule ( rhyme_drawing ) rhyme_drawing_uniform_sphere_submodule
contains
  pure module subroutine rhyme_drawing_uniform_sphere ( samr, ig, sphere )
    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( shape_t ), intent ( in ) :: sphere

    type ( hydro_conserved_t ) :: color
    integer :: l, b, k, j, i
    integer :: shift(3), lb(3), ub(3)

    call ig%prim_to_cons ( sphere%fill%states(1), color )

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        shift = samr%levels(l)%boxes(b)%left_edge

        lb = ( int( sphere%x0 - sphere%r ) - 1 ) * 2**l - shift
        lb = merge( 1, lb, lb < 1 )
        lb = merge( samr%levels(l)%boxes(b)%dims + 1, lb, lb > samr%levels(l)%boxes(b)%dims )

        ub = ( int( sphere%x0 + sphere%r ) + 1 ) * 2**l - shift
        ub = merge( 0, ub, ub < 1 )
        ub = merge( samr%levels(l)%boxes(b)%dims, ub, ub > samr%levels(l)%boxes(b)%dims )

        do k = lb(3), ub(3)
          do j = lb(2), ub(2)
            do i = lb(1), ub(1)

              if ( is_inside_sphere( [i, j, k], samr%levels(l)%boxes(b), sphere ) ) then
                samr%levels(0)%boxes(1)%hydro(i,j,k)%u = color%u
              end if

            end do
          end do
        end do
      end do
    end do

  contains
    pure logical function is_inside_sphere ( point, box, sphere ) result ( is_inside )
      implicit none

      integer, intent ( in ) :: point(3)
      type ( samr_box_t ), intent ( in ) :: box
      type ( shape_t ), intent ( in ) :: sphere

      real ( kind=8 ) :: p0(3), r2

      p0 = real( point, kind=8 ) / 2**box%level

      r2 = sphere%r**2

      if ( sum( (p0 - sphere%x0)**2 ) > r2 ) then
        is_inside = .false.
      else
        is_inside = .true.
      end if
    end function is_inside_sphere
  end subroutine rhyme_drawing_uniform_sphere
end submodule rhyme_drawing_uniform_sphere_submodule
