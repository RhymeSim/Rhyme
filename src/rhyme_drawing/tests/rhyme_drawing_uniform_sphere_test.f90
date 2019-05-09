logical function rhyme_drawing_uniform_sphere_test () result ( failed )
  use rhyme_drawing
  use rhyme_ideal_gas_factory
  use rhyme_samr_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: dr_tester

  type ( drawing_t ) :: draw
  type ( shape_t ), pointer :: shape
  type ( samr_t ) :: samr
  type ( ideal_gas_t ) :: ig
  type ( hydro_primitive_t ) :: prim
  type ( hydro_conserved_t ) :: cons

  integer :: l, b, k, j, i
  real ( kind=8 ), parameter :: origin(3) = [ 8.d0, 8.d0, 8.d0 ]
  real ( kind=8 ), parameter :: r = 4.d0

  dr_tester = .describe. "drawing uniform_sphere"

  samr = samr_factory%generate()
  ig = ig_factory%generate()
  prim = hy_factory%primitive()
  cons = hy_factory%conserved()

  draw%type = drid%transparent_canvas

  shape => draw%new_shape( drid%sphere )

  shape%sphere%origin = origin
  shape%sphere%r = r
  shape%fill%type = drid%uniform
  shape%fill%colors(1)%w = prim%w


  call rhyme_drawing_uniform_sphere( samr, ig, shape )


  do l = 0, samr%nlevels - 1
    do b = 1, samr%levels(l)%nboxes
      do k = 1, samr%levels(l)%boxes(b)%dims(3)
        do j = 1, samr%levels(l)%boxes(b)%dims(2)
          do i = 1, samr%levels(l)%boxes(b)%dims(1)
            if ( is_inside_sphere( [i, j, k], samr%levels(l)%boxes(b), shape ) ) then
              call dr_tester%expect( samr%levels(l)%boxes(b)%hydro(i,j,k)%u .toBe. cons%u )
            end if
          end do
        end do
      end do
    end do
  end do

  failed = dr_tester%failed()

contains
  logical function is_inside_sphere ( p0, box, shape ) result ( is_inside )
    implicit none

    integer, intent ( in ) :: p0(3)
    type ( samr_box_t ), intent ( in ) :: box
    type ( shape_t ), intent ( in ) :: shape

    real ( kind=8 ) :: p(3), r2

    p = real( p0, kind=8 ) / 2**box%level
    r2 = shape%sphere%r**2

    if ( sum( ( p - shape%sphere%origin )**2 ) > r2 ) then
      is_inside = .false.
    else
      is_inside = .true.
    end if
  end function is_inside_sphere
end function rhyme_drawing_uniform_sphere_test
