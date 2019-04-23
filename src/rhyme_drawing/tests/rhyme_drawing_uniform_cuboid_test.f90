logical function rhyme_drawing_uniform_cuboid_test () result (failed)
  use rhyme_drawing_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: dr_tester

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

  integer, parameter :: left_corner(3) = [ 3, 3, 3 ]
  integer, parameter :: lengths = 12

  dr_tester = .describe. "drawing uniform_canvas"

  call rhyme_drawing_factory_init

  call rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

  draw%type = drid%transparent_canvas

  shape => draw%new_shape( drid%cuboid )

  shape%cuboid%left_corner = left_corner
  shape%cuboid%lengths = lengths
  shape%fill%type = drid%uniform
  shape%fill%colors(1)%w = hy%prim%w

  call rhyme_drawing_uniform_cuboid( samr, draw_fac_ig_mon, shape )

  do l = 0, samr%nlevels - 1
    do b = 1, samr%levels(l)%nboxes
      do k = 1, samr%levels(l)%boxes(b)%dims(3)
        do j = 1, samr%levels(l)%boxes(b)%dims(2)
          do i = 1, samr%levels(l)%boxes(b)%dims(1)
            if ( is_inside_cuboid( [i, j, k], samr%levels(l)%boxes(b), shape ) ) then
              call dr_tester%expect( samr%levels(l)%boxes(b)%hydro(i,j,k)%u .toBe. hy%cons%u )
            end if
          end do
        end do
      end do
    end do
  end do

  failed = dr_tester%failed()

contains

  logical function is_inside_cuboid ( p0, box, shape ) result ( is_inside )
    implicit none

    integer, intent ( in ) :: p0(3)
    type ( samr_box_t ), intent ( in ) :: box
    type ( shape_t ), intent ( in ) :: shape

    integer :: le(3), re(3), p(3)

    p = p0 / 2**box%level

    le = shape%cuboid%left_corner
    re = shape%cuboid%left_corner + shape%cuboid%lengths - 1

    if ( any( p < le ) .or. any( p >  re ) ) then
      is_inside = .false.
    else
      is_inside = .true.
    end if

  end function is_inside_cuboid
end function rhyme_drawing_uniform_cuboid_test
