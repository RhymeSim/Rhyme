logical function rhyme_samr_init_box_test () result ( failed )
  use rhyme_samr_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: s_tester

  integer, parameter :: nlevels = 4
  integer, parameter :: base_grid(3) = [ 16, 8, 4 ]
  integer, parameter :: ghost_cells(3) = [ 2, 1, 0 ]
  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: init_nboxes ( 0:samrid%max_nlevels ) = [ &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  type ( samr_t ) :: samr
  integer :: l, b, box_dims(3), ledge(3), redge(3)

  s_tester = .describe. "samr_init_box"

  call rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr &
  )

  do l = 0, samr%nlevels - 1
    do b = 1, samr%levels(l)%max_nboxes
      box_dims = floor( base_grid / real( max_nboxes(l) ) )
      ledge = (b - 1) * box_dims + 1
      redge = ledge + box_dims

      call samr%init_box( l, b, box_dims, ledge, redge )

      call s_tester%expect( samr%levels(l)%boxes(b)%level .toBe. l )
      call s_tester%expect( samr%levels(l)%boxes(b)%number .toBe. b )
      call s_tester%expect( allocated( samr%levels(l)%boxes(b)%hydro ) .toBe. .true. )
      call s_tester%expect( lbound( samr%levels(l)%boxes(b)%hydro ) .toBe. - ghost_cells + 1 )
      call s_tester%expect( ubound( samr%levels(l)%boxes(b)%hydro ) .toBe. box_dims + ghost_cells )
      call s_tester%expect( allocated ( samr%levels(l)%boxes(b)%flags ) .toBe. .true. )
      call s_tester%expect( lbound( samr%levels(l)%boxes(b)%flags ) .toBe. - ghost_cells + 1 )
      call s_tester%expect( ubound( samr%levels(l)%boxes(b)%flags ) .toBe. box_dims + ghost_cells )
      call s_tester%expect( samr%levels(l)%nboxes .toBe. b )
      call s_tester%expect( samr%levels(l)%boxes(b)%left_edge .toBe. ledge )
      call s_tester%expect( samr%levels(l)%boxes(b)%right_edge .toBe. redge )
    end do
  end do

  failed = s_tester%failed()
end function rhyme_samr_init_box_test
