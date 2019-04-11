logical function rhyme_samr_init_box_test () result ( failed )
  use rhyme_samr_factory

  implicit none

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


  call rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr &
  )

  failed = .false.

  do l = 0, samr%nlevels - 1
    do b = 1, samr%levels(l)%max_nboxes
      box_dims = floor( base_grid / real( max_nboxes(l) ) )
      ledge = (b - 1) * box_dims + 1
      redge = ledge + box_dims

      call samr%init_box( l, b, box_dims, ledge, redge )

      failed = &
      samr%levels(l)%boxes(b)%level .ne. l &
      .or. samr%levels(l)%boxes(b)%number .ne. b &
      .or. .not. allocated( samr%levels(l)%boxes(b)%hydro ) &
      .or. any( lbound( samr%levels(l)%boxes(b)%hydro ) .ne. - ghost_cells + 1 ) &
      .or. any( ubound( samr%levels(l)%boxes(b)%hydro ) .ne. box_dims + ghost_cells ) &
      .or. .not. allocated ( samr%levels(l)%boxes(b)%flags ) &
      .or. any( lbound( samr%levels(l)%boxes(b)%flags ) .ne. - ghost_cells + 1 ) &
      .or. any( ubound( samr%levels(l)%boxes(b)%flags ) .ne. box_dims + ghost_cells ) &
      .or. samr%levels(l)%nboxes .ne. b &
      .or. any( samr%levels(l)%boxes(b)%left_edge .ne. ledge ) &
      .or. any( samr%levels(l)%boxes(b)%right_edge .ne. redge )
      if ( failed ) return
    end do
  end do
end function rhyme_samr_init_box_test
