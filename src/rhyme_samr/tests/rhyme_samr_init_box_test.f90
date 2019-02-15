logical function rhyme_samr_init_box_test () result ( failed )
  use rhyme_samr

  implicit none

  real ( kind=8 ) :: ledges(3) = [ 0.d0, 0.d0, 0.d0 ]
  real ( kind=8 ) :: redges(3) = [ 1.d0, 1.d0, 1.d0 ]

  integer, parameter :: nlevels = 4
  integer, parameter :: base_grid(3) = [ 16, 8, 4 ]
  integer, parameter :: ghost_cells(3) = [ 2, 1, 0 ]
  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  integer, parameter :: l = nlevels - 1
  integer :: b, nboxes_before
  integer, parameter :: dims(3) = [ 5, 7, 11 ]

  type ( samr_t ) :: samr

  call samr%init_with ( base_grid, nlevels, max_nboxes, ghost_cells )

  nboxes_before = samr%levels(l)%nboxes
  failed = nboxes_before .ne. 0
  if ( failed ) return

  b = max_nboxes(l)
  call samr%init_box ( l, b, dims, ledges, redges )

  failed = &
  .not. allocated ( samr%levels(l)%boxes(b)%hydro ) &
  .or. any ( lbound(samr%levels(l)%boxes(b)%hydro) .ne. - ghost_cells + 1 ) &
  .or. any ( ubound(samr%levels(l)%boxes(b)%hydro) .ne. dims + ghost_cells ) &
  .or. .not. allocated ( samr%levels(l)%boxes(b)%flags ) &
  .or. any ( lbound(samr%levels(l)%boxes(b)%flags) .ne. - ghost_cells + 1 ) &
  .or. any ( ubound(samr%levels(l)%boxes(b)%flags) .ne. dims + ghost_cells ) &
  .or. samr%levels(l)%nboxes .ne. nboxes_before + 1

end function rhyme_samr_init_box_test
