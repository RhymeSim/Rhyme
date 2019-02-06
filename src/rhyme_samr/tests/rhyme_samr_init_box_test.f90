logical function rhyme_samr_init_box_test () result ( failed )
  use rhyme_samr_factory

  implicit none

  real ( kind=8 ) :: ledges(3) = [ 0.d0, 0.d0, 0.d0 ]
  real ( kind=8 ) :: redges(3) = [ 1.d0, 1.d0, 1.d0 ]

  integer, parameter :: l = nlevels - 1
  integer, parameter :: dims(3) = [ 5, 7, 11 ]
  integer :: b

  integer :: nboxes_before


  call rhyme_samr_factory_init

  nboxes_before = samr%levels(l)%nboxes

  failed = nboxes_before .ne. 0
  if ( failed ) return

  b = nboxes(l)
  call samr%init_box ( l, b, dims, ledges, redges )

  failed = &
  .not. allocated ( samr%levels(l)%boxes(b)%hydro ) &
  .or. any ( lbound(samr%levels(l)%boxes(b)%hydro) .ne. - ghost_cells ) &
  .or. any ( ubound(samr%levels(l)%boxes(b)%hydro) .ne. dims + ghost_cells ) &
  .or. .not. allocated ( samr%levels(l)%boxes(b)%flags ) &
  .or. any ( lbound(samr%levels(l)%boxes(b)%flags) .ne. - ghost_cells ) &
  .or. any ( ubound(samr%levels(l)%boxes(b)%flags) .ne. dims + ghost_cells ) &
  .or. samr%levels(l)%nboxes .ne. nboxes_before + 1

  print *, &
  .not. allocated ( samr%levels(l)%boxes(b)%hydro ) &
  , any ( lbound(samr%levels(l)%boxes(b)%hydro) .ne. - ghost_cells ) &
  , any ( ubound(samr%levels(l)%boxes(b)%hydro) .ne. dims + ghost_cells ) &
  , .not. allocated ( samr%levels(l)%boxes(b)%flags ) &
  , any ( lbound(samr%levels(l)%boxes(b)%flags) .ne. - ghost_cells ) &
  , any ( ubound(samr%levels(l)%boxes(b)%flags) .ne. dims + ghost_cells ) &
  , samr%levels(l)%nboxes .ne. nboxes_before + 1

  print *, lbound(samr%levels(l)%boxes(b)%hydro), - ghost_cells
  print *, ubound(samr%levels(l)%boxes(b)%hydro), dims + ghost_cells

end function rhyme_samr_init_box_test
