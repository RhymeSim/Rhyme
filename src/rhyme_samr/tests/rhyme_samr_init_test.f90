logical function rhyme_samr_init_test () result (failed)
  use rhyme_samr

  implicit none

  type ( samr_t ) :: samr, samr_1d, samr_uniform

  integer :: tot_nboxes(0:23) = 0
  integer, parameter :: nlevels = 4
  integer, parameter :: base_grid(3) = [ 32, 16, 8 ]
  integer, parameter :: base_grid_1d(3) = [ 32, 1, 1 ]
  integer, parameter :: ghost_cells(3) = [ 2, 1, 0 ]
  integer, parameter :: ghost_cells_1d(3) = [ 2, 0, 0 ]

  tot_nboxes(0:3) = [ 3, 30, 300, 3000 ]

  samr%base_grid = base_grid
  samr%nlevels = nlevels
  samr%tot_nboxes(0:3) = [ 3, 30, 300, 3000 ]
  samr%ghost_cells = ghost_cells

  call samr%init

  failed = &
  samr%nlevels .ne. nlevels &
  .or. .not. samr%initialized &
  .or. any ( samr%tot_nboxes .ne. tot_nboxes ) &
  .or. any ( samr%base_grid .ne. base_grid ) &
  .or. any ( samr%ghost_cells .ne. ghost_cells ) &
  .or. any ( samr%levels(0)%boxes(1)%dims .ne. base_grid ) &
  .or. size ( samr%levels ) .ne. nlevels &
  .or. size ( samr%levels(0)%boxes ) .ne. tot_nboxes(0) &
  .or. size ( samr%levels(0)%boxes(1)%flags ) .ne. product ( base_grid + (2 * ghost_cells) ) &
  .or. size ( samr%levels(0)%boxes(1)%hydro ) .ne. product ( base_grid + (2 * ghost_cells) ) &
  .or. any ( abs(samr%levels(0)%boxes(1)%left_edge - 0.d0) > epsilon(0.d0) ) &
  .or. any ( abs(samr%levels(0)%boxes(1)%right_edge - 1.d0) > epsilon(0.d0) ) &
  .or. any ( abs ( samr%levels(0)%dx - 1.d0 / base_grid ) > epsilon(0.d0) ) &
  .or. .not. ( abs (samr%levels(0)%refine_factor - 1.d0) < epsilon(0.d0) )

  if ( failed ) return

  samr_1d%base_grid = base_grid_1d
  samr_1d%nlevels = 4
  samr_1d%tot_nboxes(0:3) = [ 1, 10, 100, 1000 ]
  samr_1d%ghost_cells = [ 2, 0, 0 ]

  call samr_1d%init

  failed = &
  any ( abs ( samr_1d%levels(1)%dx - [ 1.d0 / ( 2.d0 * base_grid_1d(1)) , 1.d0, 1.d0] ) > epsilon(0.d0) ) &
  .or. any ( abs ( samr_1d%levels(2)%dx - [ 1.d0 / ( 4.d0 * base_grid_1d(1)) , 1.d0, 1.d0] ) > epsilon(0.d0) ) &
  .or. any ( abs ( samr_1d%levels(3)%dx - [ 1.d0 / ( 8.d0 * base_grid_1d(1)) , 1.d0, 1.d0] ) > epsilon(0.d0) )

  if ( failed ) return

  samr_uniform%base_grid = base_grid_1d
  samr_uniform%nlevels = 1
  samr_uniform%tot_nboxes(0) = 1
  samr_uniform%ghost_cells = ghost_cells_1d

  call samr_uniform%init

  failed = &
  samr_uniform%tot_nboxes(0) .ne. 1 &
  .or. any ( samr_uniform%tot_nboxes(1:) .ne. 0 ) &
  .or. size ( samr_uniform%levels ) .ne. 1 &
  .or. size ( samr_uniform%levels(0)%boxes ) .ne. 1 &
  .or. samr_uniform%levels(0)%nboxes .ne. 1 &
  .or. samr_uniform%levels(0)%tot_nboxes .ne. 1

end function rhyme_samr_init_test
