logical function rhyme_samr_init_with_test () result (failed)
  use rhyme_samr

  implicit none

  type ( samr_t ) :: samr

  integer :: max_nboxes(0:samrid%max_nlevels)
  integer, parameter :: base_grid(3) = [ 32, 16, 8 ]
  integer, parameter :: nlevels = 4
  integer, parameter :: ghost_cells(3) = [2, 1, 0]

  max_nboxes(0:3) = [ 1, 10, 100, 1000 ]

  call samr%init_with ( base_grid, nlevels, max_nboxes, ghost_cells )

  failed = &
  .not. samr%initialized &
  .or. samr%nlevels .ne. nlevels &
  .or. any ( samr%max_nboxes .ne. max_nboxes ) &
  .or. any ( samr%base_grid .ne. base_grid ) &
  .or. any ( samr%ghost_cells .ne. ghost_cells ) &
  .or. any ( samr%levels(0)%boxes(1)%dims .ne. base_grid ) &
  .or. any ( .not. allocated ( samr%levels(0:nlevels-1)%boxes ) ) &
  .or. .not. allocated ( samr%levels(0)%boxes(1)%flags ) &
  .or. .not. allocated ( samr%levels(0)%boxes(1)%hydro ) &
  .or. any ( abs(samr%levels(0)%boxes(1)%left_edge - 0.d0) > epsilon(0.d0) ) &
  .or. any ( abs(samr%levels(0)%boxes(1)%right_edge - 1.0) > epsilon(0.d0) ) &
  .or. any ( abs ( samr%levels(0)%dx - 1.d0 / base_grid ) > epsilon(0.d0) ) &
  .or. abs (samr%levels(0)%refine_factor - 2.d0) > epsilon(0.d0) &
  .or. size ( samr%levels(0)%boxes(1)%flags ) .ne. product ( base_grid + (2 * ghost_cells) ) &
  .or. size ( samr%levels(0)%boxes(1)%hydro ) .ne. product ( base_grid + (2 * ghost_cells) )
end function rhyme_samr_init_with_test
