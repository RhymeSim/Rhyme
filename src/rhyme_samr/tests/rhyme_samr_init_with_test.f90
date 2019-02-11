logical function rhyme_samr_init_with_test () result (failed)
  use rhyme_samr

  implicit none

  type ( samr_t ) :: samr

  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 3, 9, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: base_grid(3) = [ 32, 16, 8 ]
  integer, parameter :: nlevels = 4
  integer, parameter :: ghost_cells(3) = [2, 1, 0]

  call samr%init_with ( base_grid, nlevels, max_nboxes, ghost_cells )

  failed = &
  samr%nlevels .ne. nlevels &
  .or. any ( samr%max_nboxes .ne. max_nboxes ) &
  .or. any ( samr%ghost_cells .ne. ghost_cells ) &
  .or. any ( samr%base_grid .ne. base_grid )
end function rhyme_samr_init_with_test
