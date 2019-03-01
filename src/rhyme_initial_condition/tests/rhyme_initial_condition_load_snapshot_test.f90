logical function rhyme_initial_condition_load_snapshot_test () result ( failed )
  use rhyme_samr_factory
  use rhyme_initial_condition_factory

  implicit none

  character ( len=1024 ) :: nickname = 'rhyme_initial_condition_load_snapshot.h5'

  character ( len=1024 ) :: filename
  integer :: l, b, i, j, k, ub(3)
  type ( initial_condition_t ) :: ic
  type ( samr_t ) :: samr, samr_read
  type ( chombo_t ) :: ch
  type ( ideal_gas_t ) :: ig
  type ( log_t ) :: log


  ! Initializing SAMR object
  call rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

  ! Initializing ideal gas
  call ig%init_with( igid%monatomic )

  ! Prepare chombo file
  ch%nickname = nickname
  call ch%filename_generator ( filename )
  call ch%write_samr ( samr )

end function rhyme_initial_condition_load_snapshot_test
