logical function rhyme_initial_condition_load_test () result ( failed )
  use rhyme_samr_factory
  use rhyme_initial_condition_factory

  implicit none

  character ( len=1024 ) :: nickname = 'rhyme_initial_condition_load.h5'

  type ( initial_condition_t ) :: ic
  type ( samr_t ) :: samr, samr_read
  type ( chombo_t ) :: ch, chreader
  type ( log_t ) :: log
  character ( len=1024 ) :: filename

  call rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

  ! Prepare chombo file
  ch%nickname = nickname
  call ch%filename_generator ( filename )
  call ch%write_samr ( samr )

  ic%type = icid%load
  ic%path = filename
  ic%max_nboxes = max_nboxes
  call ic%load ( samr_read, log )

  failed = .true.
end function rhyme_initial_condition_load_test
