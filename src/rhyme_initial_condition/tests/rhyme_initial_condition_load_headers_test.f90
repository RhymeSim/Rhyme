logical function rhyme_initial_condition_load_headers_test () result ( failed )
  use rhyme_initial_condition_factory
  use rhyme_samr_factory

  implicit none

  character ( len=1024 ) :: nickname = 'rhyme_initial_condition_load_headers'
  character ( len=1024 ) :: filename

  type ( initial_condition_t ) :: ic
  type ( chombo_t ) :: ch
  type ( samr_t ) :: samr, samr_read

  ! Initializing SAMR object
  call rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

  ! Prepare chombo file
  ch%nickname = nickname
  ch%iteration = samr%levels(0)%iteration
  call ch%filename_generator( filename )
  call ch%write_samr( samr )

  ! Running load_header
  ic%type = icid%snapshot
  ic%snapshot_type = icid%rhyme
  ic%path = filename

  call ic%load_headers( samr_read )

  ! Test
  failed = &
  samr_read%nlevels .ne. samr%nlevels &
  .or. any( samr_read%base_grid .ne. samr%base_grid ) &
  .or. any( samr_read%ghost_cells .ne. samr%ghost_cells )
end function rhyme_initial_condition_load_headers_test
