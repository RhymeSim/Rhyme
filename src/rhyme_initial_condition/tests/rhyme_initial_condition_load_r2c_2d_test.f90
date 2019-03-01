logical function rhyme_initial_condition_load_r2c_2d_test () result ( failed )
  use rhyme_samr_factory
  use rhyme_initial_condition_factory

  implicit none

  character ( len=1024 ) :: nickname = 'rhyme_initial_condition_load_r2c_2d.h5'
  character ( len=1024 ) :: filename

  type ( initial_condition_t ) :: ic
  type ( samr_t ) :: samr, samr_read
  type ( chombo_t ) :: ch
  type ( log_t ) :: log

  integer :: l, b, uid, ub(3)


  ! Initializing SAMR object
  call rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

  ! Prepare chombo file
  ch%nickname = nickname
  call ch%filename_generator ( filename )
  call ch%write_samr ( samr )

  ! Running load_header
  ic%type = icid%snapshot
  ic%snapshot_type = icid%rhyme
  ic%path = filename

  samr_read%nlevels = samr%nlevels
  samr_read%base_grid = samr%base_grid
  samr_read%ghost_cells = samr%ghost_cells
  samr_read%max_nboxes = samr%max_nboxes
  samr_read%levels%max_nboxes = samr%max_nboxes

  samr_read%levels%nboxes = 0

  call ic%load_rhyme( samr_read, log )
end function rhyme_initial_condition_load_r2c_2d_test
