logical function rhyme_initial_condition_load_r2c_2d_test () result ( failed )
  use rhyme_samr
  use rhyme_initial_condition_factory

  implicit none

  character ( len=1024 ) :: filename = 'r2c_2d_sample.hdf5'

  type ( initial_condition_t ) :: ic
  type ( samr_t ) :: samr
  type ( ideal_gas_t ) :: ig
  type ( log_t ) :: log

  ! Running load_header
  ic%type = icid%snapshot
  ic%snapshot_type = icid%r2c_2d
  ic%path = filename
  ic%max_nboxes = max_nboxes_uni

  call ic%load_headers( samr )
  samr%base_grid(3) = 1 ! Bug in R2C
  samr%ghost_cells(3) = 0

  call ig%init_with( igid%monatomic )
  call ic%load_r2c_2d( samr, ig, log )

  ! TODO: test if loaded r2c snapshot is converted correctly
  failed = .true.
end function rhyme_initial_condition_load_r2c_2d_test
