logical function rhyme_chombo_write_headers_test () result ( failed )
  use rhyme_chombo_factory
  use rhyme_samr_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chombo_t ) :: ch
  type ( samr_t ) :: samr
  type ( log_t ) :: logger

  ! variables
  integer :: ndims_read

  ! Chombo filename
  character ( len=1024 ), parameter :: nickname = "rhyme_chombo_write_headers"
  character ( len=1024 ) :: filename

  ch_tester = .describe. "chombo write_headers"

  ch = ch_factory%generate()
  samr = samr_factory%generate()
  logger = log_factory%generate()

  ch%nickname = nickname
  ch%iteration = samr%levels(0)%iteration

  call rhyme_chombo_init( ch, samr, logger )

  call rhyme_chombo_filename_generator( ch, filename )
  call rhyme_chombo_create_chombo( ch )

  call rhyme_chombo_write_headers( ch, samr )

  call rhyme_hdf5_util_close( ch%file )

  call ch_tester%expect( int( ch%level_ids(0:samr%nlevels-1) ) .notToBe. chid%unset )
  call ch_tester%expect( int( ch%level_ids(samr%nlevels:) ) .toBe. chid%unset )
  call ch_tester%expect( int( ch%chombo_global_id ) .notToBe. chid%unset )

  call rhyme_hdf5_util_open( ch%file, filename )
  call rhyme_hdf5_util_read_group_attr( ch%file, "/Chombo_global", "SpaceDim", ndims_read )

  call ch_tester%expect( ndims_read .toBe. 3 )

  call rhyme_hdf5_util_close( ch%file )

  failed = ch_tester%failed()
end function rhyme_chombo_write_headers_test
