logical function rhyme_chombo_write_headers_test () result ( failed )
  use rhyme_chombo_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  ! rhyme_chombo variables
  type ( chombo_t ) :: ch

  ! variables
  integer :: ndims_read

  ! Chombo filename
  character ( len=1024 ), parameter :: nickname = "rhyme_chombo_write_headers"
  character ( len=1024 ) :: filename

  ch_tester = .describe. "chombo write_headers"

  call rhyme_chombo_factory_init

  ch%nickname = nickname
  ch%iteration = chombo_fac_samr%levels(0)%iteration

  call ch%filename_generator( filename )
  call ch%create_chombo

  call ch%write_headers( chombo_fac_samr )

  call ch%close

  call ch_tester%expect( int( ch%level_ids(0:chombo_fac_samr%nlevels-1) ) .notToBe. chid%unset )
  call ch_tester%expect( int( ch%level_ids(chombo_fac_samr%nlevels:) ) .toBe. chid%unset )
  call ch_tester%expect( int( ch%chombo_global_id ) .notToBe. chid%unset )

  call ch%open( filename )

  call ch%read_group_attr( "/Chombo_global", "SpaceDim", ndims_read )

  call ch_tester%expect( ndims_read .toBe. 3 )

  call ch%close

  failed = ch_tester%failed()
end function rhyme_chombo_write_headers_test
