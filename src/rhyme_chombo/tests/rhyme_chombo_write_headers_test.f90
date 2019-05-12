logical function rhyme_chombo_write_headers_test () result ( failed )
  use rhyme_chombo_factory
  use rhyme_samr_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chombo_t ) :: ch
  type ( samr_t ) :: samr

  ! variables
  integer :: ndims_read

  ! Chombo filename
  character ( len=1024 ), parameter :: nickname = "rhyme_chombo_write_headers"
  character ( len=1024 ) :: filename

  ch_tester = .describe. "chombo write_headers"

  samr = samr_factory%generate()

  ch%nickname = nickname
  ch%iteration = samr%levels(0)%iteration

  call ch%filename_generator( filename )
  call ch%create_chombo

  call ch%write_headers( samr )

  call ch%close

  call ch_tester%expect( int( ch%level_ids(0:samr%nlevels-1) ) .notToBe. chid%unset )
  call ch_tester%expect( int( ch%level_ids(samr%nlevels:) ) .toBe. chid%unset )
  call ch_tester%expect( int( ch%chombo_global_id ) .notToBe. chid%unset )

  call ch%open( filename )

  call ch%read_group_attr( "/Chombo_global", "SpaceDim", ndims_read )

  call ch_tester%expect( ndims_read .toBe. 3 )

  call ch%close

  failed = ch_tester%failed()
end function rhyme_chombo_write_headers_test
