logical function rhyme_chombo_test () result ( failed )
  use rhyme_chombo
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chombo_t ) :: ch

  ch_tester = .describe. "chombo"

  call ch_tester%expect( chid%unset .toBe. h5id%unset )
  call ch_tester%expect( ch%is_opened .toBe. .false. )
  call ch_tester%expect( ch%iteration .toBe. chid%unset )
  call ch_tester%expect( ch%num_levels .toBe. chid%unset )
  call ch_tester%expect( ch%num_components .toBe. chid%unset )
  call ch_tester%expect( int( ch%level_ids ) .toBe. chid%unset )
  call ch_tester%expect( ch%initialized .toBe. .false. )

  failed = ch_tester%failed()
end function rhyme_chombo_test
