logical function rhyme_samr_test () result ( failed )
  use rhyme_samr
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: s_tester

  s_tester = .describe. "samr"

  call s_tester%expect( samrid%ghost .toBe. -1 )
  call s_tester%expect( samrid%unset .toBe. -10 )
  call s_tester%expect( samrid%max_nlevels .toBe. 23 )
  call s_tester%expect( samrid%left .toBe. 1 )
  call s_tester%expect( samrid%right .toBe. 2 )
  call s_tester%expect( samrid%bottom .toBe. 3 )
  call s_tester%expect( samrid%top .toBe. 4 )
  call s_tester%expect( samrid%back .toBe. 5 )
  call s_tester%expect( samrid%front .toBe. 6 )

  failed = s_tester%failed()
end function rhyme_samr_test
