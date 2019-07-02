logical function rhyme_samr_test () result ( failed )
  use rhyme_samr
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: s_tester

  s_tester = .describe. "samr"

  call s_tester%expect( samrid%ghost .toBe. -1 .hint. 'ghost' )
  call s_tester%expect( samrid%unset .toBe. -10 .hint. 'unset' )
  call s_tester%expect( samrid%max_nlevels .toBe. 23 .hint. 'max_nlevels' )
  call s_tester%expect( samrid%x .toBe. 1 .hint. 'left' )
  call s_tester%expect( samrid%left .toBe. 1 .hint. 'left' )
  call s_tester%expect( samrid%right .toBe. 2 .hint. 'right' )
#if NDIM > 1
  call s_tester%expect( samrid%y .toBe. 2 .hint. 'right' )
  call s_tester%expect( samrid%bottom .toBe. 3 .hint. 'bottom' )
  call s_tester%expect( samrid%top .toBe. 4 .hint. 'top' )
#endif
#if NDIM > 2
  call s_tester%expect( samrid%z .toBe. 3 .hint. 'bottom' )
  call s_tester%expect( samrid%back .toBe. 5 .hint. 'back' )
  call s_tester%expect( samrid%front .toBe. 6 .hint. 'front' )
#endif

  failed = s_tester%failed()
end function rhyme_samr_test
