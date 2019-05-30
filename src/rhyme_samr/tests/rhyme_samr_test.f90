logical function rhyme_samr_test () result ( failed )
  use rhyme_samr
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: s_tester

  s_tester = .describe. "samr"

  call s_tester%expect( samrid%ghost .toBe. -1 .hint. 'ghost' )
  call s_tester%expect( samrid%unset .toBe. -10 .hint. 'unset' )
  call s_tester%expect( samrid%max_nlevels .toBe. 23 .hint. 'max_nlevels' )
  call s_tester%expect( samrid%left .toBe. 1 .hint. 'left' )
  call s_tester%expect( samrid%right .toBe. 2 .hint. 'right' )
#if NDIM > 1
  call s_tester%expect( samrid%bottom .toBe. 3 .hint. 'bottom' )
  call s_tester%expect( samrid%top .toBe. 4 .hint. 'top' )
#endif
#if NDIM > 2
  call s_tester%expect( samrid%back .toBe. 5 .hint. 'back' )
  call s_tester%expect( samrid%front .toBe. 6 .hint. 'front' )
#endif

#if HYDRO_SOLVER
  call s_tester%expect( cid%rho .toBe. 1 .hint. 'hydro, rho' )
  call s_tester%expect( cid%u .toBe. 2 .hint. 'hydro, u' )
  call s_tester%expect( cid%rho_u .toBe. 2 .hint. 'hydro, rho_u' )
#if NDIM > 1
  call s_tester%expect( cid%v .toBe. 3 .hint. 'hydro, v' )
  call s_tester%expect( cid%rho_v .toBe. 3 .hint. 'hydro, rho_v' )
#endif
#if NDIM > 2
  call s_tester%expect( cid%w .toBe. 4 .hint. 'hydro, w' )
  call s_tester%expect( cid%rho_w .toBe. 4 .hint. 'hydro, rho_w' )
#endif
  call s_tester%expect( cid%e_tot .toBe. 1 + NDIM + 1 .hint. 'hydro, e_tot' )
  call s_tester%expect( cid%p .toBe. 1 + NDIM + 1 .hint. 'hydro, p' )
#if RT_SOLVER
  call s_tester%expect( cid%temp .toBe. 1 + NDIM + 1 + 1 .hint. 'rhd, temp' )
#endif
#elif RT_SOLVER
  call s_tester%expect( cid%temp .toBe. 1 .hint. 'only rt, temp' )
#endif

  failed = s_tester%failed()
end function rhyme_samr_test
