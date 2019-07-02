logical function rhyme_slope_limiter_minmod_test () result (failed)
  use rhyme_slope_limiter_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: sl_tester

  type ( slope_limiter_t ) :: sl
  real ( kind=8 ), dimension( cid%rho:cid%e_tot ) :: delta, cons, UL, UR, UM

  sl_tester = .describe. "slope_limiter_minmod"

  cons = hy_factory%generate_conserved()

  UL = cons
  UR = cons

  UM = cons * 1.23d0
  call rhyme_slope_limiter_minmod( sl, UL, UM, UR, delta )
  call sl_tester%expect( delta .toBe. 0.d0 )

  UM = cons * (-2.34)
  call rhyme_slope_limiter_minmod( sl, UL, UM, UR, delta )
  call sl_tester%expect( delta .toBe. 0.d0 )

  UM = cons * 1.23d0
  UR = cons * 2.34d0
  call rhyme_slope_limiter_minmod( sl, UL, UM, UR, delta )
  call sl_tester%expect( delta .notToBe. 0.d0 )

  UM = cons * (-1.23d0)
  UR = cons * (-2.34d0)
  call rhyme_slope_limiter_minmod( sl, UL, UM, UR, delta )
  call sl_tester%expect( delta .notToBe. 0.d0 )

  failed = sl_tester%failed()
end function rhyme_slope_limiter_minmod_test
