logical function rhyme_slope_limiter_superbee_test () result (failed)
  use rhyme_slope_limiter_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: sl_tester

  type ( slope_limiter_t ) :: sl
  type ( hydro_conserved_t ) :: Delta
  type ( cfl_t ) :: cfl
  type ( ideal_gas_t ) :: ig

  sl_tester = .describe. "slope_limiter_superbee"

  call rhyme_slope_limiter_factory_init
  call ig%init_with( chemi, thermo, igid%diatomic, log )

  UL%u = hy%cons%u
  UR%u = hy%cons%u


  UM%u = hy%cons%u * 1.23d0
  call sl%superbee( cfl, ig, UL, UM, UR, Delta )
  call sl_tester%expect( Delta%u .toBe. 0.d0 )

  UM%u = hy%cons%u * (-2.34)
  call sl%superbee( cfl, ig, UL, UM, UR, Delta )
  call sl_tester%expect( Delta%u .toBe. 0.d0 )


  UM%u = hy%cons%u * 1.23d0
  UR%u = hy%cons%u * 2.34d0
  call sl%superbee( cfl, ig, UL, UM, UR, Delta )
  call sl_tester%expect( Delta%u .notToBe. 0.d0 )

  UM%u = hy%cons%u * (-1.23d0)
  UR%u = hy%cons%u * (-2.34d0)
  call sl%superbee( cfl, ig, UL, UM, UR, Delta )
  call sl_tester%expect( Delta%u .notToBe. 0.d0 )

  failed = sl_tester%failed()
end function rhyme_slope_limiter_superbee_test
