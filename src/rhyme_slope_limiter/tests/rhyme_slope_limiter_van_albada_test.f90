logical function rhyme_slope_limiter_van_albada_test () result (failed)
  use rhyme_slope_limiter_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: sl_tester

  type ( slope_limiter_t ) :: sl
  type ( hydro_conserved_t ) :: Delta
  type ( cfl_t ) :: cfl

  sl_tester = .describe. "slope_limiter_van_albada"

  call rhyme_slope_limiter_factory_init

  UL%u = sl_hy%cons%u
  UR%u = sl_hy%cons%u

  UM%u = sl_hy%cons%u * 1.23d0
  call sl%van_albada( cfl, sl_ig, UL, UM, UR, Delta )
  call sl_tester%expect( Delta%u .toBe. 0.d0 )

  UM%u = sl_hy%cons%u * (-2.34)
  call sl%van_albada( cfl, sl_ig, UL, UM, UR, Delta )
  call sl_tester%expect( Delta%u .toBe. 0.d0 )

  UM%u = sl_hy%cons%u * 1.23d0
  UR%u = sl_hy%cons%u * 2.34d0
  call sl%van_albada( cfl, sl_ig, UL, UM, UR, Delta )
  call sl_tester%expect( Delta%u .notToBe. 0.d0 )

  UM%u = sl_hy%cons%u * (-1.23d0)
  UR%u = sl_hy%cons%u * (-2.34d0)
  call sl%van_albada( cfl, sl_ig, UL, UM, UR, Delta )
  call sl_tester%expect( Delta%u .notToBe. 0.d0 )

  failed = sl_tester%failed()
end function rhyme_slope_limiter_van_albada_test
