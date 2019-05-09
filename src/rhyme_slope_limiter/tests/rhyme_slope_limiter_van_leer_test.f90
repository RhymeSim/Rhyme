logical function rhyme_slope_limiter_van_leer_test () result (failed)
  use rhyme_slope_limiter_factory
  use rhyme_hydro_base_factory
  use rhyme_ideal_gas_factory
  use rhyme_cfl_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: sl_tester

  type ( slope_limiter_t ) :: sl
  type ( ideal_gas_t ) :: ig
  type ( cfl_t ) :: cfl
  type ( hydro_conserved_t ) :: delta, cons, UL, UR, UM

  sl_tester = .describe. "slope_limiter_van_leer"

  cfl = cfl_factory%generate()
  ig = ig_factory%generate( igid%diatomic )
  cons = hy_factory%conserved()

  UL%u = cons%u
  UR%u = cons%u

  UM%u = cons%u * 1.23d0
  call rhyme_slope_limiter_van_leer( sl, cfl, ig, ul, um, ur, delta )
  call sl_tester%expect( delta%u .toBe. 0.d0 )

  UM%u = cons%u * (-2.34)
  call rhyme_slope_limiter_van_leer( sl, cfl, ig, ul, um, ur, delta )
  call sl_tester%expect( delta%u .toBe. 0.d0 )

  UM%u = cons%u * 1.23d0
  UR%u = cons%u * 2.34d0
  call rhyme_slope_limiter_van_leer( sl, cfl, ig, ul, um, ur, delta )
  call sl_tester%expect( delta%u .notToBe. 0.d0 )

  UM%u = cons%u * (-1.23d0)
  UR%u = cons%u * (-2.34d0)
  call rhyme_slope_limiter_van_leer( sl, cfl, ig, ul, um, ur, delta )
  call sl_tester%expect( delta%u .notToBe. 0.d0 )

  failed = sl_tester%failed()
end function rhyme_slope_limiter_van_leer_test
