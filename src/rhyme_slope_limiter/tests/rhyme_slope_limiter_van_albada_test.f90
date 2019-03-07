logical function rhyme_slope_limiter_van_albada_test () result (failed)
  use rhyme_slope_limiter_factory

  implicit none

  type ( slope_limiter_t ) :: sl
  type ( hydro_conserved_t ) :: Delta
  type ( cfl_t ) :: cfl
  type ( ideal_gas_t ) :: ig

  call rhyme_slope_limiter_factory_init
  call ig%init_with( chemi, thermo, igid%diatomic, log )

  UL%u = hy%cons%u
  UR%u = hy%cons%u

  UM%u = hy%cons%u * 1.23d0
  call sl%van_albada( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) > epsilon(0.d0) )
  if ( failed ) return


  UM%u = hy%cons%u * (-2.34)
  call sl%van_albada( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) > epsilon(0.d0) )
  if ( failed ) return


  UM%u = hy%cons%u * 1.23d0
  UR%u = hy%cons%u * 2.34d0
  call sl%van_albada( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) < epsilon(0.d0) )
  if ( failed ) return


  UM%u = hy%cons%u * (-1.23d0)
  UR%u = hy%cons%u * (-2.34d0)
  call sl%van_albada( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) < epsilon(0.d0) )
  if ( failed ) return
end function rhyme_slope_limiter_van_albada_test
