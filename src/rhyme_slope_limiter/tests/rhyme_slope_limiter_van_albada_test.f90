logical function rhyme_slope_limiter_van_albada_test () result (failed)
  use rhyme_slope_limiter_factory

  implicit none

  type ( slope_limiter_t ) :: sl
  type ( hydro_conserved_t ) :: Delta
  type ( cfl_t ) :: cfl
  type ( ideal_gas_t ) :: ig
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo

  call chemi%init
  call thermo%init
  call ig%init_with ( chemi, thermo, igid%diatomic )

  UL%u = cons%u
  UR%u = cons%u


  UM%u = cons%u * 1.23d0
  call sl%van_albada ( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) > epsilon(0.d0) )
  if ( failed ) return


  UM%u = cons%u * (-2.34)
  call sl%van_albada ( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) > epsilon(0.d0) )
  if ( failed ) return


  UM%u = cons%u * 1.23d0
  UR%u = cons%u * 2.34d0
  call sl%van_albada ( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) < epsilon(0.d0) )
  if ( failed ) return


  UM%u = cons%u * (-1.23d0)
  UR%u = cons%u * (-2.34d0)
  call sl%van_albada ( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) < epsilon(0.d0) )
  if ( failed ) return
end function rhyme_slope_limiter_van_albada_test
