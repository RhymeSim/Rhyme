logical function rhyme_slope_limiter_superbee_test () result (failed)
  use rhyme_slope_limiter_factory

  implicit none

  type ( slope_limiter_t ) :: sl
  type ( hydro_conserved_t ) :: Delta

  call chemi%init
  call ig%init_with ( chemi, igid%diatomic )

  UL%u = cons%u
  UR%u = cons%u


  UM%u = cons%u * 1.23d0
  call sl%superbee ( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) > epsilon(0.d0) )
  if ( failed ) return


  UM%u = cons%u * (-2.34)
  call sl%superbee ( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) > epsilon(0.d0) )
  if ( failed ) return


  UM%u = cons%u * 1.23d0
  UR%u = cons%u * 2.34d0
  call sl%superbee ( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) < epsilon(0.d0) )
  if ( failed ) return


  UM%u = cons%u * (-1.23d0)
  UR%u = cons%u * (-2.34d0)
  call sl%superbee ( cfl, ig, UL, UM, UR, Delta )

  failed = any ( abs ( Delta%u ) < epsilon(0.d0) )
  if ( failed ) return
end function rhyme_slope_limiter_superbee_test
