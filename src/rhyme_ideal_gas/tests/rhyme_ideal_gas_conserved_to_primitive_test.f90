logical function rhyme_ideal_gas_conserved_to_primitive_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  type ( hydro_primitive_t ) :: prim_calc

  call chemi%init
  call thermo%init

  call ig%init_with ( chemi, thermo, gas_type )

  call ig%cons_to_prim ( cons, prim_calc )

  failed = &
  abs ( prim_calc%w(hyid%rho) - rho ) > epsilon(0.d0) &
  .or. abs ( prim_calc%w(hyid%u) - u ) > epsilon(0.d0) &
  .or. abs ( prim_calc%w(hyid%v) - v ) > epsilon(0.d0) &
  .or. abs ( prim_calc%w(hyid%w) - w ) > epsilon(0.d0) &
  .or. abs ( prim_calc%w(hyid%p) - p ) > epsilon(0.d0)
end function rhyme_ideal_gas_conserved_to_primitive_test
