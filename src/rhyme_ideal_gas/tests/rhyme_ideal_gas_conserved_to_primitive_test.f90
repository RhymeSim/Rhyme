logical function rhyme_ideal_gas_conserved_to_primitive_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( hydro_primitive_t ) :: prim_calc

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  call ig%cons_to_prim( hy%cons, prim_calc )

  failed = &
       abs ( prim_calc%w( hyid%rho ) - hy%rho ) > epsilon(0.d0) &
  .or. abs ( prim_calc%w( hyid%u ) - hy%u ) > epsilon(0.d0) &
  .or. abs ( prim_calc%w( hyid%v ) - hy%v ) > epsilon(0.d0) &
  .or. abs ( prim_calc%w( hyid%w ) - hy%w ) > epsilon(0.d0) &
  .or. abs ( prim_calc%w( hyid%p ) - hy%p ) > epsilon(0.d0)
end function rhyme_ideal_gas_conserved_to_primitive_test
