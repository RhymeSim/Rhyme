logical function rhyme_ideal_gas_conserved_to_primitive_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_primitive_t ) :: prim_calc

  ig_tester = .describe. "ideal_gas cons_to_prim"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  call ig%cons_to_prim( hy%cons, prim_calc )

  call ig_tester%expect( prim_calc%w( hyid%rho ) .toBe. hy%rho )
  call ig_tester%expect( prim_calc%w( hyid%u ) .toBe. hy%u )
  call ig_tester%expect( prim_calc%w( hyid%v ) .toBe. hy%v )
  call ig_tester%expect( prim_calc%w( hyid%w ) .toBe. hy%w )
  call ig_tester%expect( prim_calc%w( hyid%p ) .toBe. hy%p )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_conserved_to_primitive_test
