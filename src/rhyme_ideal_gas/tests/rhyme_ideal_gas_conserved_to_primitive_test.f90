logical function rhyme_ideal_gas_conserved_to_primitive_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons
  type ( hydro_primitive_t ) :: prim

  ig_tester = .describe. "ideal_gas_cons_to_prim"

  ig = ig_factory%generate()

  cons = hy_factory%conserved()
  call ig%cons_to_prim( cons, prim )

  call ig_tester%expect( prim%w( hyid%rho ) .toBe. hy_factory%rho )
  call ig_tester%expect( prim%w( hyid%u ) .toBe. hy_factory%u )
  call ig_tester%expect( prim%w( hyid%v ) .toBe. hy_factory%v )
  call ig_tester%expect( prim%w( hyid%w ) .toBe. hy_factory%w )
  call ig_tester%expect( prim%w( hyid%p ) .toBe. hy_factory%p .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_conserved_to_primitive_test
