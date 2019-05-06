logical function rhyme_ideal_gas_conserved_to_primitive_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_primitive_t ) :: prim_calc

  ig_tester = .describe. "ideal_gas cons_to_prim"

  call rhyme_ideal_gas_factory_init

  ig%type = ig_gas_type
  call rhyme_ideal_gas_init( ig, ig_chemi, ig_thermo, ig_units, ig_logger )

  call ig%cons_to_prim( ig_hy%cons, prim_calc )

  call ig_tester%expect( prim_calc%w( hyid%rho ) .toBe. ig_hy%rho )
  call ig_tester%expect( prim_calc%w( hyid%u ) .toBe. ig_hy%u )
  call ig_tester%expect( prim_calc%w( hyid%v ) .toBe. ig_hy%v )
  call ig_tester%expect( prim_calc%w( hyid%w ) .toBe. ig_hy%w )
  call ig_tester%expect( prim_calc%w( hyid%p ) .toBe. ig_hy%p .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_conserved_to_primitive_test
