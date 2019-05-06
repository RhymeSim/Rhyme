logical function rhyme_ideal_gas_primitive_to_conserved_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons_calc

  ig_tester = .describe. "ideal_gas prim_to_cons"

  call rhyme_ideal_gas_factory_init

  ig%type = ig_gas_type
  call rhyme_ideal_gas_init( ig, ig_chemi, ig_thermo, ig_units, ig_logger )

  call ig%prim_to_cons( ig_hy%prim, cons_calc )

  call ig_tester%expect( cons_calc%u( hyid%rho) .toBe. ig_hy%rho )
  call ig_tester%expect( cons_calc%u( hyid%rho_u ) .toBe. ig_hy%rho * ig_hy%u )
  call ig_tester%expect( cons_calc%u( hyid%rho_v ) .toBe. ig_hy%rho * ig_hy%v )
  call ig_tester%expect( cons_calc%u( hyid%rho_w ) .toBe. ig_hy%rho * ig_hy%w )
  call ig_tester%expect( cons_calc%u( hyid%e_tot ) .toBe. ig_hy%e_tot )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_primitive_to_conserved_test
