logical function rhyme_ideal_gas_primitive_vars_to_conserved_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons_calc

  ig_tester = .describe. "ideal_gas prim_vars_to_cons"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  call ig%prim_vars_to_cons( hy%rho, hy%u, hy%v, hy%w, hy%p, cons_calc )

  call ig_tester%expect( cons_calc%u( hyid%rho ) .toBe. hy%rho )
  call ig_tester%expect( cons_calc%u( hyid%rho_u ) .toBe. hy%rho * hy%u )
  call ig_tester%expect( cons_calc%u( hyid%rho_v ) .toBe. hy%rho * hy%v )
  call ig_tester%expect( cons_calc%u( hyid%rho_w ) .toBe. hy%rho * hy%w )
  call ig_tester%expect( cons_calc%u( hyid%e_tot ) .toBe. hy%e_tot )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_primitive_vars_to_conserved_test
