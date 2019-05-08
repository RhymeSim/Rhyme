logical function rhyme_ideal_gas_primitive_vars_to_conserved_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons_calc

  ig_tester = .describe. "ideal_gas_prim_vars_to_cons"

  call hy_factory%init
  ig = ig_factory%generate()

  call ig%prim_vars_to_cons( hy_factory%rho, hy_factory%u, &
    hy_factory%v, hy_factory%w, hy_factory%p, cons_calc )

  call ig_tester%expect( cons_calc%u( hyid%rho ) .toBe. hy_factory%rho )
  call ig_tester%expect( cons_calc%u( hyid%rho_u ) .toBe. hy_factory%rho * hy_factory%u )
  call ig_tester%expect( cons_calc%u( hyid%rho_v ) .toBe. hy_factory%rho * hy_factory%v )
  call ig_tester%expect( cons_calc%u( hyid%rho_w ) .toBe. hy_factory%rho * hy_factory%w )
  call ig_tester%expect( cons_calc%u( hyid%e_tot ) .toBe. hy_factory%e_tot )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_primitive_vars_to_conserved_test
