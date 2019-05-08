logical function rhyme_ideal_gas_flux_at_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons
  type ( hydro_flux_t ) :: f

  ig_tester = .describe. "ideal_gas flux_at"

  ig = ig_factory%generate()

  cons = hy_factory%conserved()
  call ig%flux_at( cons, hyid%y, f )

  call ig_tester%expect( f%f( hyid%rho ) .toBe. hy_factory%rho * hy_factory%v )
  call ig_tester%expect( f%f( hyid%rho_u ) .toBe. hy_factory%rho * hy_factory%u * hy_factory%v )
  call ig_tester%expect( f%f( hyid%rho_v ) .toBe. (hy_factory%rho * hy_factory%v**2 + hy_factory%p) .within. 15 )
  call ig_tester%expect( f%f( hyid%rho_w ) .toBe. hy_factory%rho * hy_factory%w * hy_factory%v )
  call ig_tester%expect( f%f( hyid%e_tot ) .toBe. hy_factory%v * ( hy_factory%e_tot + hy_factory%p ) .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_flux_at_test
