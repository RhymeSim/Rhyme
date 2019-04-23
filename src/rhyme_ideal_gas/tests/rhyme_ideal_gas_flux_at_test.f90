logical function rhyme_ideal_gas_flux_at_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_flux_t ) :: f

  ig_tester = .describe. "ideal_gas flux_at"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  call ig%flux_at( hy%cons, hyid%y, f )

  call ig_tester%expect( f%f( hyid%rho ) .toBe. hy%rho * hy%v )
  call ig_tester%expect( f%f( hyid%rho_u ) .toBe. hy%rho * hy%u * hy%v )
  call ig_tester%expect( f%f( hyid%rho_v ) .toBe. (hy%rho * hy%v**2 + hy%p) )
  call ig_tester%expect( f%f( hyid%rho_w ) .toBe. hy%rho * hy%w * hy%v )
  call ig_tester%expect( f%f( hyid%e_tot ) .toBe. hy%v * ( hy%e_tot + hy%p ) )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_flux_at_test
