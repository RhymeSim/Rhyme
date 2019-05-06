logical function rhyme_ideal_gas_flux_at_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_flux_t ) :: f

  ig_tester = .describe. "ideal_gas flux_at"

  call rhyme_ideal_gas_factory_init

  ig%type = ig_gas_type
  call rhyme_ideal_gas_init( ig, ig_chemi, ig_thermo, ig_units, ig_logger )

  call ig%flux_at( ig_hy%cons, hyid%y, f )

  call ig_tester%expect( f%f( hyid%rho ) .toBe. ig_hy%rho * ig_hy%v )
  call ig_tester%expect( f%f( hyid%rho_u ) .toBe. ig_hy%rho * ig_hy%u * ig_hy%v )
  call ig_tester%expect( f%f( hyid%rho_v ) .toBe. (ig_hy%rho * ig_hy%v**2 + ig_hy%p) .within. 15 )
  call ig_tester%expect( f%f( hyid%rho_w ) .toBe. ig_hy%rho * ig_hy%w * ig_hy%v )
  call ig_tester%expect( f%f( hyid%e_tot ) .toBe. ig_hy%v * ( ig_hy%e_tot + ig_hy%p ) .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_flux_at_test
