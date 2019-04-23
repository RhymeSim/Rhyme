logical function rhyme_ideal_gas_total_energy_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  real ( kind=8 ) :: e_tot

  ig_tester = .describe. "ideal_gas e_tot"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  e_tot = hy%rho * ( ig%e_kin_sp( hy%cons ) + ig%e_int_sp( hy%cons ) )

  call ig_tester%expect( hy%e_tot .toBe. e_tot )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_total_energy_test
