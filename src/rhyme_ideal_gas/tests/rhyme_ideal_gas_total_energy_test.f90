logical function rhyme_ideal_gas_total_energy_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons
  real ( kind=8 ) :: e_tot

  ig_tester = .describe. "ideal_gas e_tot"

  cons = hy_factory%conserved()
  ig = ig_factory%generate()

  e_tot = hy_factory%rho * ( ig%e_kin_sp( cons ) + ig%e_int_sp( cons ) )

  call ig_tester%expect( hy_factory%e_tot .toBe. e_tot )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_total_energy_test
