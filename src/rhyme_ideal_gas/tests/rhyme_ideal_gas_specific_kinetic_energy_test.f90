logical function rhyme_ideal_gas_specific_kinetic_energy_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  real ( kind=4 ) :: hy_sp_kin_e

  ig_tester = .describe. "ideal_gas e_kin_sp"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  hy_sp_kin_e = real( hy_sp_kinetic_e( hy%cons ) )

  call ig_tester%expect( ig%e_kin_sp( hy%cons ) .toBe. real( hy%e_kin_sp ) .within. 7 )
  call ig_tester%expect( ig%e_kin_sp( hy%cons ) .toBe. hy_sp_kin_e .within. 7 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_specific_kinetic_energy_test
