logical function rhyme_ideal_gas_specific_kinetic_energy_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  real ( kind=4 ) :: hy_sp_kin_e

  ig_tester = .describe. "ideal_gas e_kin_sp"

  call rhyme_ideal_gas_factory_init

  ig%type = ig_gas_type
  call rhyme_ideal_gas_init( ig, ig_chemi, ig_thermo, ig_units, ig_logger )

  hy_sp_kin_e = real( hy_sp_kinetic_e( ig_hy%cons ) )

  call ig_tester%expect( ig%e_kin_sp( ig_hy%cons ) .toBe. real( ig_hy%e_kin_sp ) .within. 7 )
  call ig_tester%expect( ig%e_kin_sp( ig_hy%cons ) .toBe. hy_sp_kin_e .within. 7 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_specific_kinetic_energy_test
