logical function rhyme_ideal_gas_specific_internal_energy_test () result ( failed )
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  real ( kind=8 ) :: u( cid%rho:cid%e_tot )

  ig_tester = .describe. "specific_internal_energy"

  u = hy_factory%generate_conserved()

  call ig_tester%expect( .notToBeNaN. rhyme_ideal_gas_specific_internal_energy( 7.d0/5.d0, u ) )
  call ig_tester%expect( rhyme_ideal_gas_specific_internal_energy( 7.d0/5.d0, u ) &
    .toBe. hy_factory%e_int / hy_factory%rho .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_specific_internal_energy_test
