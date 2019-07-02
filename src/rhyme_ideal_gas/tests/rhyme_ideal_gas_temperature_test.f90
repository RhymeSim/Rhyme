logical function rhyme_ideal_gas_temperature_test () result ( failed )
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  real ( kind=8 ) :: u( cid%rho:cid%e_tot )

  ig_tester = .describe. "temperature"

  u = hy_factory%generate_conserved()

  call ig_tester%expect( .notToBeNaN. rhyme_ideal_gas_temperature( 7.d0/5.d0, 2.34d0, u ) )
  call ig_tester%expect( &
    rhyme_ideal_gas_temperature( 7.d0/5.d0, 2.34d0, u ) &
    .toBe. rhyme_ideal_gas_temperature_per_mu( 7.d0/5.d0, u ) * 2.34d0 .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_temperature_test
