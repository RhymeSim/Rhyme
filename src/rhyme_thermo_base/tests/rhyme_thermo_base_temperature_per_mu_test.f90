logical function rhyme_thermo_base_temperature_per_mu_test () result ( failed )
  use rhyme_thermo_base_factory
  use rhyme_hydro_base_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: th_tester

  type ( thermo_base_t ) :: thermo
  type ( log_t ) :: logger
  real ( kind=8 ) :: u( cid%rho:cid%e_tot )

  integer :: gas_type

  th_tester = .describe. "temperature_per_mu"

  logger = log_factory%generate()
  u = hy_factory%generate_conserved()

  do gas_type = thid%monatomic, thid%polyatomic
    thermo = th_factory%generate( gas_type )
    call rhyme_thermo_base_init( thermo, logger )
    call th_tester%expect( .notToBeNaN. rhyme_thermo_base_temperature_per_mu( u ) )
    call th_tester%expect( rhyme_thermo_base_temperature_per_mu( u ) &
      .toBe. rhyme_ideal_gas_temperature_per_mu( ig_gamma( gas_type ), u ) .within. 15 )
  end do

  call th_tester%expect( calc_t_mu( u ) &
    .toBe. rhyme_thermo_base_temperature_per_mu( u ) .within. 15 )

  failed = th_tester%failed()
end function rhyme_thermo_base_temperature_per_mu_test
