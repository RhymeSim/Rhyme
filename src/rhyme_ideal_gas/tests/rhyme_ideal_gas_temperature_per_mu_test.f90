logical function rhyme_ideal_gas_temperature_per_mu_test () result ( failed )
  use rhyme_ideal_gas_factory
  use rhyme_physics_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( physics_t ) :: physics
  real ( kind=8 ) :: t_mu, u( cid%rho:cid%e_tot )

  ig_tester = .describe. "temperature_per_mu"

  u = hy_factory%generate_conserved()
  physics = ph_factory%generate()

  t_mu = hy_factory%p / hy_factory%rho / ( physics%kb%v / physics%amu%v )

  call ig_tester%expect( .notToBeNaN. rhyme_ideal_gas_temperature_per_mu( 7.d0/5.d0, u ) )
  call ig_tester%expect( rhyme_ideal_gas_temperature_per_mu( 7.d0/5.d0, u ) &
    .toBe. t_mu .within. 7 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_temperature_per_mu_test
