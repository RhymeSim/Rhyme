logical function rhyme_ideal_gas_temperature_test () result ( failed )
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  real ( kind=8 ) :: t, t_exp, u( cid%rho:cid%e_tot )

  ig_tester = .describe. "temperature"

  u = hy_factory%generate_conserved()

  t = rhyme_ideal_gas_temperature( hy_factory%g, hy_factory%kb_amu, hy_factory%mu, u )
  t_exp = rhyme_ideal_gas_temperature_per_mu( hy_factory%g, hy_factory%kb_amu, u ) * hy_factory%mu

  call ig_tester%expect( .notToBeNaN. t )
  call ig_tester%expect( t .toBe. t_exp .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_temperature_test
