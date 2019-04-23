logical function rhyme_ideal_gas_t_per_mu_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  real ( kind=4 ) :: T__mu

  ig_tester = .describe. "ideal_gas t_per_mu"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  T__mu = real( hy%T / hy%mu )

  call ig_tester%expect( ig%T_per_mu( hy%cons ) .toBe. T__mu )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_t_per_mu_test
