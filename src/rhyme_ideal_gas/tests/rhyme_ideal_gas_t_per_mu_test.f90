logical function rhyme_ideal_gas_t_per_mu_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons
  real ( kind=4 ) :: T__mu

  ig_tester = .describe. "ideal_gas t_per_mu"

  cons = hy_factory%conserved()
  ig = ig_factory%generate()

  T__mu = real( hy_factory%T / hy_factory%mu )

  call ig_tester%expect( ig%T_per_mu( cons ) .toBe. T__mu .within. 7 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_t_per_mu_test
