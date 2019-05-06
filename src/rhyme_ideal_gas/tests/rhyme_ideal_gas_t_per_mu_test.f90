logical function rhyme_ideal_gas_t_per_mu_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  real ( kind=4 ) :: T__mu

  ig_tester = .describe. "ideal_gas t_per_mu"

  call rhyme_ideal_gas_factory_init

  ig%type = ig_gas_type
  call rhyme_ideal_gas_init( ig, ig_chemi, ig_thermo, ig_units, ig_logger )

  T__mu = real( ig_hy%T / ig_hy%mu )

  call ig_tester%expect( ig%T_per_mu( ig_hy%cons ) .toBe. T__mu .within. 7 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_t_per_mu_test
