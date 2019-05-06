logical function rhyme_ideal_gas_t_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  real ( kind=8 ) :: maw ! mean atomic weight

  ig_tester = .describe. "ideal_gas t"

  call rhyme_ideal_gas_factory_init

  ig%type = ig_gas_type
  call rhyme_ideal_gas_init( ig, ig_chemi, ig_thermo, ig_units, ig_logger )

  maw = 2.34d1

  call ig_tester%expect( ig%T( ig_hy%cons, maw ) .toBe. ig%T_per_mu( ig_hy%cons ) * maw )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_t_test
