logical function rhyme_ideal_gas_t_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  real ( kind=8 ) :: maw ! mean atomic weight

  ig_tester = .describe. "ideal_gas t"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  maw = 2.34d1

  call ig_tester%expect( ig%T( hy%cons, maw ) .toBe. ig%T_per_mu( hy%cons ) * maw )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_t_test
