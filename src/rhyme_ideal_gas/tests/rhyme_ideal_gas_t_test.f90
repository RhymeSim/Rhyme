logical function rhyme_ideal_gas_t_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons
  real ( kind=8 ) :: maw ! mean atomic weight

  ig_tester = .describe. "ideal_gas t"

  cons = hy_factory%conserved()
  ig = ig_factory%generate()

  maw = 2.34d1

  call ig_tester%expect( ig%T( cons, maw ) .toBe. ig%T_per_mu( cons ) * maw )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_t_test
