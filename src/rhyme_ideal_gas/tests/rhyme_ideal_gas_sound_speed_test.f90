logical function rhyme_ideal_gas_sound_speed_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons
  real ( kind=4 ) :: cs

  ig_tester = .describe. "ideal_gas sound_speed"

  cons = hy_factory%conserved()
  ig = ig_factory%generate()

  cs = real( sqrt( ig%gamma * hy_factory%p / hy_factory%rho) )
  call ig_tester%expect( ig%Cs( cons ) .toBe. cs )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_sound_speed_test
