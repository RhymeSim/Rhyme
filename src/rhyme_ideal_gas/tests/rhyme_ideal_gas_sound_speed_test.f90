logical function rhyme_ideal_gas_sound_speed_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  real ( kind=4 ) :: cs

  ig_tester = .describe. "ideal_gas sound_speed"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  cs = real( sqrt( ig%gamma * hy%p / hy%rho) )
  call ig_tester%expect( ig%Cs( hy%cons ) .toBe. cs )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_sound_speed_test
