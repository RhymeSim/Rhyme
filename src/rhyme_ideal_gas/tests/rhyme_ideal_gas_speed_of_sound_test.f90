logical function rhyme_ideal_gas_speed_of_sound_test () result ( failed )
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  real ( kind=8 ) :: cs, u( cid%rho:cid%e_tot )

  ig_tester = .describe. "speed_of_sound"

  u = hy_factory%generate_conserved()

  cs = sqrt( 7.d0/5.d0 * hy_factory%p / hy_factory%rho )

  call ig_tester%expect( .notToBeNaN. rhyme_ideal_gas_speed_of_sound( 7.d0/5.d0, u ) )
  call ig_tester%expect( rhyme_ideal_gas_speed_of_sound( 7.d0/5.d0, u ) .toBe. cs &
    .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_speed_of_sound_test
