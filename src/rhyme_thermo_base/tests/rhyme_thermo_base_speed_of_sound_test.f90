logical function rhyme_thermo_base_speed_of_sound_test () result ( failed )
  use rhyme_thermo_base_factory
  use rhyme_physics_factory
  use rhyme_hydro_base_factory
  use rhyme_logger_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: th_tester

  type ( physics_t ) :: physics
  type ( thermo_base_t ) :: thermo
  type ( logger_t ) :: logger
  real ( kind=8 ) :: u( cid%rho:cid%e_tot )

  integer :: gas_type

  th_tester = .describe. "speed_of_sound"

  physics = ph_factory%generate()
  logger = log_factory%generate()
  u = hy_factory%generate_conserved()

  do gas_type = thid%monatomic, thid%polyatomic
    thermo = th_factory%generate( physics, gas_type )
    call rhyme_thermo_base_init( thermo, physics, logger )
    call th_tester%expect( .notToBeNaN. rhyme_thermo_base_speed_of_sound( u ) )
    call th_tester%expect( rhyme_thermo_base_speed_of_sound( u ) &
      .toBe. rhyme_ideal_gas_speed_of_sound( ig_gamma( gas_type ), th_factory%kb_amu, u ) .within. 15 )
  end do

  call th_tester%expect( calc_cs( u ) .toBe. rhyme_thermo_base_speed_of_sound( u ) .within. 15 )

  failed = th_tester%failed()
end function rhyme_thermo_base_speed_of_sound_test
