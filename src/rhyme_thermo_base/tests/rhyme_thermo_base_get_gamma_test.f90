logical function rhyme_thermo_base_get_gamma_test () result ( failed )
  use rhyme_thermo_base_factory
  use rhyme_physics_factory
  use rhyme_logger_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: th_tester

  type ( physics_t ) :: physics
  type ( thermo_base_t ) :: thermo
  type ( logger_t ) :: logger

  th_tester = .describe. "get_gamma"

  call rhyme_nombre_units_init

  physics = ph_factory%generate()
  logger = log_factory%generate()


  thermo = th_factory%generate( physics, thid%monatomic )
  call rhyme_thermo_base_init( thermo, physics, logger )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. ig_gamma( thid%monatomic ) )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. get_gamma() )

  thermo = th_factory%generate( physics, thid%diatomic )
  call rhyme_thermo_base_init( thermo, physics, logger )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. ig_gamma( thid%diatomic ) )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. get_gamma() )

  thermo = th_factory%generate( physics, thid%polyatomic )
  call rhyme_thermo_base_init( thermo, physics, logger )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. ig_gamma( thid%polyatomic ) )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. get_gamma() )

  failed = th_tester%failed()
end function rhyme_thermo_base_get_gamma_test
