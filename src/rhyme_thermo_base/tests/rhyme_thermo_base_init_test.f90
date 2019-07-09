logical function rhyme_thermo_base_init_test () result ( failed )
  use rhyme_thermo_base_factory
  use rhyme_physics_factory
  use rhyme_logger_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: th_tester

  type ( physics_t ) :: physics
  type ( thermo_base_t ) :: thermo
  type ( logger_t ) :: logger

  th_tester = .describe. "init"

  physics = ph_factory%generate()
  logger = log_factory%generate()

  thermo = th_factory%generate( physics, thid%diatomic )

  call rhyme_thermo_base_init( thermo, physics, logger )

  failed = th_tester%failed()
end function rhyme_thermo_base_init_test
