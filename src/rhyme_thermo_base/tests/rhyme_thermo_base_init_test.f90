logical function rhyme_thermo_base_init_test () result ( failed )
  use rhyme_thermo_base_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: th_tester

  type ( thermo_base_t ) :: thermo
  type ( log_t ) :: logger

  th_tester = .describe. "init"

  thermo = th_factory%generate( thid%diatomic )
  logger = log_factory%generate()

  call rhyme_thermo_base_init( thermo, logger )

  failed = th_tester%failed()
end function rhyme_thermo_base_init_test
