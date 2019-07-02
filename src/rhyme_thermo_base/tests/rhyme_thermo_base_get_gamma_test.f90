logical function rhyme_thermo_base_get_gamma_test () result ( failed )
  use rhyme_thermo_base_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: th_tester

  type ( thermo_base_t ) :: thermo
  type ( log_t ) :: logger

  th_tester = .describe. "get_gamma"

  logger = log_factory%generate()


  thermo = th_factory%generate( thid%monatomic )
  call rhyme_thermo_base_init( thermo, logger )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. ig_gamma( thid%monatomic ) )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. get_gamma() )

  thermo = th_factory%generate( thid%diatomic )
  call rhyme_thermo_base_init( thermo, logger )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. ig_gamma( thid%diatomic ) )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. get_gamma() )

  thermo = th_factory%generate( thid%polyatomic )
  call rhyme_thermo_base_init( thermo, logger )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. ig_gamma( thid%polyatomic ) )
  call th_tester%expect( rhyme_thermo_base_get_gamma() .toBe. get_gamma() )

  failed = th_tester%failed()
end function rhyme_thermo_base_get_gamma_test
