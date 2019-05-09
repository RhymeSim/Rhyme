logical function rhyme_irs_init_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  type ( irs_t ) :: irs
  type ( log_t ) :: logger

  irs_tester = .describe. "irs_init"

  irs = irs_factory%generate()
  logger = log_factory%generate()

  call irs%init( logger )

  call irs_tester%expect( irs%initialized .toBe. .true. )

  failed = irs_tester%failed()
end function rhyme_irs_init_test
