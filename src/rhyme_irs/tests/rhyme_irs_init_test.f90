logical function rhyme_irs_init_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  irs_tester = .describe. "irs_init"

  call irs_fac%init( irs_fac_log )

  call irs_tester%expect( irs_fac%initialized .toBe. .true. )

  failed = irs_tester%failed()
end function rhyme_irs_init_test
