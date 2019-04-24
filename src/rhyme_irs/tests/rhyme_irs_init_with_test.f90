logical function rhyme_irs_init_with_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  irs_tester = .describe. "irs_init_with"

  call rhyme_irs_factory_init

  call irs_fac%init_with ( &
    irs_fac_n_iteration, &
    irs_fac_tolerance, &
    irs_fac_pressure_floor, &
    irs_fac_log &
  )

  call irs_tester%expect( irs_fac%initialized .toBe. .true. )
  call irs_tester%expect( irs_fac_initialized .toBe. .true. )
  call irs_tester%expect( irs_fac%n_iteration .toBe. irs_fac_n_iteration )
  call irs_tester%expect( irs_fac%tolerance .toBe. irs_fac_tolerance )
  call irs_tester%expect( irs_fac%pressure_floor .toBe. irs_fac_pressure_floor)

  failed = irs_tester%failed()
end function rhyme_irs_init_with_test
