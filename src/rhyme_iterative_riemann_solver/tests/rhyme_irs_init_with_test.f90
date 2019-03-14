logical function rhyme_irs_init_with_test () result ( failed )
  use rhyme_iterative_riemann_solver_factory

  implicit none


  call rhyme_iterative_riemann_solver_factory_init

  call irs%init_with ( &
    irs_fac_n_iteration, &
    irs_fac_tolerance, &
    irs_fac_pressure_floor, &
    log &
  )

  failed = &
  .not. irs%initialized &
  .or. .not. irs_fac_initialized &
  .or. irs%n_iteration .ne. irs_fac_n_iteration &
  .or. abs ( irs%tolerance - irs_fac_tolerance ) > epsilon(0.d0) &
  .or. abs ( irs%pressure_floor - irs_fac_pressure_floor ) > epsilon(0.d0)
end function rhyme_irs_init_with_test
