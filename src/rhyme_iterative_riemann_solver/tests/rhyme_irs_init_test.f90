logical function rhyme_irs_init_test () result ( failed )
  use rhyme_iterative_riemann_solver_factory

  implicit none

  call irs_fac%init( irs_fac_log )

  failed = .not. irs_fac%initialized
end function rhyme_irs_init_test
