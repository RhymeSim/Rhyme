logical function rhyme_irs_init_test () result ( failed )
  use rhyme_iterative_riemann_solver_factory

  implicit none

  call irs%init( log )

  failed = .not. irs%initialized
end function rhyme_irs_init_test
