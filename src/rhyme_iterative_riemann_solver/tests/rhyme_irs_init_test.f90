logical function rhyme_irs_init_test () result ( failed )
  use rhyme_iterative_riemann_solver_factory

  implicit none

  call ig%init_with ( irs_factory_gastype )
  call irs%init ( ig )

  failed = &
  .not. irs%initialized &
  .or. .not. irs%ig%initialized &
  .or. irs%ig%type .ne. irs_factory_gastype
end function rhyme_irs_init_test
