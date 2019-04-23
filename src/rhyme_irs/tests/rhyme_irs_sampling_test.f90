logical function rhyme_irs_sampling_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  type ( hydro_conserved_t ) :: L, R, U
  type ( riemann_problem_solution_t ) :: solution

  integer :: i
  real ( kind=8 ) :: x, rho_, v, p, e_int

  irs_tester = .describe. "irs sampling"

  call rhyme_irs_factory_init

  call irs_Sod_test( irs_fac_ig, L, R, solution )
  call rhyme_irs_iterate( irs_fac, irs_fac_ig, solution, hyid%x )

  open ( unit=1, file="./sod_shock_tube_t_0_2_analytical.txt", action='read', form='formatted' )
  do i = 1, 12
    read (1, *)
  end do

  do i = 1, 500
    call irs_tester%reset

    read (1, *) x, rho_, v, p
    e_int = p / 0.4d0

    call rhyme_irs_sampling( irs_fac_ig, solution, hyid%x, &
      -.5d0 + real(i - 1, kind=8) / 499.d0, .2d0, U )

      print *, i
    call irs_tester%expect( rho_ .toBe. u%u(hyid%rho) .hint. 'rho' )
    call irs_tester%expect( rho_ * v .toBe. u%u(hyid%rho_u) .hint. 'rho_u' )
    call irs_tester%expect( p .toBe. irs_fac_ig%p(U) .hint. 'p' )
    call irs_tester%expect( e_int .toBe. U%u(hyid%rho) * irs_fac_ig%e_int_sp(U) .within. 1d-3 .hint. 'e_int' )

    failed = irs_tester%failed()

    if ( failed ) then
      close (1)
      return
    end if
  end do

  close (1)
end function rhyme_irs_sampling_test
