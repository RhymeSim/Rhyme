logical function rhyme_irs_iterate_test () result (failed)
  use rhyme_irs_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  call rhyme_irs_factory_init

  call irs_iterate_test_cases( irs_Sod_test, "Sod", irs_tester, 5 )
  call irs_iterate_test_cases( irs_123_test, "123 test", irs_tester, 4 )
  call irs_iterate_test_cases( irs_left_blast_wave_test, "left blast wave", irs_tester, 5 )
  call irs_iterate_test_cases( irs_right_blast_wave_test, "right blast wave", irs_tester, 5 )
  call irs_iterate_test_cases( irs_two_shocks_collision_test, "two shock collision", irs_tester, 3 )

  failed = irs_tester%failed()

contains
  subroutine irs_iterate_test_cases ( func, test_name, tester, sig_fig )
    implicit none

    external :: func
    character ( len=* ) :: test_name
    type ( assertion_t ) :: tester
    integer :: sig_fig ! Significant figures

    type ( hydro_conserved_t ) :: L, R
    type ( riemann_problem_solution_t ) :: expected_solution, solution
    real ( kind=8 ) :: ex_p, ex_u, ex_left_rho, ex_right_rho
    real ( kind=8 ) :: star_left_rho, star_right_rho

    call func( irs_fac_ig, L, R, expected_solution )
    call rhyme_irs_factory_set_sides( irs_fac_ig, L, R, solution )

    ex_p = expected_solution%star%p
    ex_u = expected_solution%star%u

    call rhyme_irs_iterate( irs_fac, irs_fac_ig, solution, hyid%x )

    if ( expected_solution%star%left%is_shock ) then
      ex_left_rho = expected_solution%star%left%shock%rho
      star_left_rho = solution%star%left%shock%rho
    else
      ex_left_rho = expected_solution%star%left%fan%rho
      star_left_rho = solution%star%left%fan%rho
    end if

    if ( expected_solution%star%right%is_shock ) then
      ex_right_rho = expected_solution%star%right%shock%rho
      star_right_rho = solution%star%right%shock%rho
    else
      ex_right_rho = expected_solution%star%right%fan%rho
      star_right_rho = solution%star%right%fan%rho
    end if

    call tester%expect( &
      solution%star%p .toBe. ex_p .within. sig_fig .hint. test_name//' p' )

    call tester%expect( &
      solution%star%u .toBe. ex_u .within. sig_fig .hint. test_name//' u' )

    call tester%expect( &
      solution%star%left%is_shock .toBe. expected_solution%star%left%is_shock &
      .hint. test_name//' left is shock?' )

    call tester%expect( &
      star_left_rho .toBe. ex_left_rho .within. sig_fig .hint. test_name//' rhol' )

    call tester%expect( &
      solution%star%right%is_shock .toBe. expected_solution%star%right%is_shock &
      .hint. test_name//' right is shock' )

    call tester%expect( &
      star_right_rho .toBe. ex_right_rho .within. sig_fig .hint. test_name//' rhor' )
  end subroutine irs_iterate_test_cases
end function rhyme_irs_iterate_test
