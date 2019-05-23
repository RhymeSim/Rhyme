logical function rhyme_irs_solve_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_irs_tests_factory
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  type ( irs_t ) :: irs
  type ( ideal_gas_t ) :: ig

  irs_tester = .describe. "irs_solve"

  irs = irs_factory%generate()
  ig = ig_factory%generate( igid%diatomic )

  ! Non-vacuum cases
  call irs_solve_test_cases( rhyme_irs_Sod_test, irs, ig, irs_tester )
  call irs_solve_test_cases( rhyme_irs_123_test, irs, ig, irs_tester )
  call irs_solve_test_cases( rhyme_irs_left_blast_wave_test, irs, ig, irs_tester )
  call irs_solve_test_cases( rhyme_irs_right_blast_wave_test, irs, ig, irs_tester )
  call irs_solve_test_cases( rhyme_irs_two_shocks_collision_test, irs, ig, irs_tester )

  failed = irs_tester%failed()

  ! TODO: vacuum cases
end function rhyme_irs_solve_test


subroutine irs_solve_test_cases ( func, irs, ig, tester )
  use rhyme_irs_factory
  use rhyme_assertion

  implicit none

  external :: func
  type ( irs_t ) :: irs
  type ( ideal_gas_t ) :: ig
  type ( assertion_t ) :: tester

  type ( hydro_conserved_t ) :: L, R, U, ex_U
  type ( riemann_problem_solution_t ) :: solution
  real ( kind=8 ) :: dx, dt

  call func( ig, L, R, solution )
  call rhyme_irs_iterate( irs, ig, solution, hyid%x )


  ! Testing the right side of the solution
  dx = 1.d0
  if ( solution%star%right%is_shock ) then
    ! Right side of the right shock (outside the shock)
    dt = dx / solution%star%right%shock%speed - epsilon(0.d0)
    call rhyme_irs_solve( irs, ig, L, R, dx, dt, hyid%x, U )

    call tester%expect( .notToBeNaN. U%u .hint. 'right_side_of_shock' )
    call tester%expect( U%u .toBe. R%u .within. 16 .hint. 'right_side_of_shock' )

    ! Inside the right shock
    dt = dx / solution%star%right%shock%speed + epsilon(0.d0)
    call rhyme_irs_solve( irs, ig, L, R, dx, dt, hyid%x, U )

    call ig%prim_vars_to_cons( &
      solution%star%right%shock%rho, &
      solution%star%u, 0.d0, 0.d0, &
      solution%star%p, &
      ex_U &
    )

    call tester%expect( .notToBeNaN. U%u .hint. 'inside_right_shock' )
    call tester%expect( U%u .toBe. ex_U%u .hint. 'inside_right_shock' )
  else
    ! Right side of the right fan (outside the fan)
    dt = dx / solution%star%right%fan%speedH - epsilon(0.d0)
    call rhyme_irs_solve( irs, ig, L, R, dx, dt, hyid%x, U )

    call tester%expect( .notToBeNaN. U%u .hint. 'right_side_of_fan' )
    call tester%expect( U%u .toBe. R%u .hint. 'right_side_of_fan' )

    ! Left side of the right fan (outside the fan)
    dt = dx / solution%star%right%fan%speedT + 2 * epsilon(0.d0)
    call rhyme_irs_solve( irs, ig, L, R, dx, dt, hyid%x, U )

    call ig%prim_vars_to_cons( &
      solution%star%right%fan%rho, &
      solution%star%u, 0.d0, 0.d0, &
      solution%star%p, &
      ex_U &
    )

    call tester%expect( .notToBeNaN. U%u .hint. 'behind_right_fan' )
    call tester%expect( U%u .toBe. ex_U%u .hint. 'behind_right_fan' )

    ! Inside the right fan
    dt = ( &
      dx / solution%star%right%fan%speedH &
      + dx / solution%star%right%fan%speedT &
    ) / 2
    call rhyme_irs_solve( irs, ig, L, R, dx, dt, hyid%x, U )

    ex_U = irs_w_kfan( ig, solution%right, dx/dt, hyid%x, is_right=.true. )

    call tester%expect( .notToBeNaN. U%u .hint. 'inside_right_fan' )
    call tester%expect( U%u .toBe. ex_U%u .hint. 'inside_right_fan' )
  end if


  ! Testing the left side of the solution
  dx = -1.d0
  if ( solution%star%left%is_shock ) then
    ! Left side of the left shock (outside the shock)
    dt = dx / solution%star%left%shock%speed - epsilon(0.d0)
    call rhyme_irs_solve( irs, ig, L, R, dx, dt, hyid%x, U )

    call tester%expect( .notToBeNaN. U%u .hint. 'left_side_of_shock' )
    call tester%expect( U%u .toBe. L%u .hint. 'left_side_of_shock' )

    ! Inside the left shock
    dt = dx / solution%star%left%shock%speed + epsilon(0.d0)
    call rhyme_irs_solve( irs, ig, L, R, dx, dt, hyid%x, U )

    call ig%prim_vars_to_cons( &
      solution%star%left%shock%rho, &
      solution%star%u, 0.d0, 0.d0, &
      solution%star%p, &
      ex_U &
    )

    call tester%expect( .notToBeNaN. U%u .hint. 'inside_left_shock' )
    call tester%expect( U%u .toBe. ex_U%u .hint. 'inside_left_shock' )
  else
    ! Left side of the left fan (outside the fan)
    dt = dx / solution%star%left%fan%speedH - epsilon(0.d0)
    call rhyme_irs_solve( irs, ig, L, R, dx, dt, hyid%x, U )

    call tester%expect( .notToBeNaN. U%u .hint. 'left_side_of_fan' )
    call tester%expect( U%u .toBe. L%u .hint. 'left_side_of_fan' )

    ! Right side of the left fan (outside the fan)
    dt = dx / solution%star%left%fan%speedT + epsilon(0.d0)
    call rhyme_irs_solve( irs, ig, L, R, dx, dt, hyid%x, U )

    call ig%prim_vars_to_cons( &
      solution%star%left%fan%rho, &
      solution%star%u, 0.d0, 0.d0, &
      solution%star%p, &
      ex_U &
    )

    call tester%expect( .notToBeNaN. U%u .hint. 'behind_left_fan' )
    call tester%expect( U%u .toBe. ex_U%u .within. 14 .hint. 'behind_left_fan' )

    ! Inside the left fan
    dt = ( &
      dx / solution%star%left%fan%speedH &
      + dx / solution%star%left%fan%speedT &
    ) / 2
    call rhyme_irs_solve( irs, ig, L, R, dx, dt, hyid%x, U )

    ex_U = irs_w_kfan( ig, solution%left, dx/dt, hyid%x, is_right=.false. )

    call tester%expect( .notToBeNaN. U%u .hint. 'inside_left_fan' )
    call tester%expect( U%u .toBe. ex_U%u .hint. 'inside_left_fan' )
  end if
end subroutine irs_solve_test_cases
