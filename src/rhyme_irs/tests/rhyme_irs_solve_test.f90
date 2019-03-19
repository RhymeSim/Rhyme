logical function rhyme_irs_solve_test () result ( failed )
  use rhyme_irs_factory

  implicit none

  logical :: failed_sod, failed_123, failed_left, failed_right, failed_shocks

  call rhyme_irs_factory_init

  ! Non-vacuum cases
  call irs_solve_test_cases( irs_Sod_test, failed_sod )
  call irs_solve_test_cases( irs_123_test, failed_123 )
  call irs_solve_test_cases( irs_left_blast_wave_test, failed_left )
  call irs_solve_test_cases( irs_right_blast_wave_test, failed_right )
  call irs_solve_test_cases( irs_two_shocks_collision_test, failed_shocks )

  failed = failed_sod .or. failed_123 .or. failed_left .or. failed_right .or. failed_shocks
  if ( failed ) return

  ! TODO: vacuum cases
end function rhyme_irs_solve_test


subroutine irs_solve_test_cases ( func, failed )
  use rhyme_irs_factory

  implicit none

  external :: func
  logical :: failed

  type ( hydro_conserved_t ) :: L, R, U, ex_U
  type ( riemann_problem_solution_t ) :: solution
  real ( kind=8 ) :: dx, dt

  call func( irs_fac_ig, L, R, solution )
  call rhyme_irs_iterate( irs_fac, irs_fac_ig, solution, hyid%x )


  ! Testing the right side of the solution
  dx = 1.d0
  if ( solution%star%right%is_shock ) then
    ! Right side of the right shock (outside the shock)
    dt = dx / solution%star%right%shock%speed - epsilon(0.d0)
    call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, dx, dt, hyid%x, U )

    failed = any( abs( U%u - R%u ) > epsilon(0.e0) )
    if ( failed ) return

    ! Inside the right shock
    dt = dx / solution%star%right%shock%speed + epsilon(0.d0)
    call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, dx, dt, hyid%x, U )

    call irs_fac_ig%prim_vars_to_cons( &
      solution%star%right%shock%rho, &
      solution%star%u, 0.d0, 0.d0, &
      solution%star%p, &
      ex_U &
    )

    failed = any( abs( U%u - ex_U%u ) > epsilon(0.e0) )
    if ( failed ) return
  else
    ! Right side of the right fan (outside the fan)
    dt = dx / solution%star%right%fan%speedH - epsilon(0.d0)
    call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, dx, dt, hyid%x, U )

    failed = any( abs( U%u - R%u ) > epsilon(0.e0) )
    if ( failed ) return

    ! Left side of the right fan (outside the fan)
    dt = dx / solution%star%right%fan%speedT + epsilon(0.d0)
    call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, dx, dt, hyid%x, U )

    call irs_fac_ig%prim_vars_to_cons( &
      solution%star%right%fan%rho, &
      solution%star%u, 0.d0, 0.d0, &
      solution%star%p, &
      ex_U &
    )

    failed = any( abs( U%u - ex_U%u ) > epsilon(0.e0) )
    if ( failed ) return

    ! Inside the right fan
    dt = ( &
      dx / solution%star%right%fan%speedH &
      + dx / solution%star%right%fan%speedT &
    ) / 2
    call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, dx, dt, hyid%x, U )

    ex_U = irs_w_kfan( irs_fac_ig, solution%right, dx/dt, hyid%x, is_right=.true. )

    failed = any( abs( U%u - ex_U%u ) > epsilon(0.e0) )
    if ( failed ) return
  end if


  ! Testing the left side of the solution
  dx = -1.d0
  if ( solution%star%left%is_shock ) then
    ! Left side of the left shock (outside the shock)
    dt = dx / solution%star%left%shock%speed - epsilon(0.d0)
    call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, dx, dt, hyid%x, U )

    failed = any( abs( U%u - L%u ) > epsilon(0.e0) )
    if ( failed ) return

    ! Inside the left shock
    dt = dx / solution%star%left%shock%speed + epsilon(0.d0)
    call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, dx, dt, hyid%x, U )

    call irs_fac_ig%prim_vars_to_cons( &
      solution%star%left%shock%rho, &
      solution%star%u, 0.d0, 0.d0, &
      solution%star%p, &
      ex_U &
    )

    failed = any( abs( U%u - ex_U%u ) > epsilon(0.e0) )
    if ( failed ) return
  else
    ! Left side of the left fan (outside the fan)
    dt = dx / solution%star%left%fan%speedH - epsilon(0.d0)
    call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, dx, dt, hyid%x, U )

    failed = any( abs( U%u - L%u ) > epsilon(0.e0) )
    if ( failed ) return

    ! Right side of the left fan (outside the fan)
    dt = dx / solution%star%left%fan%speedT + epsilon(0.d0)
    call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, dx, dt, hyid%x, U )

    call irs_fac_ig%prim_vars_to_cons( &
      solution%star%left%fan%rho, &
      solution%star%u, 0.d0, 0.d0, &
      solution%star%p, &
      ex_U &
    )

    failed = any( abs( U%u - ex_U%u ) > epsilon(0.e0) )
    if ( failed ) return

    ! Inside the left fan
    dt = ( &
      dx / solution%star%left%fan%speedH &
      + dx / solution%star%left%fan%speedT &
    ) / 2
    call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, dx, dt, hyid%x, U )

    ex_U = irs_w_kfan( irs_fac_ig, solution%left, dx/dt, hyid%x, is_right=.false. )

    failed = any( abs( U%u - ex_U%u ) > epsilon(0.e0) )
    if ( failed ) return
  end if
end subroutine irs_solve_test_cases
