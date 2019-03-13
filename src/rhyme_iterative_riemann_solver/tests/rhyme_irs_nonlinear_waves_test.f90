logical function rhyme_irs_nonlinear_waves_test () result (failed)
  use rhyme_iterative_riemann_solver_factory

  implicit none

  real(kind=8) :: f, fprime, prev_f, prev_fprime
  real(kind=8) :: rho = 1.23d3, p, p_star = 3.45d2
  integer :: i


  call rhyme_iterative_riemann_solver_factory_init

  p = irs%pressure_floor

  call rhyme_iterative_riemann_solver_nonlinear_waves( ig, rho, p, p_star, prev_f, prev_fprime )

  do i = 1, 800
    p = 2.34d0 * p

    call rhyme_iterative_riemann_solver_nonlinear_waves( ig, rho, p, p_star, f, fprime )

    failed = fprime < 0.0 &
    .or. prev_fprime - fprime > irs%tolerance * fprime &
    .or. f - prev_f > irs%tolerance * f
    if ( failed ) return

    prev_f = f
    prev_fprime = fprime
  end do
end function rhyme_irs_nonlinear_waves_test
