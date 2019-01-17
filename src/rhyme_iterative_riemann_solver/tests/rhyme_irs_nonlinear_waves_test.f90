logical function rhyme_irs_nonlinear_waves_test () result (failed)
  use rhyme_iterative_riemann_solver

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( iterative_riemann_solver_config_t ) :: irs_config
  real(kind=8) :: f, fprime, prev_f, prev_fprime
  real(kind=8) :: rho = 1.23d3, p, p_star = 3.45d2
  integer :: i

  p = irs_config%pressure_floor

  failed = .false.


  call ig%init_with ( igid%monatomic )
  call random_seed

  call irs_nonlinear_waves(ig, rho, p, p_star, prev_f, prev_fprime)

  do i = 1, 800
    p = 2.34d0 * p
    call irs_nonlinear_waves(ig, rho, p, p_star, f, fprime)

    if ( fprime < 0.0 .or. prev_fprime - fprime > irs_config%tolerance * fprime  .or. f - prev_f > irs_config%tolerance * f ) then
      failed = .true.
    end if

    prev_f = f
    prev_fprime = fprime
  end do
end function rhyme_irs_nonlinear_waves_test
