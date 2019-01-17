logical function rhyme_irs_sampling_test () result (failed)
  use rhyme_iterative_riemann_solver_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: L, R, U
  type ( iterative_riemann_solver_config_t ) :: irs_config
  type ( rp_star_region_t ) :: star

  integer :: i
  logical :: passed
  real(kind=8) :: x, rho_, v, p, e_int

  call ig%init_with ( igid%diatomic )

  call irs_Sod_test (L, R, star)
  call iterative_riemann_solver (ig, L, R, hyid%x, irs_config, star)

  failed = .false.

  open ( unit=1, file="./sod_shock_tube_t_0_2_analytical.txt", action='read', form='formatted' )
  do i = 1, 12
    read (1, *)
  end do

  do i = 1, 500
    read (1, *) x, rho_, v, p
    e_int = p / 0.4d0
    call irs_sampling ( ig, L, R, star, hyid%x, -.5d0 + real(i - 1, kind=8) / 499.d0, .2d0, U )

    passed = abs ( rho_ - u%u(hyid%rho) ) < epsilon(0.e0) &
    .and. abs ( rho_ * v - U%u(hyid%rho_u) ) < epsilon(0.e0) &
    .and. abs ( p - ig%p(U) ) < epsilon(0.e0) &
    .and. abs ( e_int - U%u(hyid%rho) * ig%e_int_sp(U) ) < epsilon(0.e0)

    if ( .not. passed ) failed = .true.
  end do

  close (1)
end function rhyme_irs_sampling_test
