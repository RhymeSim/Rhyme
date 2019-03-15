logical function rhyme_irs_sampling_test () result ( failed )
  use rhyme_irs_factory

  implicit none

  type ( hydro_conserved_t ) :: L, R, U
  type ( riemann_problem_solution_t ) :: solution

  integer :: i
  real ( kind=8 ) :: x, rho_, v, p, e_int

  call rhyme_irs_factory_init

  failed = .true.

  call irs_Sod_test( irs_fac_ig, L, R, solution )
  call rhyme_irs_solve( irs_fac, irs_fac_ig, L, R, hyid%x, solution )

  open ( unit=1, file="./sod_shock_tube_t_0_2_analytical.txt", action='read', form='formatted' )
  do i = 1, 12
    read (1, *)
  end do

  do i = 1, 500
    read (1, *) x, rho_, v, p
    e_int = p / 0.4d0

    call rhyme_irs_sampling( irs_fac_ig, L, R, &
      solution, hyid%x, -.5d0 + real(i - 1, kind=8) / 499.d0, .2d0, U )

    failed = &
    abs( rho_ - u%u(hyid%rho) ) > epsilon(0.e0) &
    .or. abs( rho_ * v - U%u(hyid%rho_u) ) > epsilon(0.e0) &
    .or. abs( p - irs_fac_ig%p(U) ) > epsilon(0.e0) &
    .or. abs( e_int - U%u(hyid%rho) * irs_fac_ig%e_int_sp(U) ) > epsilon(0.e0)
    if ( failed ) then
      close (1)
      return
    end if
  end do

  close (1)
end function rhyme_irs_sampling_test
