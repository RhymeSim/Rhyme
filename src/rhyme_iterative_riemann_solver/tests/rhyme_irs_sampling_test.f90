logical function rhyme_irs_sampling_test () result ( failed )
  use rhyme_iterative_riemann_solver_factory

  implicit none

  type ( hydro_conserved_t ) :: L, R, U
  type ( rp_star_region_t ) :: star

  integer :: i
  real ( kind=8 ) :: x, rho_, v, p, e_int

  call rhyme_iterative_riemann_solver_factory_init

  failed = .true.

  call irs_Sod_test( ig, L, R, star )
  call irs%solve( ig, L, R, hyid%x, star )

  open ( unit=1, file="./sod_shock_tube_t_0_2_analytical.txt", action='read', form='formatted' )
  do i = 1, 12
    read (1, *)
  end do

  do i = 1, 500
    read (1, *) x, rho_, v, p
    e_int = p / 0.4d0

    call irs%sampling( ig, L, R, star, hyid%x, -.5d0 + real(i - 1, kind=8) / 499.d0, .2d0, U )

    failed = &
    abs( rho_ - u%u(hyid%rho) ) > epsilon(0.e0) &
    .or. abs( rho_ * v - U%u(hyid%rho_u) ) > epsilon(0.e0) &
    .or. abs( p - ig%p(U) ) > epsilon(0.e0) &
    .or. abs( e_int - U%u(hyid%rho) * ig%e_int_sp(U) ) > epsilon(0.e0)
    if ( failed ) then
      close (1)
      return
    end if
  end do

  close (1)
end function rhyme_irs_sampling_test
