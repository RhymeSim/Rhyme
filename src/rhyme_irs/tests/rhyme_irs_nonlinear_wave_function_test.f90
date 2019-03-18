logical function rhyme_irs_nonlinear_wave_function_test () result (failed)
  use rhyme_irs_factory

  implicit none

  type ( rp_side_t ) :: state
  type ( rp_star_side_t ) :: star, prev_star
  real(kind=8) :: p, p_star = 3.45d2
  integer :: i


  call rhyme_irs_factory_init

  state%rho = 1.23d3
  state%p = irs_fac%pressure_floor

  call rhyme_irs_nonlinear_wave_function( irs_fac_ig, state, p_star, prev_star )

  do i = 1, 800
    p = 2.34d0 * p

    call rhyme_irs_nonlinear_wave_function( irs_fac_ig, state, p_star, star )

    failed = star%fprime < 0.0 &
    .or. prev_star%fprime - star%fprime > irs_fac%tolerance * star%fprime &
    .or. star%f - prev_star%f > irs_fac%tolerance * star%f
    if ( failed ) return

    prev_star%f = star%f
    prev_star%fprime = star%fprime
  end do
end function rhyme_irs_nonlinear_wave_function_test
