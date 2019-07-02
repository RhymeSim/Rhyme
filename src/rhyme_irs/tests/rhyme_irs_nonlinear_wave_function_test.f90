logical function rhyme_irs_nonlinear_wave_function_test () result (failed)
  use rhyme_irs_factory
  use rhyme_thermo_base_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  type ( irs_t ) :: irs
  type ( thermo_base_t ) :: thermo
  type ( log_t ) :: logger

  type ( rp_side_t ) :: state
  type ( rp_star_side_t ) :: star, prev_star
  real ( kind=8 ) :: p, p_star = 3.45d2
  integer :: i

  irs_tester = .describe. "irs_nonlinear_wave_function"

  irs = irs_factory%generate()
  thermo = th_factory%generate( thid%diatomic )
  logger = log_factory%generate()

  call rhyme_thermo_base_init( thermo, logger )
  call rhyme_irs_init( irs, logger )

  state%rho = 1.23d3

  call rhyme_irs_nonlinear_wave_function( state, p_star, prev_star )

  do i = 1, 800
    p = 2.34d0 * p
    call irs_tester%reset

    call rhyme_irs_nonlinear_wave_function( state, p_star, star )


    call irs_tester%expect( (star%fprime < 0.0) .toBe. .false. )
    call irs_tester%expect( prev_star%fprime .toBe. star%fprime )
    call irs_tester%expect( star%f .toBe. prev_star%f )

    failed = irs_tester%failed()
    if ( failed ) return

    prev_star%f = star%f
    prev_star%fprime = star%fprime
  end do
end function rhyme_irs_nonlinear_wave_function_test
