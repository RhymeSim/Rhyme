logical function rhyme_ideal_gas_half_step_extrapolation_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons, exp_L, exp_R, Delta, L, R
  type ( hydro_flux_t ) :: FL, FR
  real ( kind=8 ) :: dt, dx

  ig_tester = .describe. "ideal_gas half_step_extrapolation"

  cons = hy_factory%conserved()
  ig = ig_factory%generate()

  dx = 1.d0 / 1024
  dt = 1.d-5

  ! All zero Delta case
  Delta%u = [ 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 ]
  call ig%half_step_extrapolation( cons, Delta, hyid%x, dx, dt, L, R )

  call ig_tester%expect( cons%u .toBe. L%u )
  call ig_tester%expect( cons%u .toBe. R%u )

  ! None zero delta case
  Delta%u = [ 1.d0, 0.d0, 0.d0, 0.d0, 0.d0 ]
  call ig%half_step_extrapolation( cons, Delta, hyid%x, dx, dt, L, R )

  exp_L%u = cons%u
  exp_L%u(hyid%rho) = exp_L%u(hyid%rho) - .5d0

  exp_R%u = cons%u
  exp_R%u(hyid%rho) = exp_R%u(hyid%rho) + .5d0

  call ig%flux_at( exp_L, hyid%x, FL )
  call ig%flux_at( exp_R, hyid%x, FR )

  exp_L%u = exp_L%u + .5d0 * dt / dx * ( FL%f - FR%f )
  exp_R%u = exp_R%u + .5d0 * dt / dx * ( FL%f - FR%f )

  call ig_tester%expect( exp_L%u .toBe. L%u )
  call ig_tester%expect( exp_R%u .toBe. R%u )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_half_step_extrapolation_test
