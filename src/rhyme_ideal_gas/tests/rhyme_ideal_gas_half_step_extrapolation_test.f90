logical function rhyme_ideal_gas_half_step_extrapolation_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  type ( hydro_conserved_t ) :: exp_L, exp_R, Delta, L, R
  type ( hydro_flux_t ) :: FL, FR
  real ( kind=8 ) :: dt, dx

  call chemi%init
  call thermo%init
  call ig%init_with( chemi, thermo, gas_type )


  dx = 1.d0 / 1024
  dt = 1.d-5

  ! All zero Delta case
  Delta%u = [ 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 ]
  call ig%half_step_extrapolation(cons, Delta, hyid%x, dx, dt, L, R)

  failed = &
  any ( abs ( cons%u - L%u ) > epsilon(0.d0) ) &
  .or. any ( abs ( cons%u - R%u ) > epsilon(0.d0) )
  if ( failed ) return

  ! None zero delta case
  Delta%u = [ 1.d0, 0.d0, 0.d0, 0.d0, 0.d0 ]
  call ig%half_step_extrapolation(cons, Delta, hyid%x, dx, dt, L, R)

  exp_L%u = cons%u
  exp_L%u(hyid%rho) = exp_L%u(hyid%rho) - .5d0

  exp_R%u = cons%u
  exp_R%u(hyid%rho) = exp_R%u(hyid%rho) + .5d0

  call ig%flux_at ( exp_L, hyid%x, FL )
  call ig%flux_at ( exp_R, hyid%x, FR )

  exp_L%u = exp_L%u + .5d0 * dt / dx * ( FL%f - FR%f )
  exp_R%u = exp_R%u + .5d0 * dt / dx * ( FL%f - FR%f )

  failed = &
  any ( abs ( exp_L%u - L%u ) > epsilon(0.d0) ) &
  .or. any ( abs ( exp_R%u - R%u ) > epsilon(0.d0) )
end function rhyme_ideal_gas_half_step_extrapolation_test
