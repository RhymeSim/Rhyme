logical function rhyme_ideal_gas_half_step_extrapolation_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type (hydro_conserved_t) :: new_cons, Delta, L, R
  real(kind=8) :: dt, dx

  call chemi%init
  call ig%init_with (chemi, gas_type)

  new_cons%u = cons%u
  Delta%u = [0.d0, 0.d0, 0.d0, 0.d0, 0.d0]

  dx = 1.d0 / 1024
  dt = 1.d-5

  call ig%half_step_extrapolation(new_cons, Delta, hyid%x, dx, dt, L, R)

  failed = .true.
end function rhyme_ideal_gas_half_step_extrapolation_test
