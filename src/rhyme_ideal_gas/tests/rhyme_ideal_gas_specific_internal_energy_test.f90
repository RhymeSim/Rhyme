logical function rhyme_ideal_gas_specific_internal_energy_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  real(kind=8) :: e_int_sp

  call chemi%init
  call ig%init_with (chemi, gas_type)

  e_int_sp = p / rho / (gamma - 1)

  failed = &
  abs ( ig%e_int_sp(cons) - e_int / rho ) > epsilon(0.e0) &
  .or. abs ( ig%e_int_sp(cons) - e_int_sp ) > epsilon(0.e0) &
  .or. abs ( ig%e_int_sp(cons) - hy_specific_internal_energy(cons) ) > epsilon(0.e0)
end function rhyme_ideal_gas_specific_internal_energy_test
