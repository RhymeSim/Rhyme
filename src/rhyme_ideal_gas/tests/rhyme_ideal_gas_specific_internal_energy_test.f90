logical function rhyme_ideal_gas_specific_internal_energy_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  real(kind=8) :: e_int_sp

  call chemi%init
  call thermo%init
  call ig%init_with( chemi, thermo, gas_type )

  e_int_sp = p / rho / (gamma - 1)

  failed = &
  abs ( ig%e_int_sp(cons) - e_int / rho ) > epsilon(0.e0) &
  .or. abs ( ig%e_int_sp(cons) - e_int_sp ) > epsilon(0.e0) &
  .or. abs ( ig%e_int_sp(cons) - hy_sp_internal_e(cons) ) > epsilon(0.e0)
end function rhyme_ideal_gas_specific_internal_energy_test
