logical function rhyme_ideal_gas_total_energy_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo

  call chemi%init
  call thermo%init
  call ig%init_with( chemi, thermo, gas_type )

  failed = &
  abs ( e_tot - rho * (ig%e_kin_sp(cons) + ig%e_int_sp(cons)) ) > epsilon(0.e0)
end function rhyme_ideal_gas_total_energy_test
