logical function rhyme_ideal_gas_total_energy_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  call chemi%init
  call ig%init_with ( chemi, gas_type )

  failed = &
  abs ( e_tot - rho * (ig%e_kin_sp(cons) + ig%e_int_sp(cons)) ) > epsilon(0.e0)
end function rhyme_ideal_gas_total_energy_test
