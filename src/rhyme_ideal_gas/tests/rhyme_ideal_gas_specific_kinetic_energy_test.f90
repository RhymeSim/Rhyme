logical function rhyme_ideal_gas_specific_kinetic_energy_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  real(kind=8) :: hy_e_kin_sp

  call chemi%init
  call ig%init_with (chemi, gas_type)

  hy_e_kin_sp = hy_specific_kinetic_energy(cons)

  failed = &
  abs(ig%e_kin_sp(cons) - e_kin_sp) > epsilon(0.e0) &
  .or. abs(ig%e_kin_sp(cons) - hy_e_kin_sp) > epsilon(0.e0)
end function rhyme_ideal_gas_specific_kinetic_energy_test
