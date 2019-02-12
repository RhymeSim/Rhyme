logical function rhyme_ideal_gas_specific_kinetic_energy_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  real(kind=8) :: hy_sp_kin_e

  call ig%init_with ( gas_type )

  hy_sp_kin_e = hy_sp_kinetic_e(cons)

  failed = &
  abs ( ig%e_kin_sp(cons) - e_kin_sp ) > epsilon(0.e0) &
  .or. abs ( ig%e_kin_sp(cons) - hy_sp_kin_e ) > epsilon(0.e0)
end function rhyme_ideal_gas_specific_kinetic_energy_test
