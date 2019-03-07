logical function rhyme_ideal_gas_specific_kinetic_energy_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  real ( kind=8 ) :: hy_sp_kin_e

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  hy_sp_kin_e = hy_sp_kinetic_e( hy%cons )

  failed = &
  abs ( ig%e_kin_sp( hy%cons ) - hy%e_kin_sp ) > epsilon(0.e0) &
  .or. abs ( ig%e_kin_sp( hy%cons ) - hy_sp_kin_e ) > epsilon(0.e0)
end function rhyme_ideal_gas_specific_kinetic_energy_test
