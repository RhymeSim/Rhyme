logical function rhyme_ideal_gas_specific_internal_energy_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  real ( kind=8 ) :: e_int_sp

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  e_int_sp = hy%p / hy%rho / ( ig%gamma - 1 )

  failed = &
  abs ( ig%e_int_sp( hy%cons ) - hy%e_int / hy%rho ) > epsilon(0.e0) &
  .or. abs ( ig%e_int_sp( hy%cons ) - hy%e_int_sp ) > epsilon(0.e0) &
  .or. abs ( ig%e_int_sp( hy%cons ) - hy_sp_internal_e( hy%cons ) ) > epsilon(0.e0)
end function rhyme_ideal_gas_specific_internal_energy_test
