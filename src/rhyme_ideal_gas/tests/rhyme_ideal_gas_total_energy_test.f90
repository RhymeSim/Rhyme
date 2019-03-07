logical function rhyme_ideal_gas_total_energy_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  failed = &
  abs ( hy%e_tot - hy%rho &
    * ( ig%e_kin_sp( hy%cons ) + ig%e_int_sp( hy%cons ) ) &
  ) > epsilon(0.e0)
end function rhyme_ideal_gas_total_energy_test
