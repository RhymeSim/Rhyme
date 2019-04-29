logical function rhyme_ideal_gas_specific_internal_energy_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  real ( kind=8 ) :: e_int_sp

  ig_tester = .describe. "ideal_gas e_int_sp"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  e_int_sp = hy%p / hy%rho / ( ig%gamma - 1 )

  call ig_tester%expect( ig%e_int_sp( hy%cons ) .toBe. hy%e_int / hy%rho .within. 15 )
  call ig_tester%expect( ig%e_int_sp( hy%cons ) .toBe. hy%e_int_sp .within. 15 )
  call ig_tester%expect( ig%e_int_sp( hy%cons ) .toBe. hy_sp_internal_e( hy%cons ) .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_specific_internal_energy_test
