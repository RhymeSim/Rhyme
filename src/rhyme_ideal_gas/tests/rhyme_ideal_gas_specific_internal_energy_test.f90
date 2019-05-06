logical function rhyme_ideal_gas_specific_internal_energy_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  real ( kind=8 ) :: e_int_sp

  ig_tester = .describe. "ideal_gas e_int_sp"

  call rhyme_ideal_gas_factory_init

  ig%type = ig_gas_type
  call rhyme_ideal_gas_init( ig, ig_chemi, ig_thermo, ig_units, ig_logger )

  e_int_sp = ig_hy%p / ig_hy%rho / ( ig%gamma - 1 )

  call ig_tester%expect( ig%e_int_sp( ig_hy%cons ) .toBe. ig_hy%e_int / ig_hy%rho .within. 15 )
  call ig_tester%expect( ig%e_int_sp( ig_hy%cons ) .toBe. ig_hy%e_int_sp .within. 15 )
  call ig_tester%expect( ig%e_int_sp( ig_hy%cons ) .toBe. hy_sp_internal_e( ig_hy%cons ) .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_specific_internal_energy_test
