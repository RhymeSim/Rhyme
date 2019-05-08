logical function rhyme_ideal_gas_specific_internal_energy_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons
  real ( kind=8 ) :: e_int_sp

  ig_tester = .describe. "ideal_gas e_int_sp"

  cons = hy_factory%conserved()
  ig = ig_factory%generate()

  e_int_sp = hy_factory%p / hy_factory%rho / ( ig%gamma - 1 )

  call ig_tester%expect( ig%e_int_sp( cons ) .toBe. hy_factory%e_int / hy_factory%rho .within. 15 )
  call ig_tester%expect( ig%e_int_sp( cons ) .toBe. hy_factory%e_int_sp .within. 15 )
  call ig_tester%expect( ig%e_int_sp( cons ) .toBe. hy_sp_internal_e( cons ) .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_specific_internal_energy_test
