logical function rhyme_ideal_gas_bulk_modulus_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig

  ig_tester = .describe. "ideal_gas bulk_modulus"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  call ig_tester%expect( ig%B( hy%cons ) .toBe. ig%gamma * hy%p .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_bulk_modulus_test
