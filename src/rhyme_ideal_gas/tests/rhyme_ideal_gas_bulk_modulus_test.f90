logical function rhyme_ideal_gas_bulk_modulus_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig

  ig_tester = .describe. "ideal_gas bulk_modulus"

  call rhyme_ideal_gas_factory_init

  ig%type = ig_gas_type
  call rhyme_ideal_gas_init( ig, ig_chemi, ig_thermo, ig_units, ig_logger )

  call ig_tester%expect( ig%B( ig_hy%cons ) .toBe. ig%gamma * ig_hy%p .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_bulk_modulus_test
