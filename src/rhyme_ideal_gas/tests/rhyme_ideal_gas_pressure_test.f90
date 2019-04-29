logical function rhyme_ideal_gas_pressure_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig

  ig_tester = .describe. "ideal_gas gas_pressure"

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  call ig_tester%expect( ig%p( hy%cons ) .toBe. hy%p .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_pressure_test
