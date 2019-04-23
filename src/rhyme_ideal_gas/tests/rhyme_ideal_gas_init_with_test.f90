logical function rhyme_ideal_gas_init_with_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig

  ig_tester = .describe. "ideal_gas init_with"

  call ig%init_with ( chemi, thermo, igid%diatomic, log )

  call ig_tester%expect( ig%type .toBe. igid%diatomic )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_init_with_test
