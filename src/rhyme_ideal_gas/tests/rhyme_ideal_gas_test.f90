logical function rhyme_ideal_gas_test () result ( failed )
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  ig_tester = .describe. "rhyme_ideal_gas"

  failed = ig_tester%failed()
end function rhyme_ideal_gas_test
