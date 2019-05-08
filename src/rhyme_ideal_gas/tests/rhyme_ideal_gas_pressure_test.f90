logical function rhyme_ideal_gas_pressure_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons

  ig_tester = .describe. "ideal_gas gas_pressure"

  cons = hy_factory%conserved()
  ig = ig_factory%generate()

  call ig_tester%expect( ig%p( cons ) .toBe. hy_factory%p .within. 15 )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_pressure_test
