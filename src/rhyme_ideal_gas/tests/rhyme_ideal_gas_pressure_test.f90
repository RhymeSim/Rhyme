logical function rhyme_ideal_gas_pressure_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  call chemi%init
  call ig%init_with (chemi, gas_type)

  failed = abs( ig%p(cons) - p ) > epsilon(0.d0)
end function rhyme_ideal_gas_pressure_test
