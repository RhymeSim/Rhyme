logical function rhyme_ideal_gas_pressure_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  failed = abs( ig%p( hy%cons ) - hy%p ) > epsilon(0.d0)
end function rhyme_ideal_gas_pressure_test
