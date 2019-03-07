logical function rhyme_ideal_gas_sound_speed_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  failed = abs( ig%Cs( hy%cons ) - sqrt( ig%gamma * hy%p / hy%rho) ) > epsilon(0.e0)
end function rhyme_ideal_gas_sound_speed_test
