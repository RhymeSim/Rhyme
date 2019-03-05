logical function rhyme_ideal_gas_sound_speed_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo

  call chemi%init
  call thermo%init
  call ig%init_with( chemi, thermo, gas_type )

  failed = abs( ig%Cs(cons) - sqrt(gamma * p / rho) ) > epsilon(0.e0)
end function rhyme_ideal_gas_sound_speed_test
