logical function rhyme_ideal_gas_sound_speed_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  call chemi%init
  call ig%init_with (chemi, gas_type)

  failed = abs(ig%Cs(cons) - sqrt(gamma * p / rho)) > epsilon(0.e0)
end function rhyme_ideal_gas_sound_speed_test
