submodule ( rhyme_ideal_gas ) speed_of_sound_smod
contains
  pure module function rhyme_ideal_gas_speed_of_sound ( gamma, kb_amu, u ) result ( cs )
    implicit none

    real ( kind=8 ), intent ( in ) :: gamma, kb_amu, u( cid%rho:cid%e_tot )
    real ( kind=8 ) :: cs

    cs = sqrt( gamma * kb_amu * rhyme_ideal_gas_temperature_per_mu( gamma, kb_amu, u ) )
  end function rhyme_ideal_gas_speed_of_sound
end submodule speed_of_sound_smod
