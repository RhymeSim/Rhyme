submodule ( rhyme_thermo_base ) speed_of_sound_smod
contains
  pure module function rhyme_thermo_base_speed_of_sound ( u ) result ( cs )
    implicit none

    real ( kind=8 ), intent ( in ) :: u( cid%rho:cid%e_tot )
    real ( kind=8 ) :: cs

    cs = rhyme_ideal_gas_speed_of_sound( rhyme_thermo_base_get_gamma(), u )
  end function rhyme_thermo_base_speed_of_sound
end submodule speed_of_sound_smod
