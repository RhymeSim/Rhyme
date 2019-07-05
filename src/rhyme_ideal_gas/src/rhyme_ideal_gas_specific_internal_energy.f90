submodule ( rhyme_ideal_gas ) specific_internal_energy_smod
contains
  pure module function rhyme_ideal_gas_specific_internal_energy ( gamma, kb_amu, u ) result ( sp_int_e )
    implicit none

    real ( kind=8 ), intent ( in ) :: gamma, kb_amu, u( cid%rho:cid%e_tot )
    real ( kind=8 ) :: sp_int_e

    sp_int_e = kb_amu * rhyme_ideal_gas_temperature_per_mu( gamma, kb_amu, u ) &
      / ( gamma - 1 )
  end function rhyme_ideal_gas_specific_internal_energy
end submodule specific_internal_energy_smod
