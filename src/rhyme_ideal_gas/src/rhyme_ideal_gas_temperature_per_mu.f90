submodule ( rhyme_ideal_gas ) temperature_per_mu_smod
contains
  pure module function rhyme_ideal_gas_temperature_per_mu ( gamma, kb_amu, u ) result ( t_mu )
    implicit none

    real ( kind=8 ), intent ( in ) :: gamma, kb_amu, u( cid%rho:cid%e_tot )
    real ( kind=8 ) :: t_mu

    t_mu = rhyme_hydro_base_specific_internal_energy( u ) * ( gamma - 1 ) &
      / kb_amu
  end function rhyme_ideal_gas_temperature_per_mu
end submodule temperature_per_mu_smod
