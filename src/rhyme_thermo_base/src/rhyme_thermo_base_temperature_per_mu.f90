submodule ( rhyme_thermo_base ) temperature_per_mu_smod
contains
  pure module function rhyme_thermo_base_temperature_per_mu ( u ) result ( t_mu )
    implicit none

    real ( kind=8 ), intent ( in ) :: u( cid%rho:cid%e_tot )
    real ( kind=8 ) :: t_mu

    t_mu = rhyme_ideal_gas_temperature_per_mu( rhyme_thermo_base_get_gamma(), u )
  end function rhyme_thermo_base_temperature_per_mu
end submodule temperature_per_mu_smod
