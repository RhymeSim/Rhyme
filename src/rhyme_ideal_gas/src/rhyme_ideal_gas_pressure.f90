submodule ( rhyme_ideal_gas ) pressure_smod
contains
  pure module function rhyme_ideal_gas_pressure ( gamma, u ) result ( p )
    implicit none

    real ( kind=8 ), intent ( in ) :: gamma, u( cid%rho:cid%e_tot )
    real ( kind=8 ) :: p

    p = u( cid%rho ) * kb_amu * rhyme_ideal_gas_temperature_per_mu( gamma, u )
  end function rhyme_ideal_gas_pressure
end submodule pressure_smod
