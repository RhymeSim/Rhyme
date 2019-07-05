submodule ( rhyme_thermo_base ) specific_internal_energy_smod
contains
  pure module function rhyme_thermo_base_specific_internal_energy ( u ) result ( sp_int_e )
    implicit none

    real ( kind=8 ), intent ( in ) :: u( cid%rho:cid%e_tot )
    real ( kind=8 ) :: sp_int_e

    sp_int_e = rhyme_ideal_gas_specific_internal_energy( &
      rhyme_thermo_base_get_gamma(), rhyme_thermo_base_kb_amu, u )
  end function rhyme_thermo_base_specific_internal_energy
end submodule specific_internal_energy_smod
