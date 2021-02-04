submodule(rhyme_hydro_base) specific_internal_energy_smod
contains
   pure module function rhyme_hydro_base_specific_internal_energy(u) result(sp_int_e)
      implicit none

      real(kind=8), intent(in) :: u(cid%rho:cid%e_tot)
      real(kind=8) :: sp_int_e

      sp_int_e = u(cid%e_tot)/u(cid%rho) - rhyme_hydro_base_specific_kinetic_energy(u)
   end function rhyme_hydro_base_specific_internal_energy
end submodule specific_internal_energy_smod
