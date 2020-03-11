submodule(rhyme_hydro_base) specific_kinetic_energy_smod
contains
pure module function rhyme_hydro_base_specific_kinetic_energy(u) result(sp_kin_e)
   implicit none

   real(kind=8), intent(in) :: u(cid%rho:cid%e_tot)
   real(kind=8) :: sp_kin_e

   sp_kin_e = .5d0*sum(u(cid%rho_u:cid%rho_u + NDIM - 1)**2)/u(cid%rho)**2
end function rhyme_hydro_base_specific_kinetic_energy
end submodule specific_kinetic_energy_smod
