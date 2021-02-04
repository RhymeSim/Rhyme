submodule(rhyme_thermo_base) flux_smod
contains
   pure module function rhyme_thermo_base_flux(u, axis) result(f)
      implicit none

      real(kind=8), intent(in) :: u(cid%rho:cid%e_tot)
      integer, intent(in) :: axis
      real(kind=8) :: f(cid%rho:cid%e_tot)

      call rhyme_ideal_gas_flux( &
         rhyme_thermo_base_get_gamma(), &
         rhyme_thermo_base_kb_amu, u, axis, f)
   end function rhyme_thermo_base_flux
end submodule flux_smod
