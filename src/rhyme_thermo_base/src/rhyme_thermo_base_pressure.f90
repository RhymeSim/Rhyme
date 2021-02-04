submodule(rhyme_thermo_base) pressure_smod
contains
   pure module function rhyme_thermo_base_pressure(u) result(p)
      implicit none

      real(kind=8), intent(in) :: u(cid%rho:cid%e_tot)
      real(kind=8) :: p

      p = rhyme_ideal_gas_pressure(rhyme_thermo_base_get_gamma(), &
                                   rhyme_thermo_base_kb_amu, u)
   end function rhyme_thermo_base_pressure
end submodule pressure_smod
