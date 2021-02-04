submodule(rhyme_thermo_base) primitive_vars_to_conserved_smod
contains
   pure module subroutine rhyme_thermo_base_primitive_vars_to_conserved(rho, v, p, u)
      implicit none

      real(kind=8), intent(in) :: rho
      real(kind=8), intent(in) :: v(NDIM)
      real(kind=8), intent(in) :: p
      real(kind=8), intent(out) :: u(cid%rho:cid%e_tot)

      call rhyme_ideal_gas_primitive_vars_to_conserved( &
         rhyme_thermo_base_get_gamma(), rho, v, p, u)
   end subroutine rhyme_thermo_base_primitive_vars_to_conserved
end submodule primitive_vars_to_conserved_smod
