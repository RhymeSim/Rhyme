submodule(rhyme_thermo_base) conserved_to_primitive_smod
contains
   pure module subroutine rhyme_thermo_base_conserved_to_primitive(u, w)
      implicit none

      real(kind=8), intent(in) :: u(cid%rho:cid%p)
      real(kind=8), intent(out) :: w(cid%rho:cid%e_tot)

      call rhyme_ideal_gas_conserved_to_primitive(rhyme_thermo_base_get_gamma(), u, w)
   end subroutine rhyme_thermo_base_conserved_to_primitive
end submodule conserved_to_primitive_smod
