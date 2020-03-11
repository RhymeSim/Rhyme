submodule(rhyme_thermo_base) primitive_to_conserved_smod
contains
pure module subroutine rhyme_thermo_base_primitive_to_conserved(w, u)
   implicit none

   real(kind=8), intent(in) :: w(cid%rho:cid%p)
   real(kind=8), intent(out) :: u(cid%rho:cid%e_tot)

   call rhyme_ideal_gas_primitive_to_conserved(rhyme_thermo_base_get_gamma(), w, u)
end subroutine rhyme_thermo_base_primitive_to_conserved
end submodule primitive_to_conserved_smod
