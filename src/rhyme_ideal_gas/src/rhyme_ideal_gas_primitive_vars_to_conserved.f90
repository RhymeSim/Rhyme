submodule(rhyme_ideal_gas) primitive_vars_to_conserved_smod
contains
   pure module subroutine rhyme_ideal_gas_primitive_vars_to_conserved(gamma, rho, v, p, u)
      implicit none

      real(kind=8), intent(in) :: gamma, rho, v(NDIM), p
      real(kind=8), intent(out) :: u(cid%rho:cid%e_tot)

      u(cid%rho) = rho
      u(cid%rho_u:cid%rho_u + NDIM - 1) = rho*v
      u(cid%e_tot) = .5d0*rho*sum(v**2) + p/(gamma - 1.d0)
   end subroutine rhyme_ideal_gas_primitive_vars_to_conserved
end submodule primitive_vars_to_conserved_smod
