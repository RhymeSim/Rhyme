submodule(rhyme_ideal_gas) conserved_to_primitive_smod
contains
   pure module subroutine rhyme_ideal_gas_conserved_to_primitive(gamma, u, w)
      implicit none

      real(kind=8), intent(in) :: gamma, u(cid%rho:cid%e_tot)
      real(kind=8), intent(out) :: w(cid%rho:cid%p)

      w(cid%rho) = u(cid%rho)

      if (u(cid%rho) < tiny(0d0)) then
         w(cid%u:cid%u + NDIM - 1) = 0d0
      else
         w(cid%u:cid%u + NDIM - 1) = u(cid%u:cid%u + NDIM - 1)/u(cid%rho)
      end if

      w(cid%p) = (u(cid%e_tot) - rhyme_hydro_base_specific_kinetic_energy(u))*(gamma - 1d0)
   end subroutine rhyme_ideal_gas_conserved_to_primitive
end submodule conserved_to_primitive_smod
