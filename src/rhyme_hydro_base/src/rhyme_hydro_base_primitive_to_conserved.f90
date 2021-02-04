submodule(rhyme_hydro_base) primitive_to_conserved_smod
contains
   pure module subroutine rhyme_hydro_base_primitive_to_conserved(w, e_int, u)
      implicit none

      real(kind=8), intent(in) :: w(cid%rho:cid%p), e_int
      real(kind=8), intent(out) :: u(cid%rho:cid%e_tot)

      u(cid%rho) = w(cid%rho)
      u(cid%rho_u:cid%rho_u + NDIM - 1) = w(cid%rho)*w(cid%u:cid%u + NDIM - 1)
      u(cid%e_tot) = .5d0*w(cid%rho)*sum(w(cid%u:cid%u + NDIM - 1)**2) + e_int
   end subroutine rhyme_hydro_base_primitive_to_conserved
end submodule primitive_to_conserved_smod
