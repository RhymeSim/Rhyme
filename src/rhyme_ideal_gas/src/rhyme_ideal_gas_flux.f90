submodule(rhyme_ideal_gas) flux_smod
contains
   pure module subroutine rhyme_ideal_gas_flux(gamma, u, axis, f)
      implicit none

      real(kind=8), intent(in) :: gamma, u(cid%rho:cid%e_tot)
      integer, intent(in) :: axis
      real(kind=8), intent(out) :: f(cid%rho:cid%e_tot)

      real(kind=8) :: p

      if (abs(u(cid%rho)) < tiny(0.d0)) then
         f(cid%rho:cid%e_tot) = 0.d0
      else
         p = rhyme_ideal_gas_pressure(gamma, u)

         f(cid%rho) = u(cid%rho_u + axis - 1)

         f(cid%rho_u:cid%rho_u + NDIM - 1) = u(cid%rho_u:cid%rho_u + NDIM - 1)*f(cid%rho)/u(cid%rho)
         f(cid%rho_u + axis - 1) = f(cid%rho_u + axis - 1) + p

         f(cid%e_tot) = f(cid%rho)/u(cid%rho)*(u(cid%e_tot) + p)
      end if
   end subroutine rhyme_ideal_gas_flux
end submodule flux_smod
