submodule(rhyme_ideal_gas) temperature_smod
contains
pure module function rhyme_ideal_gas_temperature(gamma, kb_amu, mu, u) result(t)
   implicit none

   real(kind=8), intent(in) :: gamma, kb_amu, mu, u(cid%rho:cid%e_tot)
   real(kind=8) :: t

   t = rhyme_ideal_gas_temperature_per_mu(gamma, kb_amu, u)*mu
end function rhyme_ideal_gas_temperature
end submodule temperature_smod
