submodule(rhyme_chemistry) one_over_mu_smod
contains
pure module function rhyme_chemistry_one_over_mu(chemistry, ntr_frac) result(one_over_mu)
   implicit none

   type(chemistry_t), intent(in) :: chemistry
   real(kind=8), intent(in) :: ntr_frac(NSPE)
   real(kind=8) :: one_over_mu

   integer :: i, ei, si

   i = 1
   one_over_mu = 0d0

   do ei = 1, size(chemistry%elements)
      one_over_mu = one_over_mu + 1d0*chemistry%element_abundances(ei)/chemistry%elements(ei)%atomic_weight

      do si = 1, chemistry%elements(ei)%nspecies
         one_over_mu = &
            one_over_mu &
            + chemistry%elements(ei)%species(si)%ionized &
            *chemistry%element_abundances(ei)/chemistry%elements(ei)%atomic_weight &
            *chemistry%elements(ei)%species(si)%f(ntr_frac(i:))
      end do

      i = i + chemistry%elements(ei)%nspecies
   end do
end function rhyme_chemistry_one_over_mu
end submodule one_over_mu_smod
