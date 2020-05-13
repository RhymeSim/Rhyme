submodule(rhyme_chemistry) mu_smod
contains
module pure function rhyme_chemistry_mu(chem) result(mu)
   implicit none

   type(chemistry_t), intent(in) :: chem
   real(kind=4) :: mu

   mu = sum( &
          sum( &
            chem%element_abundances(:)/chem%elements(:)%atomic_weight &
            ) + &
          sum( &

            chem%species(:)%ionized*chem%species_abundances(:)/chem%species(:)%atomic_weight
          &) &
        )**(-1)
end function rhyme_chemistry_mu
end submodule mu_smod
