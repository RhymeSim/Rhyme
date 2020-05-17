submodule(rhyme_chemistry) ne_smod
contains

pure module function rhyme_chemistry_ne(chemistry, density, ntr_frac) result(ne)
   implicit none

   type(chemistry_t), intent(in) :: chemistry
   real(kind=8), intent(in) :: density, ntr_frac(:)

   real(kind=8) :: ne

   integer :: ei, i

   ne = 0d0

   i = 1
   do ei = 1, size(chemistry%elements)
      ne = ne + &
           chemistry%element_abundances(ei) &
           /chemistry%elements(ei)%atomic_weight &
           *chemistry%elements(ei)%ne(ntr_frac(i:))
      i = i + chemistry%elements(ei)%nspecies
   end do

   ne = density*chemistry%rho_to_number_density*ne
end function rhyme_chemistry_ne
end submodule ne_smod
