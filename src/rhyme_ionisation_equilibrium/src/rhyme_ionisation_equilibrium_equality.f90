submodule(rhyme_ionisation_equilibrium) equality_smod
contains
pure module function rhyme_ionisation_equilibrium_equality(ie1, ie2) result(eq)
   implicit none

   type(ionisation_equilibrium_t), intent(in) :: ie1, ie2
   logical :: eq

   eq = .false.

   if (ie1%case == ie2%case &
       .and. (ie1%uvb .eqv. ie2%uvb) &
       .and. (ie1%collisional .eqv. ie2%collisional) &
       .and. (ie1%photo .eqv. ie2%photo) &
       ) eq = .true.
end function rhyme_ionisation_equilibrium_equality
end submodule equality_smod
