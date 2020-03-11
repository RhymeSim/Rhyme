submodule(rhyme_nombre_prefix) equality_smod
contains
pure module function rhyme_nombre_prefix_equality(p1, p2) result(eq)
   implicit none

   type(nombre_prefix_t), intent(in) :: p1, p2
   logical :: eq

   eq = .false.

   if (p1%base_10 .eq. p2%base_10 .and. p1%symb .eq. p2%symb) then
      eq = .true.
   end if
end function rhyme_nombre_prefix_equality
end submodule equality_smod
