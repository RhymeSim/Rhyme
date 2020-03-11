submodule(rhyme_nombre_dimension) equality_smod
contains
pure module function rhyme_nombre_dimension_equality(d1, d2) result(cmp)
   implicit none

   type(nombre_dimension_t), intent(in) :: d1, d2
   logical :: cmp

   if (any(abs(d1%powers - d2%powers) > tiny(0d0))) then
      cmp = .false.
   else
      cmp = .true.
   end if
end function rhyme_nombre_dimension_equality
end submodule equality_smod
