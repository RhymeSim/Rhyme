submodule(rhyme_chemistry) equality_smod
contains
pure module function rhyme_chemistry_equality(chem1, chem2) result(eq)
   implicit none

   type(chemistry_t), intent(in) :: chem1, chem2
   logical :: eq

   character(len=128) :: todo_holder

   eq = .false.

   write (todo_holder, *) 'TODO: implement ', chem1, ' == ', chem2
end function rhyme_chemistry_equality
end submodule equality_smod
